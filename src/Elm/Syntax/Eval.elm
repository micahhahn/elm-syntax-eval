module Elm.Syntax.Eval exposing (..)

import Dict exposing (Dict)
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))


type ElmValue
    = ElmUnit
    | ElmInt Int
    | ElmFloat Float
    | ElmString String
    | ElmChar Char
    | ElmList (List ElmValue)
    | ElmTuple (List ElmValue)
    | ElmRecord (Dict String ElmValue)
    | ElmConstructor ConstructorName (List ElmValue)
    | ElmLambda (ElmValue -> Result Error ElmValue)


type ModuleName
    = ModuleName String


type ConstructorName
    = ConstructorName ModuleName String


type Error
    = Error


patternBindingNames : Node Pattern -> List String
patternBindingNames patternNode =
    let
        go todoNodes accum =
            case todoNodes of
                [] ->
                    accum

                (Node _ pattern) :: otherNodes ->
                    case pattern of
                        TuplePattern tupleNodes ->
                            go (tupleNodes ++ otherNodes) accum

                        RecordPattern fieldNodes ->
                            go otherNodes (List.foldl (Node.value >> (::)) accum fieldNodes)

                        UnConsPattern headNode tailNode ->
                            go (headNode :: tailNode :: otherNodes) accum

                        ListPattern listNodes ->
                            go (listNodes ++ otherNodes) accum

                        VarPattern name ->
                            go otherNodes (name :: accum)

                        NamedPattern _ conNodes ->
                            go (conNodes ++ otherNodes) accum

                        AsPattern innerNode (Node _ name) ->
                            go (innerNode :: otherNodes) (name :: accum)

                        ParenthesizedPattern innerNode ->
                            go (innerNode :: otherNodes) accum

                        _ ->
                            go otherNodes accum
    in
    go [ patternNode ] []


expressionBoundNames : Node Expression -> List String
expressionBoundNames expressionNode =
    let
        go : List (Node Expression) -> List String -> List String
        go todoNodes accum =
            case todoNodes of
                [] ->
                    accum

                (Node _ expression) :: otherNodes ->
                    case expression of
                        Application nodes ->
                            go (nodes ++ otherNodes) accum

                        OperatorApplication _ _ leftNode rightNode ->
                            go (leftNode :: rightNode :: otherNodes) accum

                        FunctionOrValue moduleName varName ->
                            case moduleName of
                                [] ->
                                    go otherNodes (varName :: accum)

                                _ ->
                                    go otherNodes accum

                        IfBlock condNode trueNode falseNode ->
                            go (condNode :: trueNode :: falseNode :: otherNodes) accum

                        Negation negNode ->
                            go (negNode :: otherNodes) accum

                        TupledExpression tupleNodes ->
                            go (tupleNodes ++ otherNodes) accum

                        ParenthesizedExpression parenNode ->
                            go (parenNode :: otherNodes) accum

                        LetExpression letBlock ->
                            let
                                declarationExpressions =
                                    letBlock.declarations
                                        |> List.map
                                            (\(Node _ declaration) ->
                                                case declaration of
                                                    LetFunction function ->
                                                        function.declaration
                                                            |> Node.value
                                                            |> .expression

                                                    LetDestructuring _ letExpression ->
                                                        letExpression
                                            )
                            in
                            go (declarationExpressions ++ letBlock.expression :: otherNodes) accum

                        CaseExpression caseBlock ->
                            let
                                caseBranchExpressions =
                                    List.map Tuple.second caseBlock.cases
                            in
                            go (caseBranchExpressions ++ caseBlock.expression :: otherNodes) accum

                        LambdaExpression lambda ->
                            go (lambda.expression :: otherNodes) accum

                        RecordExpr recordSetterNodes ->
                            let
                                recordSetExpressions =
                                    List.map (Node.value >> Tuple.second) recordSetterNodes
                            in
                            go (recordSetExpressions ++ otherNodes) accum

                        ListExpr listNodes ->
                            go (listNodes ++ otherNodes) accum

                        RecordAccess recordNode _ ->
                            go (recordNode :: otherNodes) accum

                        RecordUpdateExpression _ recordSetterNodes ->
                            let
                                recordSetExpressions =
                                    List.map (Node.value >> Tuple.second) recordSetterNodes
                            in
                            go (recordSetExpressions ++ otherNodes) accum

                        _ ->
                            go otherNodes accum
    in
    go [ expressionNode ] []
