module Elm.Syntax.Eval exposing (..)

import Dict exposing (Dict)
import Elm.Syntax.Exposing exposing (Exposing(..))
import Elm.Syntax.Expression exposing (Expression(..), FunctionImplementation, LetBlock, LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Graph
import Result.Extra2
import Set exposing (Set)


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
    | ElmLambda (Dict String ElmValue) (ElmValue -> Result Error ElmValue)


type ModuleName
    = ModuleName String


type ConstructorName
    = ConstructorName ModuleName String


type Error
    = CyclicDependencyError
    | PatternMatchError PatternMatchError


type PatternMatchError
    = CharMatchError Char Char
    | StringMatchError String String
    | IntMatchError Int Int
    | FloatMatchError Float Float
    | ConstructorMatchError ConstructorName ConstructorName


evalLetBlock : LetBlock -> Dict String ElmValue -> Result Error ElmValue
evalLetBlock letBlock originalBindings =
    case orderLetDeclarations (List.map Node.value letBlock.declarations) of
        Err e ->
            Err e

        Ok orderedDeclarations ->
            let
                newBindingsResult =
                    Result.Extra2.combineFold
                        (\declaration bindings ->
                            case declaration of
                                LetFunction function ->
                                    evalFunction (Node.value function.declaration) bindings

                                LetDestructuring patternNode expressionNode ->
                                    evalExpression bindings expressionNode
                                        |> Result.andThen (bindDestructuring caseConstantMatching patternNode)
                        )
                        originalBindings
                        orderedDeclarations
            in
            newBindingsResult
                |> Result.andThen (\newBindings -> evalExpression newBindings letBlock.expression)


evalExpression : Dict String ElmValue -> Node Expression -> Result Error ElmValue
evalExpression bindings (Node _ expression) =
    case expression of 
        UnitExpr ->
            Ok ElmUnit

        _ ->
            Debug.todo ("Unimplemented case" ++ Debug.toString expression)


type alias ConstantMatching =
    { matchChar : Char -> Char -> Result Error ElmValue
    , matchString : String -> String -> Result Error ElmValue
    , matchInt : Int -> Int -> Result Error ElmValue
    , matchFloat : Float -> Float -> Result Error ElmValue
    }


caseConstantMatching : ConstantMatching
caseConstantMatching =
    let
        match makeValue makeError =
            \left right ->
                if left == right then
                    Ok <| makeValue left

                else
                    Err <| PatternMatchError (makeError left right)
    in
    { matchChar = match ElmChar CharMatchError
    , matchString = match ElmString StringMatchError
    , matchInt = match ElmInt IntMatchError
    , matchFloat = match ElmFloat FloatMatchError
    }


bindDestructuring : ConstantMatching -> Node Pattern -> ElmValue -> Result Error (Dict String ElmValue)
bindDestructuring constantMatching patternNode value =
    case ( Node.value patternNode, value ) of
        _ ->
            Debug.todo ""


evalFunction : FunctionImplementation -> Dict String ElmValue -> Result Error (Dict String ElmValue)
evalFunction functionImplementation bindings =
    Debug.todo ""


orderLetDeclarations : List LetDeclaration -> Result Error (List LetDeclaration)
orderLetDeclarations declarations =
    let
        ( bindingInfos, nameToIndex ) =
            declarations
                |> List.indexedMap
                    (\index declaration ->
                        case declaration of
                            LetFunction function ->
                                let
                                    implementation =
                                        Node.value function.declaration

                                    bindingInfo =
                                        { nodeId = index
                                        , boundNames = expressionBoundNames implementation.expression
                                        , declaration = declaration
                                        }
                                in
                                [ ( bindingInfo, ( Node.value implementation.name, index ) ) ]

                            LetDestructuring pattern expression ->
                                let
                                    patternNames =
                                        patternBindingNames pattern

                                    bindingInfo =
                                        { nodeId = index
                                        , boundNames = expressionBoundNames expression
                                        , declaration = declaration
                                        }
                                in
                                List.map (\name -> ( bindingInfo, ( name, index ) )) patternNames
                    )
                |> List.concat
                |> List.unzip
                |> Tuple.mapSecond Dict.fromList

        nodes =
            List.map (\bindingInfo -> { id = bindingInfo.nodeId, label = bindingInfo }) bindingInfos

        edgePairs =
            List.concatMap
                (\bindingInfo ->
                    List.filterMap
                        (\name ->
                            Dict.get name nameToIndex
                                |> Maybe.map (\referenceIndex -> ( referenceIndex, bindingInfo.nodeId ))
                        )
                        bindingInfo.boundNames
                )
                bindingInfos
    in
    Graph.fromNodeLabelsAndEdgePairs nodes edgePairs
        |> Graph.checkAcyclic
        |> Result.mapError (always CyclicDependencyError)
        |> Result.map (Graph.topologicalSort >> List.map (.node >> .label >> .label >> .declaration))


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
