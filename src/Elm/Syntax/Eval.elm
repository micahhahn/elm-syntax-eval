module Elm.Syntax.Eval exposing (..)

import Dict exposing (Dict)
import Elm.Syntax.Exposing exposing (Exposing(..))
import Elm.Syntax.Expression exposing (CaseBlock, Expression(..), FunctionImplementation, LetBlock, LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..), QualifiedNameRef)
import Elm.Syntax.TypeAnnotation exposing (RecordField)
import Fuzz exposing (tuple)
import Graph
import List.Extra
import Result
import Result.Extra
import Result.Extra2


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


type ElmType
    = ElmUnitT
    | ElmIntT
    | ElmFloatT
    | ElmStringT
    | ElmCharT
    | ElmListT ElmType
    | ElmTupleT (List ElmType)
    | ElmRecordT (Dict String ElmType)
    | ElmCustomT TypeName


type ModuleName
    = ModuleName String


type ConstructorName
    = ConstructorName ModuleName String


type TypeName
    = TypeName ModuleName String


qualifiedNameToConstructorName : QualifiedNameRef -> ConstructorName
qualifiedNameToConstructorName { moduleName, name } =
    ConstructorName (ModuleName (String.join "." moduleName)) name


type Error
    = CyclicDependencyError
    | PatternMatchError PatternMatchError
    | TypeError TypeError
    | MissingBinding String
    | CaseNonExhaustive
    | TrueTypeMismatch


type PatternMatchError
    = CharMatchError Char Char
    | StringMatchError String String
    | IntMatchError Int Int
    | FloatMatchError Float Float
    | ConstructorMatchError ConstructorName ConstructorName


type TypeError
    = MissingPattern
    | MissingValue
    | MissingRecordField String
    | TypeMismatch Pattern ElmValue


type alias Bindings =
    Dict String ElmValue


consConstructorName : ConstructorName
consConstructorName =
    ConstructorName (ModuleName "List") "::"


emptyListConstructorName : ConstructorName
emptyListConstructorName =
    ConstructorName (ModuleName "List") "[]"


evalLetBlock : Dict String ElmValue -> LetBlock -> Result Error ElmValue
evalLetBlock originalBindings letBlock =
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
                                    evalFunction bindings (Node.value function.declaration)

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

        Application nodes ->
            case nodes of
                [] ->
                    Debug.todo "What do we do here?"

                funcNode :: argNodes ->
                    evalExpression bindings funcNode
                        |> Result.andThen (\funcValue -> evalApplication bindings funcValue argNodes)

        OperatorApplication operator _ leftExpression rightExpression ->
            case ( evalExpression bindings leftExpression, evalExpression bindings rightExpression ) of
                ( Err err, _ ) ->
                    Err err

                ( _, Err err ) ->
                    Err err

                ( Ok leftValue, Ok rightValue ) ->
                    case ( operator, leftValue, rightValue ) of
                        ( "+", ElmInt leftInt, ElmInt rightInt ) ->
                            Ok <| ElmInt (leftInt + rightInt)

                        ( "+", ElmFloat leftFloat, ElmFloat rightFloat ) ->
                            Ok <| ElmFloat (leftFloat + rightFloat)

                        _ ->
                            Debug.todo ""

        FunctionOrValue moduleName name ->
            case moduleName of
                [] ->
                    case Dict.get name bindings of
                        Nothing ->
                            Err <| MissingBinding name

                        Just value ->
                            Ok value

                _ ->
                    Debug.todo ""

        Integer int ->
            Ok <| ElmInt int

        Hex int ->
            Ok <| ElmInt int

        Floatable float ->
            Ok <| ElmFloat float

        Negation negNode ->
            evalExpression bindings negNode
                |> Result.andThen
                    (\value ->
                        case value of
                            ElmInt i ->
                                Ok <| ElmInt -i

                            ElmFloat i ->
                                Ok <| ElmFloat -i

                            _ ->
                                Debug.todo ""
                    )

        Literal string ->
            Ok <| ElmString string

        CharLiteral char ->
            Ok <| ElmChar char

        TupledExpression tupleNodes ->
            Result.Extra.combineMap (evalExpression bindings) tupleNodes
                |> Result.map ElmTuple

        LetExpression letBlock ->
            evalLetBlock bindings letBlock

        CaseExpression caseBlock ->
            evalCaseBlock bindings caseBlock

        LambdaExpression lambda ->
            bindLambda bindings lambda.args lambda.expression

        RecordExpr recordSetters ->
            recordSetters
                |> Result.Extra.combineMap
                    (\(Node _ ( Node _ name, expressionNode )) ->
                        evalExpression bindings expressionNode
                            |> Result.map (Tuple.pair name)
                    )
                |> Result.map (Dict.fromList >> ElmRecord)

        ListExpr listNodes ->
            Result.Extra.combineMap (evalExpression bindings) listNodes
                |> Result.map ElmList

        RecordAccess recordNode (Node _ fieldNameNode) ->
            evalExpression bindings recordNode
                |> Result.andThen
                    (withRecord
                        (\recordDict ->
                            case Dict.get fieldNameNode recordDict of
                                Nothing ->
                                    Err <| TypeError (MissingRecordField fieldNameNode)

                                Just value ->
                                    Ok value
                        )
                    )

        _ ->
            Debug.todo ("Unimplemented case" ++ Debug.toString expression)


withRecord : (Dict String ElmValue -> Result Error ElmValue) -> ElmValue -> Result Error ElmValue
withRecord recordFunc value =
    case value of
        ElmRecord recordDict ->
            recordFunc recordDict

        _ ->
            Err TrueTypeMismatch


bindLambda : Dict String ElmValue -> List (Node Pattern) -> Node Expression -> Result Error ElmValue
bindLambda bindings argPatterns expressionNode =
    case argPatterns of
        [] ->
            evalExpression bindings expressionNode

        argPattern :: otherArgPatterns ->
            Ok <|
                ElmLambda
                    (\value ->
                        bindDestructuring caseConstantMatching argPattern value
                            |> Result.andThen
                                (\argBindings ->
                                    bindLambda (Dict.union argBindings bindings) otherArgPatterns expressionNode
                                )
                    )


evalApplication : Dict String ElmValue -> ElmValue -> List (Node Expression) -> Result Error ElmValue
evalApplication bindings value argNodes =
    case argNodes of
        [] ->
            Ok value

        argNode :: otherArgs ->
            case value of
                ElmLambda func ->
                    evalExpression bindings argNode
                        |> Result.andThen func
                        |> Result.andThen (\newValue -> evalApplication bindings newValue otherArgs)

                _ ->
                    Debug.todo "Expected lambda"


evalCaseBlock : Bindings -> CaseBlock -> Result Error ElmValue
evalCaseBlock bindings { expression, cases } =
    evalExpression bindings expression
        |> Result.andThen
            (\caseValue ->
                cases
                    |> List.Extra.findMap
                        (\( casePattern, caseExpression ) ->
                            case bindDestructuring caseConstantMatching casePattern caseValue of
                                Ok newBindings ->
                                    -- The case statement matched!
                                    Just <|
                                        evalExpression (Dict.union newBindings bindings) caseExpression

                                Err (PatternMatchError _) ->
                                    -- If we have a pattern match error just try the next one
                                    Nothing

                                Err err ->
                                    -- Any other error is a type error and needs to be floated up
                                    Just (Err err)
                        )
                    |> Maybe.withDefault (Err CaseNonExhaustive)
            )


type alias ConstantMatching x =
    { matchChar : x -> Char -> Char -> Result Error x
    , matchString : x -> String -> String -> Result Error x
    , matchInt : x -> Int -> Int -> Result Error x
    , matchFloat : x -> Float -> Float -> Result Error x
    }


{-| Value match errors produce errors
-}
caseConstantMatching : ConstantMatching x
caseConstantMatching =
    let
        match makeError =
            \bindings left right ->
                if left == right then
                    Ok <| bindings

                else
                    Err <| PatternMatchError (makeError left right)
    in
    { matchChar = match CharMatchError
    , matchString = match StringMatchError
    , matchInt = match IntMatchError
    , matchFloat = match FloatMatchError
    }


{-| Value match errors are ignored
-}
letConstanctMatching : ConstantMatching x
letConstanctMatching =
    { matchChar = \accum _ _ -> Ok accum
    , matchString = \accum _ _ -> Ok accum
    , matchInt = \accum _ _ -> Ok accum
    , matchFloat = \accum _ _ -> Ok accum
    }


bindDestructuring : ConstantMatching (List ( String, ElmValue )) -> Node Pattern -> ElmValue -> Result Error (Dict String ElmValue)
bindDestructuring constantMatching patternNode originalValue =
    let
        bindSequence : List ( String, ElmValue ) -> List (Node Pattern) -> List ElmValue -> Int -> Result ( Int, Error ) (List ( String, ElmValue ))
        bindSequence accum patterns values seqIndex =
            case ( patterns, values ) of
                ( pattern :: otherPatterns, value :: otherValues ) ->
                    case bind pattern value accum of
                        Err e ->
                            Err ( seqIndex, e )

                        Ok result ->
                            bindSequence result otherPatterns otherValues (seqIndex + 1)

                ( [], [] ) ->
                    Ok accum

                ( [], _ ) ->
                    Err <| ( seqIndex, TypeError MissingPattern )

                ( _, [] ) ->
                    Err <| ( seqIndex, TypeError MissingValue )

        bind : Node Pattern -> ElmValue -> List ( String, ElmValue ) -> Result Error (List ( String, ElmValue ))
        bind (Node _ pattern) value accum =
            case ( pattern, value ) of
                ( AllPattern, _ ) ->
                    Ok accum

                ( UnitPattern, ElmUnit ) ->
                    Ok accum

                ( CharPattern patternChar, ElmChar valueChar ) ->
                    constantMatching.matchChar accum patternChar valueChar

                ( StringPattern patternString, ElmString valueString ) ->
                    constantMatching.matchString accum patternString valueString

                ( IntPattern patternInt, ElmInt valueInt ) ->
                    constantMatching.matchInt accum patternInt valueInt

                ( HexPattern patternInt, ElmInt valueInt ) ->
                    constantMatching.matchInt accum patternInt valueInt

                ( FloatPattern patternFloat, ElmFloat valueFloat ) ->
                    constantMatching.matchFloat accum patternFloat valueFloat

                ( TuplePattern tupleNodes, ElmTuple tupleValues ) ->
                    bindSequence accum tupleNodes tupleValues 0
                        |> Result.mapError (Tuple.second {- Fixme? Record structured errors? -})

                ( RecordPattern nameNodes, ElmRecord recordDict ) ->
                    nameNodes
                        |> Result.Extra.combineMap
                            (\(Node _ name) ->
                                case Dict.get name recordDict of
                                    Nothing ->
                                        Err <| TypeError (MissingRecordField name)

                                    Just v ->
                                        Ok <| ( name, v )
                            )
                        |> Result.map (\pairs -> pairs ++ accum)

                ( UnConsPattern headPattern tailPattern, ElmList listValues ) ->
                    case listValues of
                        headValue :: tailValue ->
                            bind headPattern headValue accum
                                |> Result.andThen
                                    (bind tailPattern (ElmList tailValue))

                        _ ->
                            Err <| PatternMatchError (ConstructorMatchError consConstructorName emptyListConstructorName)

                ( ListPattern listNodes, ElmList listValues ) ->
                    bindSequence accum listNodes listValues 0
                        |> Result.mapError (Tuple.second {- Fixme? -})

                ( VarPattern name, _ ) ->
                    Ok <| ( name, value ) :: accum

                ( NamedPattern qualifiedNameRef constructorPatterns, ElmConstructor constructorName constructorValues ) ->
                    if qualifiedNameToConstructorName qualifiedNameRef == constructorName then
                        bindSequence accum constructorPatterns constructorValues 0
                            |> Result.mapError (Tuple.second {- Fixme? -})

                    else
                        Err <| PatternMatchError (ConstructorMatchError (qualifiedNameToConstructorName qualifiedNameRef) constructorName)

                ( AsPattern innerPatternNode (Node _ name), _ ) ->
                    bind innerPatternNode value (( name, value ) :: accum)

                ( ParenthesizedPattern innerPatternNode, _ ) ->
                    bind innerPatternNode value accum

                _ ->
                    Err <| TypeError (TypeMismatch pattern value)
    in
    bind patternNode originalValue []
        |> Result.map Dict.fromList


evalFunction : Dict String ElmValue -> FunctionImplementation -> Result Error (Dict String ElmValue)
evalFunction bindings { name, arguments, expression } =
    -- elm-syntax got this one wrong.  x = 7 should be a destructuring instead of a function, but here we are.
    case arguments of
        [] ->
            evalExpression bindings expression
                |> Result.map
                    (\value ->
                        Dict.insert
                            (Node.value name)
                            value
                            bindings
                    )

        _ ->
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
