module Elm.Syntax.Eval exposing (..)

import Dict exposing (Dict)
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import List exposing (tail)


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
