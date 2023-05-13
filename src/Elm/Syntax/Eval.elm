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

eval = Debug.todo ""