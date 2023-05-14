module Elm.Syntax.EvalTest exposing (suite)

import Elm.Parser exposing (parse)
import Elm.Processing
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Eval exposing (evalExpression, ElmValue(..))
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Expect
import Dict
import Test exposing (..)


makeExpression : String -> Node Expression
makeExpression elmExpression =
    let
        expressionName =
            "loremipsum"

        elmFile =
            "module Test exposing (..)\n\n"
                ++ expressionName
                ++ " =\n    "
                ++ String.trim elmExpression

        file =
            case parse elmFile of
                Err err ->
                    Debug.todo ("Failed to parse file with: " ++ Debug.toString err)

                Ok rawFile ->
                    Elm.Processing.process Elm.Processing.init rawFile
    in
    case file.declarations of
        [ Node _ declaration ] ->
            case declaration of
                FunctionDeclaration functionImplementation ->
                    functionImplementation
                        |> .declaration
                        |> Node.value
                        |> .expression

                _ ->
                    Debug.todo "Expected an expression node"

        _ ->
            Debug.todo "makeExpression should only contain an expresion"

suite : Test
suite =
    describe "Elm.Syntax.Eval"
        [ test "Unit Expression" <|
            \_ ->
                """
    ()
                """
                    |> makeExpression
                    |> evalExpression Dict.empty
                    |> Expect.equal (Ok ElmUnit)
        , test "Simple let expression" <|
            \_ ->
                """
    let 
        x = 
            ()
    in x
                """
                |> makeExpression
                |> evalExpression Dict.empty
                |> Expect.equal (Ok ElmUnit)
        ]
