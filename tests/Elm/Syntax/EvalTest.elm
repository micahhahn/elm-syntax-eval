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
        x = ()
    in x
                """
                |> makeExpression
                |> evalExpression Dict.empty
                |> Expect.equal (Ok ElmUnit)
        , test "Let expression with dependency" <|
            \_ ->
                """
    let 
        x = 1
        y = x + 1
    in y
                """
                |> makeExpression
                |> evalExpression Dict.empty
                |> Expect.equal (Ok (ElmInt 2))
        , test "Let expression with out of order dependency" <|
            \_ ->
                """
    let 
        y = x + 1
        x = 1
    in y
                """
                |> makeExpression
                |> evalExpression Dict.empty
                |> Expect.equal (Ok (ElmInt 2))
        , test "Simple let with lambda expression" <|
            \_ ->
                """
    let
        x = \\_ -> ()
    in x ()
                """
                |> makeExpression
                |> evalExpression Dict.empty
                |> Expect.equal (Ok ElmUnit)
        ]
