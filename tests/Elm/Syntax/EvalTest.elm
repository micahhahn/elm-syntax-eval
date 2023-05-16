module Elm.Syntax.EvalTest exposing (suite)

import Dict
import Elm.Parser exposing (parse)
import Elm.Processing
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Eval exposing (ElmValue(..), evalExpression)
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Expect
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
        , test "Identity lambda expression" <|
            \_ ->
                """
    let
        f = \\x -> x
    in f ()
                """
                    |> makeExpression
                    |> evalExpression Dict.empty
                    |> Expect.equal (Ok ElmUnit)
        , test "Multiple argument lambda expression" <|
            \_ ->
                """
    let
        f = \\x y -> x + y
    in f 1 2
                """
                    |> makeExpression
                    |> evalExpression Dict.empty
                    |> Expect.equal (Ok (ElmInt 3))
        , test "Simple case expression" <|
            \_ ->
                """
    case 2 of 
        1 ->
            1

        _ ->
            2
                """
                    |> makeExpression
                    |> evalExpression Dict.empty
                    |> Expect.equal (Ok (ElmInt 2))
        , test "Simple tuple construction" <|
            \_ ->
                """
    (1, 2)
                """
                    |> makeExpression
                    |> evalExpression Dict.empty
                    |> Expect.equal (Ok (ElmTuple [ ElmInt 1, ElmInt 2 ]))
        , test "Simple record construction (1)" <|
            \_ ->
                """
    { a = 1 }
                """
                    |> makeExpression
                    |> evalExpression Dict.empty
                    |> Expect.equal (Ok (ElmRecord (Dict.fromList [ ( "a", ElmInt 1 ) ])))
        , test "Simple record construction (2)" <|
            \_ ->
                """
    { a = 1, b = 2}
                """
                    |> makeExpression
                    |> evalExpression Dict.empty
                    |> Expect.equal (Ok (ElmRecord (Dict.fromList [ ( "a", ElmInt 1 ), ( "b", ElmInt 2 ) ])))
        ]
