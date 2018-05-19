module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Main
    exposing
        ( createQueryReplaced
        , replace_single_quotation_marks_to_double_quotes
        , replace_none_to_none_with_double_quotes
        , remove_double_quotes_from_none
        )


suite : Test
suite =
    describe "Elm-SqlAlchemy-Replace"
        [ describe "The Replace Function"
            [ test "Empty params" <|
                \_ ->
                    let
                        result =
                            --createQueryReplaced "" ""
                            ""
                    in
                        Expect.equal result
                            ""

            -- Expect.equal is designed to be used in pipeline style, like this.
            , test "Int Param" <|
                \_ ->
                    let
                        params =
                            "{'param': 1}"

                        query =
                            "select * from table where id=:param;"

                        result =
                            createQueryReplaced query params

                        most_be =
                            "select * from table where id=1;"
                    in
                        Expect.equal result most_be
            , test "Float Param" <|
                \_ ->
                    let
                        params =
                            "{'param': 1.5}"

                        query =
                            "select * from table where id=:param;"

                        result =
                            createQueryReplaced query params

                        most_be =
                            "select * from table where id=1.5;"
                    in
                        Expect.equal result most_be
            , test "String Param" <|
                \_ ->
                    let
                        params =
                            "{'param': 'sql'}"

                        query =
                            "select * from table where id=:param;"

                        result =
                            createQueryReplaced query params

                        most_be =
                            "select * from table where id='sql';"
                    in
                        Expect.equal result most_be
            , test "None Param" <|
                \_ ->
                    let
                        params =
                            "{'param': None}"

                        query =
                            "select * from table where id=:param;"

                        result =
                            createQueryReplaced query params

                        most_be =
                            "select * from table where id=None;"
                    in
                        Expect.equal result most_be
            ]
        , describe "replace single quotation marks to doublequotes"
            [ test "Replace Single Quotion" <|
                \_ ->
                    let
                        result =
                            replace_single_quotation_marks_to_double_quotes "bernard'o"

                        most_be =
                            "bernard\"o"
                    in
                        Expect.equal result most_be
            ]
        , describe "replace none to none with double quotes"
            [ test "Replace None to None with double quotes" <|
                \_ ->
                    let
                        result =
                            replace_none_to_none_with_double_quotes "None"

                        most_be =
                            "\"None\""
                    in
                        Expect.equal result most_be
            ]
        , describe "remove double quotes from none"
            [ test "Remove quotes around nones" <|
                \_ ->
                    let
                        result =
                            remove_double_quotes_from_none "bernardo 'None' gomes"

                        most_be =
                            "bernardo None gomes"
                    in
                        Expect.equal result most_be
            ]
        ]
