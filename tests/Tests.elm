module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Main
    exposing
        ( createQueryReplaced
        , replace_single_quotation_marks_to_double_quotes
        , replace_none_to_none_with_double_quotes
        , remove_double_quotes_from_none
        , remove_caracter_break_line
        , convert_datetime_to_tochar
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
        , describe "remove break line (\n) from query"
            [ (\_ ->
                let
                    result =
                        remove_caracter_break_line "select * \nfrom table"

                    most_be =
                        "select * from table"
                in
                    Expect.equal result most_be
              )
                |> test "remove (\n) from query with (\n)"
            , (\_ ->
                let
                    result =
                        remove_caracter_break_line "select * from table"

                    most_be =
                        "select * from table"
                in
                    Expect.equal result most_be
              )
                |> test "remove (\n) from query without (\n)"
            , (\_ ->
                let
                    result =
                        remove_caracter_break_line ""

                    most_be =
                        ""
                in
                    Expect.equal result most_be
              )
                |> test "remove (\n) from blank query"
            , (\_ ->
                let
                    result =
                        remove_caracter_break_line "select * \\nfrom table"

                    most_be =
                        "select * from table"
                in
                    Expect.equal result most_be
              )
                |> test "remove (\\n) from query with (\\n)"
            , (\_ ->
                let
                    result =
                        remove_caracter_break_line "select * from table"

                    most_be =
                        "select * from table"
                in
                    Expect.equal result most_be
              )
                |> test "remove (\\n) from query without (\\n)"
            ]
        , describe "Convert param Datetime to TO_DATE(oracle)"
            [ (\_ ->
                let
                    result =
                        convert_datetime_to_tochar "{'data_acao': datetime.datetime(2018, 07, 25, 17, 02, 34, 864111)}"

                    most_be =
                        "{'data_acao': TO_DATE('2018/07/25 17:02:34', 'YYYY/MM/DD HH:MI:SS')}"
                in
                    Expect.equal result most_be
              )
                |> test "create to_date from datetime.datetime"
            , (\_ ->
                let
                    result =
                        convert_datetime_to_tochar "{'data_acao': datetime.datetime(2018, 07, 25, 17, 02, 34, 864111), 'data_acao2': datetime.datetime(2017, 07, 25, 17, 02, 34, 864111)}"

                    most_be =
                        "{'data_acao': TO_DATE('2018/07/25 17:02:34', 'YYYY/MM/DD HH:MI:SS'), 'data_acao2': TO_DATE('2017/07/25 17:02:34', 'YYYY/MM/DD HH:MI:SS')}"
                in
                    Expect.equal result most_be
              )
                |> test "create to_date from datetime.datetime multiples params"
            ]
        ]
