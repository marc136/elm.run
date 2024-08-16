module Tests exposing (..)

import AnsiExtra
import Elm.Error
import Expect
import Test exposing (Test)


suite : Test
suite =
    [ ( "\u{001B}[36m<function>\u{001B}[0mString -> Int"
      , [ Elm.Error.Styled { bold = False, color = Just Elm.Error.Cyan, underline = False } "<function>"
        , Elm.Error.Unstyled "String -> Int"
        ]
      )
    ]
        |> List.map
            (\( input, expected ) ->
                Test.test ("Should parse `" ++ input ++ "`") <|
                    \() ->
                        AnsiExtra.parse input
                            |> Expect.equal expected
            )
        |> Test.describe "Parse ANSI sequences"
