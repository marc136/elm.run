module ElmFormat exposing (..)

import Elm.DSLParser
import Elm.Parser
import Elm.Pretty
import Expect
import Test exposing (Test)


tests : Test
tests =
    [ { input = """abc =
  -- horst

  let 
    _ = 
      -- needed?
      Debug.log "moin" 1
  in
  13
    """
      , expected = """abc =
    -- horst
    let
        _ =
            -- needed?
            Debug.log "moin" 1
    in
    13"""
      }
    ]
        |> List.indexedMap
            (\index { input, expected } ->
                Test.test (String.fromInt index) <|
                    \() ->
                        elmFormat input
                            |> Expect.equal expected
            )
        |> Test.describe "Compare elm formatting"
        |> Test.skip


elmFormat : String -> String
elmFormat unformatted =
    let
        fake =
            "module Main exposing (..)\n" ++ unformatted

        rawfile =
            Elm.Parser.parse fake

        _ =
            Debug.log "parsed" <| rawfile

        -- file = Elm.Parser.parseToFile fake
        file =
            Elm.DSLParser.parse fake

        _ =
            Debug.log "parsed2" file

        _ =
            file
                |> Result.map (Elm.Pretty.pretty 80)
                |> Debug.log "pretty?"

        moduleDef =
            "module Main exposing (..)\n"
    in
    -- TODO use `stil4m/elm-syntax` and `the-sett/elm-syntax-dsl` to parse and format the code
    -- TODO ask `stil4m/elm-syntax` to expose not only the file parser, but also custom ones (or vendor/fork it)
    -- problem right now is that it removes comments
    case
        moduleDef
            ++ unformatted
            |> Elm.DSLParser.parse
    of
        Err _ ->
            unformatted

        Ok parsed ->
            Elm.Pretty.pretty 80 parsed
                |> String.dropLeft (String.length moduleDef)
                |> String.trim
