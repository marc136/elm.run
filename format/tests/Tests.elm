module Tests exposing (..)

import ElmSyntaxFormat
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
                checkFormatter (String.fromInt index) input (Ok expected)
            )
        |> Test.describe "Compare elm formatting"


checkFormatter name input expected =
    Test.test name <|
        \() ->
            ElmSyntaxFormat.formatCode input
                |> Expect.equal expected
