module ElmSyntaxFormat exposing
    ( DeadEnd
    , formatCode
    )

import Elm.Parser
import ElmSyntaxPrint
import Parser


type alias DeadEnd =
    Parser.DeadEnd


formatCode : String -> Result (List DeadEnd) String
formatCode input =
    moduleHeader
        ++ input
        |> Elm.Parser.parseToFile
        |> Result.map
            (\file ->
                file
                    |> ElmSyntaxPrint.module_
                    |> ElmSyntaxPrint.toString
                    |> String.dropLeft (String.length moduleHeader)
                    |> String.trimRight
            )


moduleHeader : String
moduleHeader =
    "module Main exposing (..)\n\n"
