{-# LANGUAGE OverloadedStrings #-}
module Wasm.Reporting (
    noMain
  )
  where

import qualified Reporting.Doc as Doc
import Reporting.Doc ((<>))
import qualified Reporting.Exit.Help

noMain :: Reporting.Exit.Help.Report
noMain =
  -- from ../worker/src/Endpoint/Compile.hs
  Reporting.Exit.Help.report "NO MAIN" Nothing
    (
      "Without a `main` value, I do not know what to show on screen!"
    )
    [ Doc.reflow $
        "Adding a `main` value can be as brief as:"
    , Doc.vcat
        [ Doc.fillSep [Doc.cyan "import","Html"]
        , ""
        , Doc.fillSep [Doc.green "main","="]
        , Doc.indent 2 $ Doc.fillSep [Doc.cyan "Html" <> ".text", Doc.dullyellow "\"Hello!\""]
        ]
    , Doc.reflow $
        "Try adding something like that!"
    , Doc.toSimpleNote $
        "I recommend looking through https://guide.elm-lang.org for more advice on\
        \ how to fill in `main` values."
    ]
