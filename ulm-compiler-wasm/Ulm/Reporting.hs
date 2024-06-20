{-# LANGUAGE OverloadedStrings #-}

module Ulm.Reporting
  ( noMain,
  )
where

import Reporting.Doc ((<>))
import Reporting.Doc qualified as Doc
import Reporting.Exit.Help qualified

noMain :: Reporting.Exit.Help.Report
noMain =
  -- from ../worker/src/Endpoint/Compile.hs
  Reporting.Exit.Help.report
    "NO MAIN"
    Nothing
    ( "Without a `main` value, I do not know what to show on screen!"
    )
    [ Doc.reflow $
        "Adding a `main` value can be as brief as:",
      Doc.vcat
        [ Doc.fillSep [Doc.cyan "import", "Html"],
          "",
          Doc.fillSep [Doc.green "main", "="],
          Doc.indent 2 $ Doc.fillSep [Doc.cyan "Html" <> ".text", Doc.dullyellow "\"Hello!\""]
        ],
      Doc.reflow $
        "Try adding something like that!",
      Doc.toSimpleNote $
        "I recommend looking through https://guide.elm-lang.org for more advice on\
        \ how to fill in `main` values."
    ]
