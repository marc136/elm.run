module Ulm where

import qualified AST.Canonical as Can
import AST.Optimized as Opt
import qualified AST.Source as Src
import qualified Compile
import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.UTF8 -- from utf8-string
import qualified Data.ByteString.UTF8 as BSU -- from utf8-string
import qualified Data.Map as Map
import qualified Data.Name
import Debug.Trace
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified File
import qualified Generate.Mode as Mode
import qualified Generate.JavaScript as JS
import qualified Json.Encode
import Json.Encode ((==>))
import qualified GHC.Wasm.Prim as Wasm -- See https://gitlab.haskell.org/ghc/ghc/-/commit/317a915bc46fee2c824d595b0d618057bf7fbbf1#82b5a034883a3ede9540d6423738da627660f860
import qualified Parse.Module as Parse
import qualified Reporting.Error
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Exit.Help
import qualified Wasm.ReadArtifacts as ReadArtifacts
import qualified Wasm.Reporting

main :: IO ()
main = mempty

foreign export javascript "buildArtifacts" buildArtifacts :: IO ()
buildArtifacts = putStrLn "TODO buildArtifacts"

-- The main compilation logic is the same as
-- `../worker/src/Endpoint/Compile.hs``

data Outcome
  = Success ModuleName.Raw String
  | NoMain
  | BadInput ModuleName.Raw Reporting.Error.Error

foreign export javascript "compile"
  compileWasm :: Wasm.JSString -> IO Wasm.JSString

compileWasm :: Wasm.JSString -> IO Wasm.JSString
compileWasm jsString =
  let
    str = Wasm.fromJSString jsString
    source = BSU.fromString $ trace "parsing" $ traceShowId str
  in
  do
    trace "wrote sample file" $ Data.ByteString.Builder.writeFile "/wasm-can-write" (Data.ByteString.Builder.stringUtf8 "horst")
    trace "wrote sample file" $ Data.ByteString.Builder.writeFile "/packages/wasm-can-write" (Data.ByteString.Builder.stringUtf8 "horst")
    outcome <- compile source
    pure $ encodeJson $ outcomeToJson source outcome

compile :: BSU.ByteString -> IO Outcome
compile source =
  case parse source of
    Left err ->
      pure $ BadInput Data.Name._Main (Reporting.Error.BadSyntax err)

    Right modul@(Src.Module _ _ _ imports _ _ _ _ _) ->
      -- TODO add a way to verify imports
      let importNames = fmap Src.getImportName imports in
        do
          artifacts <- ReadArtifacts.getArtifactsForWasm
          -- traceShow "show importNames" $ traceShow importNames $
          case Compile.compile Pkg.dummyName (ReadArtifacts.interfaces artifacts) modul of
              Left err ->
                pure $ BadInput (Src.getName modul) err

              Right (Compile.Artifacts canModule _ locals) ->
                trace "Compile did not fail" $ case locals of
                  Opt.LocalGraph Nothing _ _ ->
                    pure NoMain

                  Opt.LocalGraph (Just main_) _ _ ->
                    let 
                      mode = Mode.Dev Nothing
                      home = Can._name canModule
                      name = ModuleName._module home
                      mains = Map.singleton home main_
                      graph = Opt.addLocalGraph locals (ReadArtifacts.objects artifacts)
                      js :: Data.ByteString.Builder.Builder
                      js = trace "generated js" $ JS.generate mode graph mains
                      _ = traceShowId js
                      filename = "generated.js"
                      filepath = "/tmp/" ++ filename
                    in do
                      trace "Success3, generated JS code" $ Data.ByteString.Builder.writeFile "/absolute.js" js
                      trace "Success2, generated JS code" $ Data.ByteString.Builder.writeFile "./relative.js" js
                      trace "Success, generated JS code" $ Data.ByteString.Builder.writeFile filepath js
                      pure $ Success name filename

parse :: BSU.ByteString -> Either Syntax.Error Src.Module
parse bs =
  Parse.fromByteString Parse.Application bs

outcomeToJson :: BSU.ByteString -> Outcome -> Json.Encode.Value
outcomeToJson source outcome =
  case outcome of
    Success name file ->
      Json.Encode.object
        [ "type" ==> Json.Encode.chars "success"
        , "file" ==> Json.Encode.chars file
        , "name" ==> Json.Encode.chars (ModuleName.toChars name)
        ]

    NoMain ->
      reportToJson $ Wasm.Reporting.noMain

    BadInput name err ->
      reportToJson $
        Reporting.Exit.Help.compilerReport "/"
          (Reporting.Error.Module name "/try" File.zeroTime source err)
          []

reportToJson :: Reporting.Exit.Help.Report -> Json.Encode.Value
reportToJson =
  Reporting.Exit.Help.reportToJson

encodeJson :: Json.Encode.Value -> Wasm.JSString
encodeJson value =
  builderToJsString $ Json.Encode.encode value

builderToJsString :: Data.ByteString.Builder.Builder -> Wasm.JSString
builderToJsString builder =
  let
    lazyStr :: Data.ByteString.Lazy.LazyByteString
    lazyStr = Data.ByteString.Builder.toLazyByteString builder
    str :: String
    str = Data.ByteString.Lazy.UTF8.toString lazyStr
  in
  Wasm.toJSString str
