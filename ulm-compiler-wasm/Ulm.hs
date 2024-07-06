module Ulm where

import AST.Canonical qualified as Can
import AST.Optimized as Opt
import AST.Source qualified as Src
import Compile qualified
import Data.ByteString.Builder qualified
import Data.ByteString.Lazy qualified
import Data.ByteString.Lazy.UTF8 qualified -- from utf8-string
import Data.ByteString.UTF8 qualified as BSU -- from utf8-string
import Data.Map qualified as Map
import Data.Name qualified
import Debug.Trace
import Elm.ModuleName qualified as ModuleName
import Elm.Outline qualified
import Elm.Package qualified as Pkg
import File qualified
import GHC.Wasm.Prim qualified as Wasm -- See https://gitlab.haskell.org/ghc/ghc/-/commit/317a915bc46fee2c824d595b0d618057bf7fbbf1#82b5a034883a3ede9540d6423738da627660f860
import Generate.JavaScript qualified as JS
import Generate.Mode qualified as Mode
import Json.Encode ((==>))
import Json.Encode qualified
import Parse.Module qualified as Parse
import Reporting.Error qualified
import Reporting.Error.Syntax qualified as Syntax
import Reporting.Exit.Help qualified
import ToStringHelper
import Ulm.Details qualified
import Ulm.ReadArtifacts qualified as ReadArtifacts
import Ulm.Reporting qualified

main :: IO ()
main = mempty

foreign export javascript "wip"
  -- current work-in-progress helper to avoid changing `./ulm.cabal` for every export
  wipJs :: Wasm.JSString -> IO Wasm.JSString

wipJs :: Wasm.JSString -> IO Wasm.JSString
wipJs jsString =
  let str = Wasm.fromJSString jsString
      source = BSU.fromString $ trace "parsing" $ traceShowId str
   in do
        fmap encodeJson Ulm.Details.wipJson

foreign export javascript "buildArtifacts" buildArtifacts :: IO ()

buildArtifacts = putStrLn "TODO buildArtifacts"

-- The main compilation logic is the same as
-- `../worker/src/Endpoint/Compile.hs`

data Outcome
  = Success ModuleName.Raw String
  | NoMain
  | BadInput ModuleName.Raw Reporting.Error.Error

foreign export javascript "compile"
  compileWasm :: Wasm.JSString -> IO Wasm.JSString

compileWasm :: Wasm.JSString -> IO Wasm.JSString
compileWasm jsString =
  let str = Wasm.fromJSString jsString
      source = BSU.fromString $ trace "parsing" $ traceShowId str
   in do
        trace "wrote sample file" $ Data.ByteString.Builder.writeFile "/wasm-can-write" (Data.ByteString.Builder.stringUtf8 "horst")
        trace "wrote sample file" $ Data.ByteString.Builder.writeFile "/packages/wasm-can-write" (Data.ByteString.Builder.stringUtf8 "horst")
        outcome <- compileWithPrebuiltDependencies source
        pure $ encodeJson $ outcomeToJson source outcome

-- This might be useful for a repl. It cannot compile dependencies, but the file size is smaller.
-- For a repl that might be more important
compileWithPrebuiltDependencies :: BSU.ByteString -> IO Outcome
compileWithPrebuiltDependencies source =
  case parse source of
    Left err ->
      pure $ BadInput Data.Name._Main (Reporting.Error.BadSyntax err)
    Right modul@(Src.Module _ _ _ imports _ _ _ _ _) ->
      let importNames = fmap Src.getImportName imports
       in do
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
                    let mode = Mode.Dev Nothing
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
                          pure $ Success name filepath

parse :: BSU.ByteString -> Either Syntax.Error Src.Module
parse bs =
  Parse.fromByteString Parse.Application bs

outcomeToJson :: BSU.ByteString -> Outcome -> Json.Encode.Value
outcomeToJson source outcome =
  case outcome of
    Success name file ->
      Json.Encode.object
        [ "type" ==> Json.Encode.chars "success",
          "file" ==> Json.Encode.chars file,
          "name" ==> Json.Encode.chars (ModuleName.toChars name)
        ]
    NoMain ->
      reportToJson $ Ulm.Reporting.noMain
    BadInput name err ->
      reportToJson $
        Reporting.Exit.Help.compilerReport
          "/"
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
  let lazyStr :: Data.ByteString.Lazy.LazyByteString
      lazyStr = Data.ByteString.Builder.toLazyByteString builder
      str :: String
      str = Data.ByteString.Lazy.UTF8.toString lazyStr
   in Wasm.toJSString str
