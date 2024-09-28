module WasmMake where

import Data.ByteString.Builder qualified
import Data.ByteString.Lazy qualified
import Data.ByteString.Lazy.UTF8 qualified -- from utf8-string
import Data.ByteString.UTF8 qualified as BSU -- from utf8-string
import Data.Map qualified as Map
import Debug.Trace
import Elm.Package qualified
-- See https://gitlab.haskell.org/ghc/ghc/-/commit/317a915bc46fee2c824d595b0d618057bf7fbbf1#82b5a034883a3ede9540d6423738da627660f860
import GHC.Wasm.Prim qualified as Wasm
import Json.Encode qualified
import Ulm.Details qualified
import Ulm.Install qualified
import Ulm.Make qualified

main :: IO ()
main = mempty

foreign export javascript "wip"
  -- current work-in-progress helper to avoid changing `./ulm.cabal` for every export
  wipJs :: Wasm.JSString -> IO Wasm.JSString

wipJs :: Wasm.JSString -> IO Wasm.JSString
wipJs jsString =
  let -- str = "elm/html"
      -- source = BSU.fromString $ trace "wipJs" $ traceShowId str
      str = Wasm.fromJSString jsString
   in do
        fmap encodeJson $ Ulm.Install.installJson str

foreign export javascript "buildArtifacts" buildArtifacts :: IO ()

buildArtifacts = putStrLn "TODO buildArtifacts"

foreign export javascript "make"
  makeWasm :: Wasm.JSString -> IO Wasm.JSString

makeWasm :: Wasm.JSString -> IO Wasm.JSString
makeWasm filepath = do
  let path = Wasm.fromJSString filepath
  outcome <- Ulm.Make.makeFile path
  pure $ encodeJson $ Ulm.Make.outcomeToJson (BSU.fromString path) outcome

foreign export javascript "compile"
  compileWasm :: Wasm.JSString -> IO Wasm.JSString

compileWasm :: Wasm.JSString -> IO Wasm.JSString
compileWasm jsString =
  let str = Wasm.fromJSString jsString
      source = BSU.fromString $ trace "parsing" $ traceShowId str
   in do
        trace "wrote sample file" $ Data.ByteString.Builder.writeFile "/wasm-can-write" (Data.ByteString.Builder.stringUtf8 "horst")
        outcome <- Ulm.Make.compileThis source
        pure $ encodeJson $ Ulm.Make.outcomeToJson source outcome

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
