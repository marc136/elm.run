module WasmRepl where

import Data.ByteString.Builder qualified
import Data.ByteString.Lazy qualified
import Data.ByteString.Lazy.UTF8 qualified -- from utf8-string
import Data.ByteString.UTF8 qualified -- from utf8-string
import Debug.Trace
import GHC.Wasm.Prim qualified as Wasm -- See https://gitlab.haskell.org/ghc/ghc/-/commit/317a915bc46fee2c824d595b0d618057bf7fbbf1#82b5a034883a3ede9540d6423738da627660f860
import Json.Encode ((==>))
import Json.Encode qualified
import ToStringHelper
import Ulm.Repl qualified

main :: IO ()
main = mempty

foreign export javascript "wip"
  -- current work-in-progress helper to avoid changing `./ulm.cabal` for every export
  wipJs :: Wasm.JSString -> IO Wasm.JSString

wipJs :: Wasm.JSString -> IO Wasm.JSString
wipJs jsString =
  let str = Wasm.fromJSString jsString
      source = Data.ByteString.UTF8.fromString $ trace "parsing" $ traceShowId str
   in do
    fmap encodeJson $ Ulm.Repl.read str

foreign export javascript "check"
  checkJs :: Wasm.JSString -> IO Wasm.JSString

checkJs :: Wasm.JSString -> IO Wasm.JSString
checkJs jsString =
  fmap encodeJson $ Ulm.Repl.checkRepl (Wasm.fromJSString jsString)

foreign export javascript "evaluate"
  evaluateJs :: Wasm.JSString -> IO Wasm.JSString

evaluateJs :: Wasm.JSString -> IO Wasm.JSString
evaluateJs jsString =
  fmap encodeJson $ Ulm.Repl.evaluateRepl (Wasm.fromJSString jsString)

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
