module Ulm where


import qualified AST.Canonical as Can
import AST.Optimized as Opt
import qualified AST.Source as Src
import qualified Compile
import qualified Data.ByteString.Builder
import qualified Data.ByteString.UTF8 as BSU -- from utf8-string
import qualified Data.Map as Map
import qualified Data.Map.Utils as Map
import qualified Data.Name as Name
import qualified Elm.Interface
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Generate.Mode as Mode
import Debug.Trace
import qualified Generate.JavaScript as JS
import GHC.Wasm.Prim -- See https://gitlab.haskell.org/ghc/ghc/-/commit/317a915bc46fee2c824d595b0d618057bf7fbbf1#82b5a034883a3ede9540d6423738da627660f860
import qualified Parse.Module as Parse
import qualified Reporting.Error
import qualified Reporting.Error.Syntax as Syntax
import qualified Wasm.ReadArtifacts as ReadArtifacts


main :: IO ()
main = mempty

foreign export javascript "buildArtifacts" buildArtifacts :: IO ()
buildArtifacts = putStrLn "TODO buildArtifacts"

-- The main compilation logic is the same as
-- `../worker/src/Endpoint/Compile.hs``

foreign export javascript "compile"
  compile :: JSString -> IO JSString
compile jsString =
  let
    str = fromJSString jsString
    source = BSU.fromString $ trace "parsing" $ traceShowId str
  in
  case parse source of
    Left err ->
      let wrapped = Reporting.Error.BadSyntax err
    --    in return $ errorToJSString source wrapped
        in return $ toJSString "NoMain"

    Right modul@(Src.Module _ _ _ imports _ _ _ _ _) ->
      -- TODO add a way to verify imports
      let importNames = fmap Src.getImportName imports in
        do
          artifacts <- ReadArtifacts.getArtifactsForWasm
          traceShow "show importNames" $
        --   traceShow "show importNames" $ traceShow importNames $
            case Compile.compile Pkg.dummyName (ReadArtifacts.interfaces artifacts) modul of
              Left err ->
                -- TODO hier möchte ich den error report generieren und als JSString zurück geben
                return $ toJSString "Compile failed"
                -- return $ errorToJSString source err

              Right (Compile.Artifacts canModule _ locals) ->
                trace "Compile did not fail" $ case locals of
                  Opt.LocalGraph Nothing _ _ ->
                    return $ toJSString "NoMain"
                  Opt.LocalGraph (Just main_) _ _ ->
                    let mode = Mode.Dev Nothing
                        home = Can._name canModule
                        name = ModuleName._module home
                        mains = Map.singleton home main_
                        graph = Opt.addLocalGraph locals (ReadArtifacts.objects artifacts)
                        js :: Data.ByteString.Builder.Builder
                        js = trace "generated js" $ JS.generate mode graph mains
                        _ = traceShowId js
                     in -- in Success name $ JS.generate mode graph mains
                        do
                          -- File.writeBuilder "/tmp/generated.js" js
                          trace "Success, generated JS code" $ Data.ByteString.Builder.writeFile "/tmp/generated.js" js
                          -- return (builderToJsString js)
                          return (toJSString "/tmp/generated.js")


parse :: BSU.ByteString -> Either Syntax.Error Src.Module
parse bs =
  Parse.fromByteString Parse.Application bs
