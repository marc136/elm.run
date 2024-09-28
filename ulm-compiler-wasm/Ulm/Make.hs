module Ulm.Make
  ( Outcome,
    compileThis,
    outcomeToJson,
    makeFile,
  )
where

import AST.Canonical qualified as Can
import AST.Optimized as Opt
import AST.Source qualified as Src
import Compile qualified
import Data.ByteString.Builder qualified
import Data.ByteString.Lazy qualified
import Data.ByteString.Lazy.UTF8 qualified -- from utf8-string
import Data.ByteString.UTF8 qualified as BSU -- from utf8-string
import Data.Map qualified as Map
import Data.Map.Utils qualified as Map
import Data.Name qualified
import Data.NonEmptyList qualified as NE
import Debug.Trace
import Elm.Interface qualified as I
import Elm.ModuleName qualified as ModuleName
import Elm.Outline qualified
import Elm.Package qualified as Pkg
import File qualified
import Generate.JavaScript qualified as JS
import Generate.Mode qualified as Mode
import Json.Encode ((==>))
import Json.Encode qualified
import Parse.Module qualified as Parse
import Reporting.Annotation qualified
import Reporting.Error qualified
import Reporting.Error.Import qualified
import Reporting.Error.Syntax qualified as Syntax
import Reporting.Exit.Help qualified
import ToStringHelper
import Ulm.Details qualified
import Ulm.ReadArtifacts qualified as ReadArtifacts
import Ulm.Repl qualified
import Ulm.Reporting qualified
import Ulm.Reporting.Exit qualified as Exit

data Outcome
  = Success ModuleName.Raw String
  | NoMain
  | BadInput ModuleName.Raw Reporting.Error.Error
  | BuildingDependenciesFailed
  deriving (Show)

makeFile :: String -> IO Outcome
makeFile filepath = do
  source <- File.readUtf8 filepath
  compileThis source

compileThis :: BSU.ByteString -> IO Outcome
compileThis source =
  -- The main compilation logic is the same as
  -- `../worker/src/Endpoint/Compile.hs`
  case parse source of
    Left err ->
      pure $ BadInput Data.Name._Main (Reporting.Error.BadSyntax err)
    Right modul@(Src.Module _ _ _ imports _ _ _ _ _) -> do
      loaded <- Ulm.Details.loadArtifactsForApp "/"
      case loaded of
        Left err ->
          do
            putStrLn ("loadArtifacts error" ++ show err)
            pure BuildingDependenciesFailed
        Right artifacts ->
          case checkImports (ReadArtifacts.interfaces artifacts) imports of
            Left err ->
              pure $ BadInput (Src.getName modul) (Reporting.Error.BadImports err)
            Right ifaces ->
              let importNames = fmap Src.getImportName imports
               in do
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
                                js = JS.generate mode graph mains
                                filename = "generated.js"
                                filepath = "/tmp/" ++ filename
                             in do
                                  Data.ByteString.Builder.writeFile filepath js
                                  putStrLn "Success, generated JS code"
                                  pure $ Success name filepath

parse :: BSU.ByteString -> Either Syntax.Error Src.Module
parse bs =
  Parse.fromByteString Parse.Application bs

checkImports :: Map.Map ModuleName.Raw I.Interface -> [Src.Import] -> Either (NE.List Reporting.Error.Import.Error) (Map.Map ModuleName.Raw I.Interface)
checkImports interfaces imports =
  let importDict = Map.fromValues Src.getImportName imports
      missing = Map.difference importDict interfaces
   in case Map.elems missing of
        [] ->
          Right (Map.intersection interfaces importDict)
        i : is ->
          let unimported =
                Map.keysSet (Map.difference interfaces importDict)

              toError (Src.Import (Reporting.Annotation.At region name) _ _) =
                Reporting.Error.Import.Error region name unimported Reporting.Error.Import.NotFound
           in Left (fmap toError (NE.List i is))

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
    BuildingDependenciesFailed ->
      Json.Encode.object
        [ "type" ==> Json.Encode.chars "dependency-error",
          "message" ==> Json.Encode.chars "Could not build all dependencies"
        ]

reportToJson :: Reporting.Exit.Help.Report -> Json.Encode.Value
reportToJson =
  Reporting.Exit.Help.reportToJson
