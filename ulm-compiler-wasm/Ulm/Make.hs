module Ulm.Make
  (make)
where

-- Copied from elm-compiler-wasm/terminal/src/Make.hs


import qualified Data.ByteString.Builder as B
import qualified Data.Maybe as Maybe
import qualified Data.NonEmptyList as NE
import qualified System.Directory as Dir
import qualified System.FilePath as FP

import qualified AST.Optimized as Opt
-- import qualified BackgroundWriter as BW
import qualified Ulm.Build as Build
import qualified Ulm.Details as Details
import qualified Elm.ModuleName as ModuleName
import qualified File
import qualified Ulm.Generate as Generate
-- import qualified Generate.Html as Html
-- import qualified Reporting
import Ulm.Reporting.Exit as Exit
import qualified Reporting.Task as Task



-- RUN


type Task a = Task.Task Exit.Make a

make :: FilePath -> IO (Either Exit.Make FilePath)
make file =
  let target = "elm.js" in
  -- `runHelp` from elm-compiler-wasm/terminal/src/Make.hs
  Task.run $
  do 
    let root = "/"
    let desiredMode = Dev
    details <- Task.eio Exit.MakeBadDetails (Details.load root)
    -- Note: `runHelp` in elm-compiler-wasm/terminal/src/Make.hs also contains code to compile multiple files into one
    -- exposed <- getExposed details
    -- buildExposed root details maybeDocs exposed
    artifacts <- buildPaths root details (NE.List file [])
    case getNoMains artifacts of
      [] ->
        do  builder <- toBuilder root details desiredMode artifacts
            generate target builder (Build.getRootNames artifacts)

      name:names ->
        Task.throw (Exit.MakeNonMainFilesIntoJavaScript name names)



-- BUILD PROJECTS


buildPaths :: FilePath -> Details.Details -> NE.List FilePath -> Task Build.Artifacts
buildPaths root details paths =
  Task.eio Exit.MakeCannotBuild $
    Build.fromPaths root details paths



-- GET MAINS


isMain :: ModuleName.Raw -> Build.Module -> Bool
isMain targetName modul =
  case modul of
    Build.Fresh name _ (Opt.LocalGraph maybeMain _ _) ->
      Maybe.isJust maybeMain && name == targetName

    Build.Cached name mainIsDefined _ ->
      mainIsDefined && name == targetName



-- GET MAINLESS


getNoMains :: Build.Artifacts -> [ModuleName.Raw]
getNoMains (Build.Artifacts _ _ roots modules) =
  Maybe.mapMaybe (getNoMain modules) (NE.toList roots)


getNoMain :: [Build.Module] -> Build.Root -> Maybe ModuleName.Raw
getNoMain modules root =
  case root of
    Build.Inside name ->
      if any (isMain name) modules
      then Nothing
      else Just name

    Build.Outside name _ (Opt.LocalGraph maybeMain _ _) ->
      case maybeMain of
        Just _  -> Nothing
        Nothing -> Just name



-- GENERATE


generate :: FilePath -> B.Builder -> NE.List ModuleName.Raw -> Task FilePath
generate target builder names =
  Task.io $
    do  Dir.createDirectoryIfMissing True (FP.takeDirectory target)
        File.writeBuilder target builder
        -- Reporting.reportGenerate style names target
        pure target



-- TO BUILDER


data DesiredMode = Debug | Dev | Prod


toBuilder :: FilePath -> Details.Details -> DesiredMode -> Build.Artifacts -> Task B.Builder
toBuilder root details desiredMode artifacts =
  Task.mapError Exit.MakeBadGenerate $
    case desiredMode of
      Debug -> Generate.debug root details artifacts
      Dev   -> Generate.dev   root details artifacts
      Prod  -> Generate.prod  root details artifacts