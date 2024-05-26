module Wasm.ReadArtifacts where


import AST.Optimized as Opt
import Control.Monad (liftM, liftM2, liftM3)
import Data.Binary (Binary, decodeOrFail, get, put)
import Data.Binary qualified as Binary
import Data.ByteString.Lazy.UTF8 (ByteString, fromString)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe, catMaybes)
import Data.Set qualified as Set
import Debug.Trace
import Elm.Interface qualified as I
import Elm.ModuleName qualified as ModuleName
import Elm.Package qualified as Pkg
import Elm.Version qualified as V
import System.IO qualified as IO
import GHC.Wasm.Prim
import qualified System.Directory as Dir
import Data.Maybe (maybeToList)

data ArtifactsForWasm = ArtifactsForWasm
  { interfaces :: Map.Map ModuleName.Raw I.Interface,
    objects :: Opt.GlobalGraph
  }

getArtifactsForWasm :: IO ArtifactsForWasm
getArtifactsForWasm =
    do
      artifacts <- getArtifacts
      let 
        interfaces :: [Map.Map ModuleName.Raw I.Interface]
        interfaces = map artifactToCompileInterface artifacts
        in
          pure $ ArtifactsForWasm 
            { interfaces = Map.unions interfaces,
              objects = foldr mergeGlobalObjectsGraphs Opt.empty artifacts
            }

-- copied from builder/src/Elm/Details
data Artifacts
  = Artifacts
  { _ifaces :: Map.Map ModuleName.Raw I.DependencyInterface,
    _objects :: Opt.GlobalGraph
  }

data ArtifactCache
  = ArtifactCache
  { _fingerprints :: Set.Set Fingerprint,
    _artifacts :: Artifacts
  }

type Fingerprint =
  Map.Map Pkg.Name V.Version

instance Binary Artifacts where
  get = liftM2 Artifacts get get
  put (Artifacts a b) = put a >> put b

instance Binary ArtifactCache where
  get = liftM2 ArtifactCache get get
  put (ArtifactCache a b) = put a >> put b

-- / copied from builder/src/Elm/Details

artifactToCompileInterface :: Artifacts -> Map.Map ModuleName.Raw I.Interface
artifactToCompileInterface (Artifacts _ifaces _) =
  Map.mapMaybe toCompileInterface _ifaces

toCompileInterface :: I.DependencyInterface -> Maybe I.Interface
toCompileInterface dep =
  case dep of
    I.Public interface ->
      Just interface
    I.Private {} ->
      Nothing

mergeGlobalObjectsGraphs :: Artifacts -> Opt.GlobalGraph -> Opt.GlobalGraph
mergeGlobalObjectsGraphs (Artifacts _ objects) acc =
  Opt.addGlobalGraph acc objects

getArtifacts :: IO [Artifacts]
getArtifacts =
  do
    maybeArtifacts <-
      mapM readArtifacts
        [ "elm/core/1.0.5"
        , "elm/html/1.0.0"
        , "elm/browser/1.0.2"
        , "elm/json/1.1.3"
        , "elm/virtual-dom/1.0.3"
        ]
    return (catMaybes maybeArtifacts)
  

readArtifacts :: (Binary ArtifactCache) => FilePath -> IO (Maybe Artifacts)
readArtifacts package =
  let path = "/packages/" ++ package ++ "/artifacts.dat" in
  do
    cache <- readBinary path
    return (fmap onlyArtifacts cache)

onlyArtifacts :: ArtifactCache -> Artifacts
onlyArtifacts cache =
    _artifacts cache

readBinary :: (Binary a) => FilePath -> IO (Maybe a)
readBinary path =
  -- / copied from builder/src/File.hs
  do
    pathExists <- (traceShow ("does file exist? " ++ path ) (Dir.doesFileExist path))
    
    if traceShowId pathExists
      then do
        result <- Binary.decodeFileOrFail path
        case result of
          Right a ->
            return (Just a)
          Left (offset, message) ->
            do
            --   IO.hPutStrLn IO.stderr $
            --     unlines $
            --       [ "+-------------------------------------------------------------------------------",
            --         "|  Corrupt File: " ++ path,
            --         "|   Byte Offset: " ++ show offset,
            --         "|       Message: " ++ message,
            --         "|",
            --         "| Please report this to https://github.com/elm/compiler/issues",
            --         "| Trying to continue anyway.",
            --         "+-------------------------------------------------------------------------------"
            --       ]
              return Nothing
      else
        trace ("ERROR: file " ++ path ++ " does not exist") $
           return Nothing