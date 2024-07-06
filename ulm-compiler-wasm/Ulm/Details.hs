module Ulm.Details where

-- clone of elm-compiler-wasm/builder/src/Elm/Details.hs

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)
import Control.Monad (liftM, liftM2, liftM3)
import Data.Binary (Binary, get, put, getWord8, putWord8)
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Map.Utils as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Name as Name
import qualified Data.NonEmptyList as NE
import qualified Data.OneOrMore as OneOrMore
import qualified Data.Set as Set
import qualified Data.Utf8 as Utf8
import Data.Word (Word64)
import qualified System.Directory as Dir
import System.FilePath ((</>), (<.>))

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Optimized as Opt
-- import qualified BackgroundWriter as BW
-- import qualified Compile
import qualified Ulm.Deps.Registry as Registry
import qualified Ulm.Deps.Solver as Solver
-- import qualified Deps.Website as Website
import qualified Elm.Constraint as Con
-- import qualified Elm.Docs as Docs
import qualified Elm.Interface as I
-- import qualified Elm.Kernel as Kernel
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified File
-- import qualified Http
-- import qualified Json.Decode as D
-- import qualified Json.Encode as E
import qualified Json.Encode
import Json.Encode ((==>))
-- import qualified Parse.Module as Parse
-- import qualified Reporting
-- import qualified Reporting.Annotation as A
import qualified Ulm.Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Stuff
import ToStringHelper
import Debug.Trace (traceShow, traceShowId)

wipJson :: IO Json.Encode.Value
wipJson =
  do  loaded <- load "/"
      let shown = traceShow "loaded" loaded
      case traceShowId loaded of
        Left err ->
          wip "loaded err"
        Right ok ->
          pure $
            Json.Encode.object
              [ "wip" ==> Json.Encode.chars "loaded ok"
              , "ok" ==> Json.Encode.chars (show ok)
              ]


wip str =
  pure $
    Json.Encode.object
      [ "wip" ==> Json.Encode.chars str
      ]


-- DETAILS


data Details =
  Details
    { _outlineTime :: () -- File.Time
    , _outline :: ValidOutline
    , _buildID :: () -- BuildID
    , _locals :: Map.Map ModuleName.Raw Local
    , _foreigns :: Map.Map ModuleName.Raw Foreign
    , _extras :: () -- ArtifactsCached | ArtifactsFresh
    }
  deriving (Show)


data ValidOutline
  = ValidApp (NE.List Outline.SrcDir)
  | ValidPkg Pkg.Name [ModuleName.Raw] (Map.Map Pkg.Name V.Version {- for docs in reactor -})
  deriving (Show)

data Local =
  Local
    { _path :: FilePath
    , _time :: () -- File.Time
    , _deps :: [ModuleName.Raw]
    , _main :: Bool
    , _lastChange :: () -- BuildID
    , _lastCompile :: () -- BuildID
    }
  deriving (Show)

data Foreign =
  Foreign Pkg.Name [Pkg.Name]
  deriving (Show)

type Interfaces =
  Map.Map ModuleName.Canonical I.DependencyInterface



-- LOAD -- used by Make, Repl, Reactor


load :: FilePath -> IO (Either Exit.Details Details)
load root =
    generate root



-- GENERATE


generate ::  FilePath ->  IO (Either Exit.Details Details)
generate root =
  do
    result <- initEnv root
    case result of
      Left exit ->
        pure (Left exit)

      Right (env, outline) ->
        case outline of
            Outline.Pkg pkg -> Task.run (verifyPkg env pkg)
            Outline.App app -> Task.run (verifyApp env app)



-- ENV


data Env =
  Env
    { _key :: () -- from Reporting, used to track tasks for Reporting.report
    , _scope :: () -- BW.Scope -- not sure if I will need this
    , _root :: FilePath
    , _cache :: Stuff.PackageCache
    , _manager :: () -- Http
    , _connection :: () -- uses Http, always use `Offline` instead
    , _registry :: Registry.Registry 
    }


initEnv :: FilePath -> IO (Either Exit.Details (Env, Outline.Outline))
initEnv root =
  -- maybe I need to clone Solver.initEnv
  do  cache <- Stuff.getPackageCache
      maybeRegistry <- Registry.read cache
      eitherOutline <- Outline.read root
      case (eitherOutline, maybeRegistry) of
        (Left problem, _) ->
          return $ Left $ Exit.DetailsBadOutline problem
        
        (_, Nothing) ->
            return $ Left $ Exit.DetailsNoRegistryCache

        (Right outline, Just registry) ->
          return $ Right $ (Env () () root cache () () registry, traceShowId outline)



-- VERIFY PROJECT

type Task a = Task.Task Exit.Details a
-- type Task a = IO (Either Exit.Details a)


verifyPkg :: Env -> Outline.PkgOutline -> Task Details
verifyPkg env (Outline.PkgOutline pkg _ _ _ exposed direct testDirect elm) =
  -- if Con.goodElm elm
  -- then
  --   do  solution <- verifyConstraints env =<< union noDups direct testDirect
  --       let exposedList = Outline.flattenExposed exposed
  --       let exactDeps = Map.map (\(Solver.Details v _) -> v) solution -- for pkg docs in reactor
  --       verifyDependencies env (ValidPkg pkg exposedList exactDeps) solution direct
  -- else
    Task.throw $ Exit.DetailsBadElmInPkg elm


verifyApp :: Env -> Outline.AppOutline -> Task Details
verifyApp env outline@(Outline.AppOutline elmVersion srcDirs direct _ _ _) =
  if elmVersion == V.compiler
  then
    do  stated <- checkAppDeps outline
        let l = traceShow "stated deps" stated
        actual <- verifyConstraints env (Map.map Con.exactly stated)
        let l2 = traceShow "actual deps" actual
        if Map.size stated == Map.size actual
          then verifyDependencies env (ValidApp srcDirs) (traceShow "actual" actual) direct
          else Task.throw $ Exit.DetailsHandEditedDependencies
  else
    Task.throw $ Exit.DetailsBadElmInAppOutline elmVersion


checkAppDeps :: Outline.AppOutline -> Task (Map.Map Pkg.Name V.Version)
checkAppDeps (Outline.AppOutline _ _ direct indirect testDirect testIndirect) =
  do  x <- union allowEqualDups indirect testDirect
      y <- union noDups direct testIndirect
      union noDups x y



-- VERIFY CONSTRAINTS


verifyConstraints :: Env -> Map.Map Pkg.Name Con.Constraint -> Task (Map.Map Pkg.Name Solver.Details)
verifyConstraints (Env _ _ _ cache _ _ registry) constraints =
  do  result <- Task.io $ Solver.verify cache registry constraints
      case result of
        Solver.Ok details        -> return details
        Solver.NoOfflineSolution -> Task.throw $ Exit.DetailsNoOfflineSolution
        Solver.Err exit          -> Task.throw $ Exit.DetailsSolverProblem exit



-- UNION


union :: (Ord k) => (k -> v -> v -> Task v) -> Map.Map k v -> Map.Map k v -> Task (Map.Map k v)
union tieBreaker deps1 deps2 =
  Map.mergeA Map.preserveMissing Map.preserveMissing (Map.zipWithAMatched tieBreaker) deps1 deps2


noDups :: k -> v -> v -> Task v
noDups _ _ _ =
  Task.throw Exit.DetailsHandEditedDependencies


allowEqualDups :: (Eq v) => k -> v -> v -> Task v
allowEqualDups _ v1 v2 =
  if v1 == v2
  then return v1
  else Task.throw Exit.DetailsHandEditedDependencies



-- FORK


fork :: IO a -> IO (MVar a)
fork work =
  do  mvar <- newEmptyMVar
      _ <- forkIO $ putMVar mvar =<< work
      return mvar



-- VERIFY DEPENDENCIES


verifyDependencies :: Env -> ValidOutline -> Map.Map Pkg.Name Solver.Details -> Map.Map Pkg.Name a -> Task Details
verifyDependencies env@(Env key scope root cache _ _ _) outline solution directDeps =
  Task.eio id $
  do  --Reporting.report key (Reporting.DStart (Map.size solution))
      mvar <- newEmptyMVar
      mvars <- Map.traverseWithKey (\k v -> fork (verifyDep env mvar solution k v)) solution
      putMVar mvar mvars
      deps <- traverse readMVar mvars
      case sequence deps of
        Left _ ->
          do  home <- Stuff.getElmHome
              return $ Left $ Exit.DetailsBadDeps home $
                Maybe.catMaybes $ Either.lefts $ Map.elems deps

        Right artifacts ->
          let
            objs = Map.foldr addObjects Opt.empty artifacts
            ifaces = Map.foldrWithKey (addInterfaces directDeps) Map.empty artifacts
            foreigns = Map.map (OneOrMore.destruct Foreign) $ Map.foldrWithKey gatherForeigns Map.empty $ Map.intersection artifacts directDeps
            details = Details () outline () Map.empty foreigns ()
          in
          do  File.writeBinary (Stuff.objects    root) objs
              File.writeBinary (Stuff.interfaces root) ifaces
              File.writeBinary (Stuff.details    root) details
              return (Right details)


addObjects :: Artifacts -> Opt.GlobalGraph -> Opt.GlobalGraph
addObjects (Artifacts _ objs) graph =
  Opt.addGlobalGraph objs graph


addInterfaces :: Map.Map Pkg.Name a -> Pkg.Name -> Artifacts -> Interfaces -> Interfaces
addInterfaces directDeps pkg (Artifacts ifaces _) dependencyInterfaces =
  Map.union dependencyInterfaces $ Map.mapKeysMonotonic (ModuleName.Canonical pkg) $
    if Map.member pkg directDeps
      then ifaces
      else Map.map I.privatize ifaces


gatherForeigns :: Pkg.Name -> Artifacts -> Map.Map ModuleName.Raw (OneOrMore.OneOrMore Pkg.Name) -> Map.Map ModuleName.Raw (OneOrMore.OneOrMore Pkg.Name)
gatherForeigns pkg (Artifacts ifaces _) foreigns =
  let
    isPublic di =
      case di of
        I.Public _      -> Just (OneOrMore.one pkg)
        I.Private _ _ _ -> Nothing
  in
  Map.unionWith OneOrMore.more foreigns (Map.mapMaybe isPublic ifaces)



-- VERIFY DEPENDENCY


data Artifacts =
  Artifacts
    { _ifaces :: Map.Map ModuleName.Raw I.DependencyInterface
    , _objects :: Opt.GlobalGraph
    }


type Dep =
  Either (Maybe Exit.DetailsBadDep) Artifacts


verifyDep :: Env -> MVar (Map.Map Pkg.Name (MVar Dep)) -> Map.Map Pkg.Name Solver.Details -> Pkg.Name -> Solver.Details -> IO Dep
verifyDep (Env key _ _ cache manager _ _) depsMVar solution pkg details@(Solver.Details vsn directDeps) =
  do  let fingerprint = Map.intersectionWith (\(Solver.Details v _) _ -> v) solution directDeps
      let dir = Stuff.package cache pkg vsn </> "src"
      exists <- Dir.doesDirectoryExist dir
      if exists
        then
          do  --Reporting.report key Reporting.DCached
              maybeCache <- File.readBinary (Stuff.package cache pkg vsn </> "artifacts.dat")
              case maybeCache of
                Nothing ->
                  build cache depsMVar pkg details fingerprint Set.empty

                Just (ArtifactCache fingerprints artifacts) ->
                  if Set.member fingerprint fingerprints
                    -- then Reporting.report key Reporting.DBuilt >> return (Right artifacts)
                    then return (Right artifacts)
                    else build cache depsMVar pkg details fingerprint fingerprints
        else
          return $ Left $ Just $ Exit.BD_BadDownload pkg vsn (Exit.PP_DirectoryDoesNotExist dir)



-- ARTIFACT CACHE


data ArtifactCache =
  ArtifactCache
    { _fingerprints :: Set.Set Fingerprint
    , _artifacts :: Artifacts
    }


type Fingerprint =
  Map.Map Pkg.Name V.Version



-- BUILD


build :: Stuff.PackageCache -> MVar (Map.Map Pkg.Name (MVar Dep)) -> Pkg.Name -> Solver.Details -> Fingerprint -> Set.Set Fingerprint -> IO Dep
build cache depsMVar pkg (Solver.Details vsn _) f fs =
  pure (Left (Just Exit.BD_MARC_TODO_BUILD))



-- BINARY


instance Binary Details where
  put (Details a b c d e _) = put a >> put b >> put c >> put d >> put e
  get =
    do  a <- get
        b <- get
        c <- get
        d <- get
        e <- get
        return (Details a b c d e ())


instance Binary ValidOutline where
  put outline =
    case outline of
      ValidApp a     -> putWord8 0 >> put a
      ValidPkg a b c -> putWord8 1 >> put a >> put b >> put c

  get =
    do  n <- getWord8
        case n of
          0 -> liftM  ValidApp get
          1 -> liftM3 ValidPkg get get get
          _ -> fail "binary encoding of ValidOutline was corrupted"


instance Binary Local where
  put (Local a b c d e f) = put a >> put b >> put c >> put d >> put e >> put f
  get =
    do  a <- get
        b <- get
        c <- get
        d <- get
        e <- get
        f <- get
        return (Local a b c d e f)


instance Binary Foreign where
  get = liftM2 Foreign get get
  put (Foreign a b) = put a >> put b


instance Binary Artifacts where
  get = liftM2 Artifacts get get
  put (Artifacts a b) = put a >> put b


instance Binary ArtifactCache where
  get = liftM2 ArtifactCache get get
  put (ArtifactCache a b) = put a >> put b
