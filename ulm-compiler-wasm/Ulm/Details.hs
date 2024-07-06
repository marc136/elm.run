module Ulm.Details
  ( Details(..)
  -- , BuildID
  , ValidOutline(..)
  , Local(..)
  , Foreign(..)
  , load
  , loadObjects
  , loadInterfaces
  -- , verifyInstall
  --
  , loadArtifactsForApp
  , wipJson
  )
 where

-- clone of elm-compiler-wasm/builder/src/Elm/Details.hs

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar, takeMVar)
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
-- import qualified Data.Utf8 as Utf8
-- import Data.Word (Word64)
import qualified System.Directory as Dir
import System.FilePath ((</>), (<.>))

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Optimized as Opt
-- import qualified BackgroundWriter as BW
import qualified Compile
import qualified Ulm.Deps.Registry as Registry
import qualified Ulm.Deps.Solver as Solver
-- import qualified Deps.Website as Website
import qualified Elm.Constraint as Con
import qualified Elm.Docs as Docs
import qualified Elm.Interface as I
import qualified Elm.Kernel as Kernel
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified File
-- import qualified Http
-- import qualified Json.Decode as D
import qualified Json.Encode as E
import qualified Json.Encode
import Json.Encode ((==>))
import qualified Parse.Module as Parse
-- import qualified Reporting
import qualified Reporting.Annotation as A
import qualified Ulm.Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Ulm.Paths
import qualified Ulm.ReadArtifacts
import ToStringHelper ()
import Debug.Trace (traceShow, traceShowId)

wipJson :: IO Json.Encode.Value
wipJson =
  do  --  loaded <- load "/"
      -- loaded <- loadArtifactsForApp "/"
      -- let shown = traceShow "loaded" loaded
      -- case traceShowId loaded of
      --   Left err ->
      --     wip "loaded err"
      --   Right ok ->
      --     pure $
      --       Json.Encode.object
      --         [ "wip" ==> Json.Encode.chars "loaded ok"
      --         , "ok" ==> Json.Encode.chars (show ok)
      --         ]
      wip "Details.wipJson"

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



-- LOAD ARTIFACTS

loadArtifactsForApp :: FilePath -> IO (Either Exit.Details Ulm.ReadArtifacts.ArtifactsForWasm)
loadArtifactsForApp root =
  -- TODO extract this function into its own module to build a compiler that compiles dependencies and a single file?
  -- Might be useful to reduce bundle size, and it is sufficient for the elm-lang.org/try editor
    do
      env <- initEnv root
      case env of
        Left exit ->
          pure $ Left exit

        Right (env, Outline.Pkg pkg) ->
          pure $ Left Exit.DetailsNoSolution

        Right (env, Outline.App outline@(Outline.AppOutline _ _ direct indirect _ _)) ->
          let packages = Map.foldrWithKey (\p v acc -> (Pkg.toChars p ++ "/" ++ V.toChars v) : acc ) [] (Map.union direct indirect)  in
          do  verified <- Task.run (verifyApp env outline)
              putStrLn ("direct: " ++ show direct)
              putStrLn ("indirect: " ++ show indirect)
              putStrLn ("mapped: " ++ show packages)
              artifacts <- Ulm.ReadArtifacts.getArtifacts packages
              pure $ Right artifacts


loadObjects :: FilePath -> Details -> IO (MVar (Maybe Opt.GlobalGraph))
loadObjects root (Details _ _ _ _ _ extras) =
  fork (File.readBinary (Ulm.Paths.objects root))


loadInterfaces :: FilePath -> Details -> IO (MVar (Maybe Interfaces))
loadInterfaces root (Details _ _ _ _ _ extras) =
  fork (File.readBinary (Ulm.Paths.interfaces root))



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
    , _cache :: Ulm.Paths.PackageCache
    , _manager :: () -- Http
    , _connection :: () -- uses Http, always use `Offline` instead
    , _registry :: Registry.Registry
    }


initEnv :: FilePath -> IO (Either Exit.Details (Env, Outline.Outline))
initEnv root =
  -- maybe I need to clone Solver.initEnv
  do  cache <- Ulm.Paths.getPackageCache
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
          return $ Left $ Exit.DetailsBadDeps Ulm.Paths.packageCacheDir $
              Maybe.catMaybes $ Either.lefts $ Map.elems deps

        Right artifacts ->
          let
            objs = Map.foldr addObjects Opt.empty artifacts
            ifaces = Map.foldrWithKey (addInterfaces directDeps) Map.empty artifacts
            foreigns = Map.map (OneOrMore.destruct Foreign) $ Map.foldrWithKey gatherForeigns Map.empty $ Map.intersection artifacts directDeps
            details = Details () outline () Map.empty foreigns ()
          in
          do  File.writeBinary (Ulm.Paths.objects    root) objs
              File.writeBinary (Ulm.Paths.interfaces root) ifaces
              File.writeBinary (Ulm.Paths.details    root) details
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

instance Show Artifacts where
  show (Artifacts _ifaces _objects) = "Artifacts with inferfaces: " ++ show (Map.keys _ifaces)

type Dep =
  Either (Maybe Exit.DetailsBadDep) Artifacts


verifyDep :: Env -> MVar (Map.Map Pkg.Name (MVar Dep)) -> Map.Map Pkg.Name Solver.Details -> Pkg.Name -> Solver.Details -> IO Dep
verifyDep (Env key _ _ cache manager _ _) depsMVar solution pkg details@(Solver.Details vsn directDeps) =
  do  let fingerprint = Map.intersectionWith (\(Solver.Details v _) _ -> v) solution directDeps
      let dir = Ulm.Paths.package cache pkg vsn </> "src"
      exists <- Dir.doesDirectoryExist dir
      if exists
        then
          do  --Reporting.report key Reporting.DCached
              maybeCache <- File.readBinary (Ulm.Paths.package cache pkg vsn </> "artifacts.dat")
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


build :: Ulm.Paths.PackageCache -> MVar (Map.Map Pkg.Name (MVar Dep)) -> Pkg.Name -> Solver.Details -> Fingerprint -> Set.Set Fingerprint -> IO Dep
build cache depsMVar pkg (Solver.Details vsn _) f fs =
  do  eitherOutline <- Outline.read (Ulm.Paths.package cache pkg vsn)
      case eitherOutline of
        Left _ ->
          do  --Reporting.report key Reporting.DBroken
              return $ Left $ Just $ Exit.BD_BadBuild pkg vsn f

        Right (Outline.App _) ->
          do  --Reporting.report key Reporting.DBroken
              return $ Left $ Just $ Exit.BD_BadBuild pkg vsn f

        Right (Outline.Pkg (Outline.PkgOutline _ _ _ _ exposed deps _ _)) ->
          do  allDeps <- readMVar depsMVar
              -- because `deps` only contains a range of versions, we need to find a valid version number
              directDeps <- traverse readMVar (Map.intersection allDeps deps)
              putStrLn ("directDeps: " ++ show directDeps)
              case sequence directDeps of
                Left _ ->
                  do  --Reporting.report key Reporting.DBroken
                      return $ Left $ Nothing

                Right directArtifacts ->
                  do  let src = Ulm.Paths.package cache pkg   vsn </> "src"
                      let foreignDeps = gatherForeignInterfaces directArtifacts
                      let exposedDict = Map.fromKeys (\_ -> ()) (Outline.flattenExposed exposed)
                      docsStatus <- getDocsStatus cache pkg vsn
                      mvar <- newEmptyMVar
                      mvars <- Map.traverseWithKey (const . fork . crawlModule foreignDeps mvar pkg src docsStatus) exposedDict
                      putMVar mvar mvars
                      mapM_ readMVar mvars
                      maybeStatuses <- traverse readMVar =<< readMVar mvar
                      case sequence maybeStatuses of
                        Nothing ->
                          do  --Reporting.report key Reporting.DBroken
                              return $ Left $ Just $ Exit.BD_BadBuild pkg vsn f

                        Just statuses ->
                          do  rmvar <- newEmptyMVar
                              rmvars <- traverse (fork . compile pkg rmvar) statuses
                              putMVar rmvar rmvars
                              maybeResults <- traverse readMVar rmvars
                              case sequence maybeResults of
                                Nothing ->
                                  do  --Reporting.report key Reporting.DBroken
                                      return $ Left $ Just $ Exit.BD_BadBuild pkg vsn f

                                Just results ->
                                  let
                                    path = Ulm.Paths.package cache pkg vsn </> "artifacts.dat"
                                    ifaces = gatherInterfaces exposedDict results
                                    objects = gatherObjects results
                                    artifacts = Artifacts ifaces objects
                                    fingerprints = Set.insert f fs
                                  in
                                  do  writeDocs cache pkg vsn docsStatus results
                                      File.writeBinary path (ArtifactCache fingerprints artifacts)
                                      --Reporting.report key Reporting.DBuilt
                                      return (Right artifacts)



-- GATHER


gatherObjects :: Map.Map ModuleName.Raw Result -> Opt.GlobalGraph
gatherObjects results =
  Map.foldrWithKey addLocalGraph Opt.empty results


addLocalGraph :: ModuleName.Raw -> Result -> Opt.GlobalGraph -> Opt.GlobalGraph
addLocalGraph name status graph =
  case status of
    RLocal _ objs _ -> Opt.addLocalGraph objs graph
    RForeign _      -> graph
    RKernelLocal cs -> Opt.addKernel (Name.getKernel name) cs graph
    RKernelForeign  -> graph


gatherInterfaces :: Map.Map ModuleName.Raw () -> Map.Map ModuleName.Raw Result -> Map.Map ModuleName.Raw I.DependencyInterface
gatherInterfaces exposed artifacts =
  let
    onLeft  = Map.mapMissing (error "compiler bug manifesting in Elm.Details.gatherInterfaces")
    onRight = Map.mapMaybeMissing     (\_    iface -> toLocalInterface I.private iface)
    onBoth  = Map.zipWithMaybeMatched (\_ () iface -> toLocalInterface I.public  iface)
  in
  Map.merge onLeft onRight onBoth exposed artifacts


toLocalInterface :: (I.Interface -> a) -> Result -> Maybe a
toLocalInterface func result =
  case result of
    RLocal iface _ _ -> Just (func iface)
    RForeign _       -> Nothing
    RKernelLocal _   -> Nothing
    RKernelForeign   -> Nothing



-- GATHER FOREIGN INTERFACES


data ForeignInterface
  = ForeignAmbiguous
  | ForeignSpecific I.Interface


gatherForeignInterfaces :: Map.Map Pkg.Name Artifacts -> Map.Map ModuleName.Raw ForeignInterface
gatherForeignInterfaces directArtifacts =
    Map.map (OneOrMore.destruct finalize) $
      Map.foldrWithKey gather Map.empty directArtifacts
  where
    finalize :: I.Interface -> [I.Interface] -> ForeignInterface
    finalize i is =
      case is of
        [] -> ForeignSpecific i
        _:_ -> ForeignAmbiguous

    gather :: Pkg.Name -> Artifacts -> Map.Map ModuleName.Raw (OneOrMore.OneOrMore I.Interface) -> Map.Map ModuleName.Raw (OneOrMore.OneOrMore I.Interface)
    gather _ (Artifacts ifaces _) buckets =
      Map.unionWith OneOrMore.more buckets (Map.mapMaybe isPublic ifaces)

    isPublic :: I.DependencyInterface -> Maybe (OneOrMore.OneOrMore I.Interface)
    isPublic di =
      case di of
        I.Public iface  -> Just (OneOrMore.one iface)
        I.Private _ _ _ -> Nothing



-- CRAWL


type StatusDict =
  Map.Map ModuleName.Raw (MVar (Maybe Status))


data Status
  = SLocal DocsStatus (Map.Map ModuleName.Raw ()) Src.Module
  | SForeign I.Interface
  | SKernelLocal [Kernel.Chunk]
  | SKernelForeign


crawlModule :: Map.Map ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> DocsStatus -> ModuleName.Raw -> IO (Maybe Status)
crawlModule foreignDeps mvar pkg src docsStatus name =
  do  let path = src </> ModuleName.toFilePath name <.> "elm"
      exists <- File.exists path
      case Map.lookup name foreignDeps of
        Just ForeignAmbiguous ->
          return Nothing

        Just (ForeignSpecific iface) ->
          if exists
          then return Nothing
          else return (Just (SForeign iface))

        Nothing ->
          if exists then
            crawlFile foreignDeps mvar pkg src docsStatus name path

          else if Pkg.isKernel pkg && Name.isKernel name then
            crawlKernel foreignDeps mvar pkg src name

          else
            return Nothing


crawlFile :: Map.Map ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> DocsStatus -> ModuleName.Raw -> FilePath -> IO (Maybe Status)
crawlFile foreignDeps mvar pkg src docsStatus expectedName path =
  do  bytes <- File.readUtf8 path
      case Parse.fromByteString (Parse.Package pkg) bytes of
        Right modul@(Src.Module (Just (A.At _ actualName)) _ _ imports _ _ _ _ _) | expectedName == actualName ->
          do  deps <- crawlImports foreignDeps mvar pkg src imports
              return (Just (SLocal docsStatus deps modul))

        _ ->
          return Nothing


crawlImports :: Map.Map ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> [Src.Import] -> IO (Map.Map ModuleName.Raw ())
crawlImports foreignDeps mvar pkg src imports =
  do  statusDict <- takeMVar mvar
      let deps = Map.fromList (map (\i -> (Src.getImportName i, ())) imports)
      let news = Map.difference deps statusDict
      mvars <- Map.traverseWithKey (const . fork . crawlModule foreignDeps mvar pkg src DocsNotNeeded) news
      putMVar mvar (Map.union mvars statusDict)
      mapM_ readMVar mvars
      return deps


crawlKernel :: Map.Map ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> ModuleName.Raw -> IO (Maybe Status)
crawlKernel foreignDeps mvar pkg src name =
  do  let path = src </> ModuleName.toFilePath name <.> "js"
      exists <- File.exists path
      if exists
        then
          do  bytes <- File.readUtf8 path
              case Kernel.fromByteString pkg (Map.mapMaybe getDepHome foreignDeps) bytes of
                Nothing ->
                  return Nothing

                Just (Kernel.Content imports chunks) ->
                  do  _ <- crawlImports foreignDeps mvar pkg src imports
                      return (Just (SKernelLocal chunks))
        else
          return (Just SKernelForeign)


getDepHome :: ForeignInterface -> Maybe Pkg.Name
getDepHome fi =
  case fi of
    ForeignSpecific (I.Interface pkg _ _ _ _) -> Just pkg
    ForeignAmbiguous                          -> Nothing



-- COMPILE


data Result
  = RLocal !I.Interface !Opt.LocalGraph (Maybe Docs.Module)
  | RForeign I.Interface
  | RKernelLocal [Kernel.Chunk]
  | RKernelForeign


compile :: Pkg.Name -> MVar (Map.Map ModuleName.Raw (MVar (Maybe Result))) -> Status -> IO (Maybe Result)
compile pkg mvar status =
  case status of
    SLocal docsStatus deps modul ->
      do  resultsDict <- readMVar mvar
          maybeResults <- traverse readMVar (Map.intersection resultsDict deps)
          case sequence maybeResults of
            Nothing ->
              return Nothing

            Just results ->
              case Compile.compile pkg (Map.mapMaybe getInterface results) modul of
                Left _ ->
                  return Nothing

                Right (Compile.Artifacts canonical annotations objects) ->
                  let
                    ifaces = I.fromModule pkg canonical annotations
                    docs = makeDocs docsStatus canonical
                  in
                  return (Just (RLocal ifaces objects docs))

    SForeign iface ->
      return (Just (RForeign iface))

    SKernelLocal chunks ->
      return (Just (RKernelLocal chunks))

    SKernelForeign ->
      return (Just RKernelForeign)


getInterface :: Result -> Maybe I.Interface
getInterface result =
  case result of
    RLocal iface _ _ -> Just iface
    RForeign iface   -> Just iface
    RKernelLocal _   -> Nothing
    RKernelForeign   -> Nothing



-- MAKE DOCS


data DocsStatus
  = DocsNeeded
  | DocsNotNeeded


getDocsStatus :: Ulm.Paths.PackageCache -> Pkg.Name -> V.Version -> IO DocsStatus
getDocsStatus cache pkg vsn =
  do  exists <- File.exists (Ulm.Paths.package cache pkg vsn </> "docs.json")
      if exists
        then return DocsNotNeeded
        else return DocsNeeded


makeDocs :: DocsStatus -> Can.Module -> Maybe Docs.Module
makeDocs status modul =
  case status of
    DocsNeeded ->
      case Docs.fromModule modul of
        Right docs -> Just docs
        Left _     -> Nothing

    DocsNotNeeded ->
      Nothing


writeDocs :: Ulm.Paths.PackageCache -> Pkg.Name -> V.Version -> DocsStatus -> Map.Map ModuleName.Raw Result -> IO ()
writeDocs cache pkg vsn status results =
  case status of
    DocsNeeded ->
      E.writeUgly (Ulm.Paths.package cache pkg vsn </> "docs.json") $
        Docs.encode $ Map.mapMaybe toDocs results

    DocsNotNeeded ->
      return ()


toDocs :: Result -> Maybe Docs.Module
toDocs result =
  case result of
    RLocal _ _ docs -> docs
    RForeign _      -> Nothing
    RKernelLocal _  -> Nothing
    RKernelForeign  -> Nothing



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
