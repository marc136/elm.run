module Ulm.Build
  ( fromPaths
  -- ( fromExposed
  -- , fromPaths
  -- , fromRepl
  , Artifacts(..)
  , Root(..)
  , Module(..)
  , CachedInterface(..)
  -- , ReplArtifacts(..)
  -- , DocsGoal(..)
  , getRootNames
  )
  where

-- Copied from elm-compiler-wasm/builder/src/Build.hs

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (filterM, mapM_, sequence_)
import qualified Data.ByteString as B
import qualified Data.Char as Char
import qualified Data.Graph as Graph
import qualified Data.List as List
import qualified Data.Map.Utils as Map
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))
import qualified Data.Maybe as Maybe
import qualified Data.Name as Name
import qualified Data.NonEmptyList as NE
import qualified Data.OneOrMore as OneOrMore
import qualified Data.Set as Set
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import System.FilePath ((</>), (<.>))

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Optimized as Opt
-- import qualified Compile
import qualified Ulm.Details as Details
-- import qualified Elm.Docs as Docs
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified File
-- import qualified Json.Encode as E
import qualified Parse.Module as Parse
-- import qualified Reporting
-- import qualified Reporting.Annotation as A
import qualified Ulm.Reporting.Error as Error
-- import qualified Reporting.Error.Docs as EDocs
-- import qualified Reporting.Error.Syntax as Syntax
-- import qualified Reporting.Error.Import as Import
import qualified Ulm.Reporting.Exit as Exit
-- import qualified Reporting.Render.Type.Localizer as L
import qualified Ulm.Paths



-- ENVIRONMENT


data Env =
  Env
    { _key :: () --Reporting.BKey
    , _root :: FilePath
    , _project :: Parse.ProjectType
    , _srcDirs :: [AbsoluteSrcDir]
    , _buildID :: () --Details.BuildID
    , _locals :: Map.Map ModuleName.Raw Details.Local
    , _foreigns :: Map.Map ModuleName.Raw Details.Foreign
    }


makeEnv :: FilePath -> Details.Details -> IO Env
makeEnv root (Details.Details _ validOutline buildID locals foreigns _) =
  let key = () in
  case validOutline of
    Details.ValidApp givenSrcDirs ->
      do  srcDirs <- traverse (toAbsoluteSrcDir root) (NE.toList givenSrcDirs)
          return $ Env key root Parse.Application srcDirs buildID locals foreigns

    Details.ValidPkg pkg _ _ ->
      do  srcDir <- toAbsoluteSrcDir root (Outline.RelativeSrcDir "src")
          return $ Env key root (Parse.Package pkg) [srcDir] buildID locals foreigns



-- SOURCE DIRECTORY


newtype AbsoluteSrcDir =
  AbsoluteSrcDir FilePath


toAbsoluteSrcDir :: FilePath -> Outline.SrcDir -> IO AbsoluteSrcDir
toAbsoluteSrcDir root srcDir =
  AbsoluteSrcDir <$> Dir.canonicalizePath
    (
      case srcDir of
        Outline.AbsoluteSrcDir dir -> dir
        Outline.RelativeSrcDir dir -> root </> dir
    )


addRelative :: AbsoluteSrcDir -> FilePath -> FilePath
addRelative (AbsoluteSrcDir srcDir) path =
  srcDir </> path



-- FORK


-- PERF try using IORef semephore on file crawl phase?
-- described in Chapter 13 of Parallel and Concurrent Programming in Haskell by Simon Marlow
-- https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/ch13.html#sec_conc-par-overhead
--
fork :: IO a -> IO (MVar a)
fork work =
  do  mvar <- newEmptyMVar
      _ <- forkIO $ putMVar mvar =<< work
      return mvar



-- FROM EXPOSED



-- FROM PATHS


data Artifacts =
  Artifacts
    { _name :: Pkg.Name
    , _deps :: Dependencies
    , _roots :: NE.List Root
    , _modules :: [Module]
    }
  

data Module
  = Fresh ModuleName.Raw I.Interface Opt.LocalGraph
  | Cached ModuleName.Raw Bool (MVar CachedInterface)


type Dependencies =
  Map.Map ModuleName.Canonical I.DependencyInterface


fromPaths :: FilePath -> Details.Details -> NE.List FilePath -> IO (Either Exit.BuildProblem Artifacts)
fromPaths root details paths =
  do  env <- makeEnv root details

      elroots <- findRoots env paths
      case elroots of
        Left problem ->
          return (Left (Exit.BuildProjectProblem problem))

        Right lroots ->
          -- do  -- crawl
          --     dmvar <- Details.loadInterfaces root details
          --     smvar <- newMVar Map.empty
          --     srootMVars <- traverse (fork . crawlRoot env smvar) lroots
          --     sroots <- traverse readMVar srootMVars
          --     statuses <- traverse readMVar =<< readMVar smvar

          --     midpoint <- checkMidpointAndRoots dmvar statuses sroots
          --     case midpoint of
          --       Left problem ->
          --         return (Left (Exit.BuildProjectProblem problem))

          --       Right foreigns ->
          --         do  -- compile
          --             rmvar <- newEmptyMVar
          --             resultsMVars <- forkWithKey (checkModule env foreigns rmvar) statuses
          --             putMVar rmvar resultsMVars
          --             rrootMVars <- traverse (fork . checkRoot env resultsMVars) sroots
          --             results <- traverse readMVar resultsMVars
          --             writeDetails root details results
          --             toArtifacts env foreigns results <$> traverse readMVar rrootMVars
          pure (Left Exit.BuildProblem_TODO)



-- GET ROOT NAMES


getRootNames :: Artifacts -> NE.List ModuleName.Raw
getRootNames (Artifacts _ _ roots _) =
  fmap getRootName roots


getRootName :: Root -> ModuleName.Raw
getRootName root =
  case root of
    Inside  name     -> name
    Outside name _ _ -> name



-- CRAWL



-- CHECK MODULE


type ResultDict =
  Map.Map ModuleName.Raw (MVar Result)


data Result
  = RNew !Details.Local !I.Interface !Opt.LocalGraph !(Maybe ())
  | RSame !Details.Local !I.Interface !Opt.LocalGraph !(Maybe ())
  -- = RNew !Details.Local !I.Interface !Opt.LocalGraph !(Maybe Docs.Module)
  -- | RSame !Details.Local !I.Interface !Opt.LocalGraph !(Maybe Docs.Module)
  -- | RCached Bool Details.BuildID (MVar CachedInterface)
  -- | RNotFound Import.Problem
  | RProblem Error.Module
  | RBlocked
  | RForeign I.Interface
  | RKernel


data CachedInterface
  = Unneeded
  | Loaded I.Interface
  | Corrupted



-- CHECK DEPS



-- TO IMPORT ERROR



-- LOAD CACHED INTERFACES



-- CHECK PROJECT



-- CHECK FOR CYCLES



-- CHECK UNIQUE ROOTS



-- COMPILE MODULE



-- WRITE DETAILS


writeDetails :: FilePath -> Details.Details -> Map.Map ModuleName.Raw Result -> IO ()
writeDetails root (Details.Details time outline buildID locals foreigns extras) results =
  File.writeBinary (Ulm.Paths.details root) $
    Details.Details time outline buildID (Map.foldrWithKey addNewLocal locals results) foreigns extras


addNewLocal :: ModuleName.Raw -> Result -> Map.Map ModuleName.Raw Details.Local -> Map.Map ModuleName.Raw Details.Local
addNewLocal name result locals =
  case result of
    RNew  local _ _ _ -> Map.insert name local locals
    RSame local _ _ _ -> Map.insert name local locals
    -- RCached _ _ _     -> locals
    -- RNotFound _       -> locals
    RProblem _        -> locals
    RBlocked          -> locals
    RForeign _        -> locals
    RKernel           -> locals



-- FINALIZE EXPOSED


-- finalizeExposed :: FilePath -> DocsGoal docs -> NE.List ModuleName.Raw -> Map.Map ModuleName.Raw Result -> IO (Either Exit.BuildProblem docs)
-- finalizeExposed root docsGoal exposed results =
--   case foldr (addImportProblems results) [] (NE.toList exposed) of
--     p:ps ->
--       return $ Left $ Exit.BuildProjectProblem (Exit.BP_MissingExposed (NE.List p ps))

--     [] ->
--       case Map.foldr addErrors [] results of
--         []   -> Right <$> finalizeDocs docsGoal results
--         e:es -> return $ Left $ Exit.BuildBadModules root e es


addErrors :: Result -> [Error.Module] -> [Error.Module]
addErrors result errors =
  case result of
    RNew  _ _ _ _ ->   errors
    RSame _ _ _ _ ->   errors
    -- RCached _ _ _ ->   errors
    -- RNotFound _   ->   errors
    RProblem e    -> e:errors
    RBlocked      ->   errors
    RForeign _    ->   errors
    RKernel       ->   errors


-- addImportProblems :: Map.Map ModuleName.Raw Result -> ModuleName.Raw -> [(ModuleName.Raw, Import.Problem)] -> [(ModuleName.Raw, Import.Problem)]
-- addImportProblems results name problems =
--   case results ! name of
--     RNew  _ _ _ _ -> problems
--     RSame _ _ _ _ -> problems
--     RCached _ _ _ -> problems
--     RNotFound p   -> (name, p) : problems
--     RProblem _    -> problems
--     RBlocked      -> problems
--     RForeign _    -> problems
--     RKernel       -> problems



-- DOCS






--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
------ AFTER THIS, EVERYTHING IS ABOUT HANDLING MODULES GIVEN BY FILEPATH ------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------



-- FIND ROOT


data RootLocation
  = LInside ModuleName.Raw
  | LOutside FilePath


findRoots :: Env -> NE.List FilePath -> IO (Either Exit.BuildProjectProblem (NE.List RootLocation))
findRoots env paths =
  do  mvars <- traverse (fork . getRootInfo env) paths
      einfos <- traverse readMVar mvars
      return $ checkRoots =<< sequence einfos


checkRoots :: NE.List RootInfo -> Either Exit.BuildProjectProblem (NE.List RootLocation)
checkRoots infos =
  let
    toOneOrMore loc@(RootInfo absolute _ _) =
      (absolute, OneOrMore.one loc)

    fromOneOrMore loc locs =
      case locs of
        [] -> Right ()
        loc2:_ -> Left (Exit.BP_MainPathDuplicate (_relative loc) (_relative loc2))
  in
  fmap (\_ -> fmap _location infos) $
    traverse (OneOrMore.destruct fromOneOrMore) $
      Map.fromListWith OneOrMore.more $ map toOneOrMore (NE.toList infos)



-- ROOT INFO


data RootInfo =
  RootInfo
    { _absolute :: FilePath
    , _relative :: FilePath
    , _location :: RootLocation
    }


getRootInfo :: Env -> FilePath -> IO (Either Exit.BuildProjectProblem RootInfo)
getRootInfo env path =
  do  exists <- File.exists path
      if exists
        then getRootInfoHelp env path =<< Dir.canonicalizePath path
        else return (Left (Exit.BP_PathUnknown path))


getRootInfoHelp :: Env -> FilePath -> FilePath -> IO (Either Exit.BuildProjectProblem RootInfo)
getRootInfoHelp (Env _ _ _ srcDirs _ _ _) path absolutePath =
  let
    (dirs, file) = FP.splitFileName absolutePath
    (final, ext) = FP.splitExtension file
  in
  if ext /= ".elm"
  then
    return $ Left $ Exit.BP_WithBadExtension path
  else
    let
      absoluteSegments = FP.splitDirectories dirs ++ [final]
    in
    case Maybe.mapMaybe (isInsideSrcDirByPath absoluteSegments) srcDirs of
      [] ->
        return $ Right $ RootInfo absolutePath path (LOutside path)

      [(_, Right names)] ->
        do  let name = Name.fromChars (List.intercalate "." names)
            matchingDirs <- filterM (isInsideSrcDirByName names) srcDirs
            case matchingDirs of
              d1:d2:_ ->
                do  let p1 = addRelative d1 (FP.joinPath names <.> "elm")
                    let p2 = addRelative d2 (FP.joinPath names <.> "elm")
                    return $ Left $ Exit.BP_RootNameDuplicate name p1 p2

              _ ->
                return $ Right $ RootInfo absolutePath path (LInside name)

      [(s, Left names)] ->
        return $ Left $ Exit.BP_RootNameInvalid path s names

      (s1,_):(s2,_):_ ->
        return $ Left $ Exit.BP_WithAmbiguousSrcDir path s1 s2



isInsideSrcDirByName :: [String] -> AbsoluteSrcDir -> IO Bool
isInsideSrcDirByName names srcDir =
  File.exists (addRelative srcDir (FP.joinPath names <.> "elm"))


isInsideSrcDirByPath :: [String] -> AbsoluteSrcDir -> Maybe (FilePath, Either [String] [String])
isInsideSrcDirByPath segments (AbsoluteSrcDir srcDir) =
  case dropPrefix (FP.splitDirectories srcDir) segments of
    Nothing ->
      Nothing

    Just names ->
      if all isGoodName names
      then Just (srcDir, Right names)
      else Just (srcDir, Left names)


isGoodName :: [Char] -> Bool
isGoodName name =
  case name of
    [] ->
      False

    char:chars ->
      Char.isUpper char && all (\c -> Char.isAlphaNum c || c == '_') chars


-- INVARIANT: Dir.canonicalizePath has been run on both inputs
--
dropPrefix :: [FilePath] -> [FilePath] -> Maybe [FilePath]
dropPrefix roots paths =
  case roots of
    [] ->
      Just paths

    r:rs ->
      case paths of
        []   -> Nothing
        p:ps -> if r == p then dropPrefix rs ps else Nothing



-- CRAWL ROOTS


data RootStatus
  = SInside ModuleName.Raw
  | SOutsideOk Details.Local B.ByteString Src.Module
  | SOutsideErr Error.Module



-- CHECK ROOTS


data RootResult
  = RInside ModuleName.Raw
  | ROutsideOk ModuleName.Raw I.Interface Opt.LocalGraph
  | ROutsideErr Error.Module
  | ROutsideBlocked




-- TO ARTIFACTS


data Root
  = Inside ModuleName.Raw
  | Outside ModuleName.Raw I.Interface Opt.LocalGraph


toArtifacts :: Env -> Dependencies -> Map.Map ModuleName.Raw Result -> NE.List RootResult -> Either Exit.BuildProblem Artifacts
toArtifacts (Env _ root projectType _ _ _ _) foreigns results rootResults =
  case gatherProblemsOrMains results rootResults of
    Left (NE.List e es) ->
      Left (Exit.BuildBadModules root e es)

    Right roots ->
      -- Right $ Artifacts (projectTypeToPkg projectType) foreigns roots $
      --   Map.foldrWithKey addInside (foldr addOutside [] rootResults) results
      Left Exit.BuildProblem_TODO


gatherProblemsOrMains :: Map.Map ModuleName.Raw Result -> NE.List RootResult -> Either (NE.List Error.Module) (NE.List Root)
gatherProblemsOrMains results (NE.List rootResult rootResults) =
  let
    addResult result (es, roots) =
      case result of
        RInside n        -> (  es, Inside n      : roots)
        ROutsideOk n i o -> (  es, Outside n i o : roots)
        ROutsideErr e    -> (e:es,                 roots)
        ROutsideBlocked  -> (  es,                 roots)

    errors = Map.foldr addErrors [] results
  in
  case (rootResult, foldr addResult (errors, []) rootResults) of
    (RInside n       , (  [], ms)) -> Right (NE.List (Inside n) ms)
    (RInside _       , (e:es, _ )) -> Left  (NE.List e es)
    (ROutsideOk n i o, (  [], ms)) -> Right (NE.List (Outside n i o) ms)
    (ROutsideOk _ _ _, (e:es, _ )) -> Left  (NE.List e es)
    (ROutsideErr e   , (  es, _ )) -> Left  (NE.List e es)
    (ROutsideBlocked , (  [], _ )) -> error "seems like elm-stuff/ is corrupted"
    (ROutsideBlocked , (e:es, _ )) -> Left  (NE.List e es)


addInside :: ModuleName.Raw -> Result -> [Module] -> [Module]
addInside name result modules =
  case result of
    RNew  _ iface objs _ -> Fresh name iface objs : modules
    RSame _ iface objs _ -> Fresh name iface objs : modules
    -- RCached main _ mvar  -> Cached name main mvar : modules
    -- RNotFound _          -> error (badInside name)
    RProblem _           -> error (badInside name)
    RBlocked             -> error (badInside name)
    RForeign _           -> modules
    RKernel              -> modules


badInside :: ModuleName.Raw -> [Char]
badInside name =
  "Error from `" ++ Name.toChars name ++ "` should have been reported already."


addOutside :: RootResult -> [Module] -> [Module]
addOutside root modules =
  case root of
    RInside _                  -> modules
    ROutsideOk name iface objs -> Fresh name iface objs : modules
    ROutsideErr _              -> modules
    ROutsideBlocked            -> modules
