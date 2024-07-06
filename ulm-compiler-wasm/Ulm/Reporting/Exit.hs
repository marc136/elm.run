{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings #-}
module Ulm.Reporting.Exit
  ( Make(..)
  --
  , Solver(..)
  , Outline (..)
  , OutlineProblem(..)
  , Details(..)
  , DetailsBadDep(..)
  , PackageProblem(..)
  -- , RegistryProblem(..)
  , BuildProblem(..)
  , BuildProjectProblem(..)
  -- , DocsProblem(..)
  , Generate(..)
  --
  -- , toString
  -- , toStderr
  -- , toJson
  )
  where

-- extracted from /elm-compiler-wasm/builder/src/Reporting/Exit.hs


import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS_UTF8
import qualified Data.List as List
import qualified Data.Map as Map
-- import qualified Data.Name as N
-- import qualified Data.NonEmptyList as NE
-- -- import qualified Network.HTTP.Client as HTTP
-- -- import qualified Network.HTTP.Types.Header as HTTP
-- -- import qualified Network.HTTP.Types.Status as HTTP
-- import qualified System.FilePath as FP
-- import System.FilePath ((</>), (<.>))

import qualified Elm.Constraint as C
import qualified Elm.Magnitude as M
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
-- import qualified File
-- import qualified Http
import qualified Json.Decode as Decode
-- import qualified Json.Encode as Encode
import qualified Json.String as Json
import Parse.Primitives (Row, Col)
-- import qualified Reporting.Annotation as A
-- import Reporting.Doc ((<>))
-- import qualified Reporting.Doc as D
-- import qualified Reporting.Error.Import as Import
-- import qualified Reporting.Error.Json as Json
-- import qualified Reporting.Exit.Help as Help
import qualified Ulm.Reporting.Error as Error
-- import qualified Reporting.Render.Code as Code


-- SOLVER


data Solver
  = SolverBadCacheData Pkg.Name V.Version
  | SolverBadHttpData Pkg.Name V.Version String
  -- | SolverBadHttp Pkg.Name V.Version Http.Error


-- OUTLINE


data Outline
  = OutlineHasBadStructure (Decode.Error OutlineProblem)
  | OutlineHasMissingSrcDirs FilePath [FilePath]
  | OutlineHasDuplicateSrcDirs FilePath FilePath FilePath
  | OutlineNoPkgCore
  | OutlineNoAppCore
  | OutlineNoAppJson


data OutlineProblem
  = OP_BadType
  | OP_BadPkgName Row Col
  | OP_BadVersion Row Col
  | OP_BadConstraint C.Error
  | OP_BadModuleName Row Col
  | OP_BadModuleHeaderTooLong
  | OP_BadDependencyName Row Col
  | OP_BadLicense Json.String [Json.String]
  | OP_BadSummaryTooLong
  | OP_NoSrcDirs



-- DETAILS


data Details
  = DetailsNoSolution
  | DetailsNoOfflineSolution
  | DetailsSolverProblem Solver
  | DetailsBadElmInPkg C.Constraint
  | DetailsBadElmInAppOutline V.Version
  | DetailsHandEditedDependencies
  | DetailsBadOutline Outline
  -- | DetailsCannotGetRegistry RegistryProblem
  | DetailsNoRegistryCache
  | DetailsBadDeps FilePath [DetailsBadDep]


data DetailsBadDep
  = BD_BadDownload Pkg.Name V.Version PackageProblem
  | BD_BadBuild Pkg.Name V.Version (Map.Map Pkg.Name V.Version)
  | BD_MARC_TODO_BUILD



-- PACKAGE PROBLEM


data PackageProblem
  -- = PP_BadEndpointRequest Http.Error
  -- | PP_BadEndpointContent String
  -- | PP_BadArchiveRequest Http.Error
  -- | PP_BadArchiveContent String
  -- | PP_BadArchiveHash String String String
  = PP_DirectoryDoesNotExist String



-- REGISTRY PROBLEM


data RegistryProblem
  -- = RP_Http Http.Error
  -- | RP_Data String BS.ByteString
  = RP_Data String BS.ByteString



-- MAKE


data Make
  = MakeNoOutline
  | MakeCannotOptimizeAndDebug
  | MakeBadDetails Details
  | MakeAppNeedsFileNames
  | MakePkgNeedsExposing
  | MakeMultipleFilesIntoHtml
  | MakeNoMain
  | MakeNonMainFilesIntoJavaScript ModuleName.Raw [ModuleName.Raw]
  | MakeCannotBuild BuildProblem
  | MakeBadGenerate Generate



-- BUILD PROBLEM


data BuildProblem
  = BuildBadModules FilePath Error.Module [Error.Module]
  | BuildProjectProblem BuildProjectProblem
  | BuildProblem_TODO


data BuildProjectProblem
  = BP_PathUnknown FilePath
  | BP_WithBadExtension FilePath
  | BP_WithAmbiguousSrcDir FilePath FilePath FilePath
  | BP_MainPathDuplicate FilePath FilePath
  | BP_RootNameDuplicate ModuleName.Raw FilePath FilePath
  | BP_RootNameInvalid FilePath FilePath [String]
  | BP_CannotLoadDependencies
  | BP_Cycle ModuleName.Raw [ModuleName.Raw]
  -- | BP_MissingExposed (NE.List (ModuleName.Raw, Import.Problem))



-- GENERATE


data Generate
  = GenerateCannotLoadArtifacts
  | GenerateCannotOptimizeDebugValues ModuleName.Raw [ModuleName.Raw]
  | Generate_TODO


-- REPL


data Repl
  = ReplBadDetails Details
  | ReplBadInput BS.ByteString Error.Error
  | ReplBadLocalDeps FilePath Error.Module [Error.Module]
  | ReplProjectProblem BuildProjectProblem
  | ReplBadGenerate Generate
  | ReplBadCache
  | ReplBlocked


