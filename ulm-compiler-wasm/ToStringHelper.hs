{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- copied from https://github.com/mdgriffith/elm-dev/blob/9434c0aa09e0bcb9c6d90d17e6abd125214a6c08/ext-dev/StandalonInstances.hs

module ToStringHelper where

-- import qualified Build
import Data.ByteString.Builder as B
import Data.ByteString.Lazy as BSL
import Data.String (IsString, fromString)
import qualified Data.Map as Map
import qualified GHC.IORef
import qualified Control.Concurrent.MVar as MVar
import qualified Debug.Trace


-- Elm modules

import qualified AST.Canonical
import qualified AST.Optimized
import qualified AST.Source
import qualified AST.Utils.Binop
import qualified AST.Utils.Shader

import qualified Data.Index
import qualified Data.Name
import qualified Data.NonEmptyList
import qualified Data.OneOrMore
import qualified Data.Utf8 as Utf8

import qualified Elm.Constraint
-- import qualified Elm.Details
import qualified Elm.Float
import qualified Elm.Interface
import qualified Elm.Kernel
import qualified Elm.Licenses
import qualified Elm.ModuleName
import qualified Elm.Outline
import qualified Elm.Package
import qualified Elm.String
import qualified Elm.Version

import qualified Json.Decode
import qualified Json.Encode
import qualified Json.String

import qualified Parse.Primitives

import qualified Reporting.Annotation
import qualified Reporting.Error.Canonicalize

-- Ulm modules

-- import qualified Ulm.Details
import qualified Ulm.Deps.Registry
-- import qualified Ulm.Deps.Solver
import qualified Ulm.Reporting
import qualified Ulm.Reporting.Error
import qualified Ulm.Reporting.Exit

import CompileToStringHelper ()

-- Show

deriving instance Show Ulm.Reporting.Exit.Details
deriving instance Show Ulm.Reporting.Exit.DetailsBadDep
deriving instance Show Ulm.Reporting.Exit.Outline
deriving instance Show Ulm.Reporting.Exit.OutlineProblem
deriving instance Show Ulm.Reporting.Exit.PackageProblem
deriving instance Show Ulm.Reporting.Exit.Solver


-- Helpers


quoted :: String -> String
quoted s = "\"" ++ s ++ "\""


showMapQualified :: (Show k, Show a) => Map.Map k a -> String
showMapQualified m =
  "Map.fromList " ++ show (Map.toList m)


{- General debugger you can put anywhere -}
debugLog label a =
  Debug.Trace.trace (label ++ ": " ++ show a) a
