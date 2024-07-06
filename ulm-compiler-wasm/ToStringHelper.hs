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

-- Ulm modules

-- import qualified Ulm.Details
import qualified Ulm.Deps.Registry
-- import qualified Ulm.Deps.Solver
import qualified Ulm.Reporting
import qualified Ulm.Reporting.Error
import qualified Ulm.Reporting.Exit


-- Show

deriving instance Show AST.Source.Module
deriving instance Show AST.Source.Exposing
deriving instance Show AST.Source.Exposed
deriving instance Show AST.Source.Privacy
deriving instance Show AST.Source.Docs
deriving instance Show AST.Source.Comment
deriving instance Show AST.Source.Import
deriving instance Show AST.Source.Value
deriving instance Show AST.Source.Pattern_
deriving instance Show AST.Source.Expr_
deriving instance Show AST.Source.Type_
deriving instance Show AST.Source.Union
deriving instance Show AST.Source.Alias
deriving instance Show AST.Source.Infix
deriving instance Show AST.Source.Effects
deriving instance Show AST.Source.Port
deriving instance Show AST.Source.Manager
deriving instance Show AST.Source.VarType
deriving instance Show AST.Source.Def
deriving instance Show AST.Utils.Binop.Associativity
deriving instance Show AST.Utils.Binop.Precedence
instance Show AST.Utils.Shader.Source where
  show _ = "<shader source>"
deriving instance Show AST.Utils.Shader.Types
deriving instance Show AST.Utils.Shader.Type

instance Show Data.Name.Name where
  show = quoted . Data.Name.toChars
instance (Show a) => Show (Data.NonEmptyList.List a) where
  show = show . Data.NonEmptyList.toList
instance (Show a) => Show (Data.OneOrMore.OneOrMore a) where
  show = show . Data.OneOrMore.destruct (\v acc -> acc ++ [v])
instance Show Data.Index.ZeroBased where
  -- show (Data.Index.ZeroBased 0) = "Index.first"
  -- show (Data.Index.ZeroBased 1) = "Index.second"
  -- show (Data.Index.ZeroBased 2) = "Index.third"
  -- show (Data.Index.ZeroBased n) = "(Index.ZeroBased " ++ show n ++ ")"
  show _ = "Index"

instance Show Elm.Constraint.Constraint where show = Elm.Constraint.toChars
deriving instance Show Elm.Constraint.Error
instance Show Elm.Float.Float where
--   show = T.unpack . T.decodeUtf8 . BSL.toStrict . B.toLazyByteString . Elm.Float.toBuilder
    show _ = "<Float>"
instance Show Elm.Licenses.License where
  show _ = "\"<Elm.Licenses.License>\""
deriving instance Show Elm.Outline.Outline
deriving instance Show Elm.Outline.AppOutline
deriving instance Show Elm.Outline.SrcDir
deriving instance Show Elm.Outline.PkgOutline
deriving instance Show Elm.Outline.Exposed
instance Show Elm.Package.Name where
--   show (Elm.Package.Name author project) =
--     "Name " ++ (quoted . Utf8.toChars) author <> " " <> (quoted . Utf8.toChars) project
  show = Elm.Package.toChars
instance Show Elm.String.String where show = quoted . Elm.String.toChars
instance Show Elm.Version.Version where show = Elm.Version.toChars

deriving instance (Show a) => Show (Json.Decode.Error a)
deriving instance (Show a) => Show (Json.Decode.Problem a)
deriving instance Show Json.Decode.ParseError
deriving instance Show Json.Decode.StringProblem
deriving instance Show Json.Decode.DecodeExpectation
deriving instance Show Json.Encode.Value
instance Show Json.String.String where
  show x = "\"" <> Utf8.toChars x <> "\""

deriving instance Show Parse.Primitives.Snippet

deriving instance Show Reporting.Annotation.Region
deriving instance Show Reporting.Annotation.Position
-- deriving instance (Show a) => Show (Reporting.Annotation.Located a)
instance (Show a) => Show (Reporting.Annotation.Located a) where
  show (Reporting.Annotation.At region a) =
      "(a (" ++ show a ++ "))"


-- deriving instance Show Ulm.Details.Details
-- deriving instance Show Ulm.Details.ValidOutline
-- deriving instance Show Ulm.Details.Local
-- deriving instance Show Ulm.Details.Foreign

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
