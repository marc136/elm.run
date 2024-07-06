module Ulm.Deps.Registry 
  ( Registry(..)
  , KnownVersions(..)
  , read
  -- , fetch
  -- , update
  -- , latest
  , getVersions
  -- , getVersions'
  )
where

-- clone of elm-compiler-wasm/builder/src/Deps/Registry.hs

import Prelude hiding (read)
import Control.Monad (liftM2)
import Data.Binary (Binary, get, put)
import qualified Data.List as List
import qualified Data.Map.Strict as Map

import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified File
import qualified Ulm.Reporting.Exit as Exit
import qualified Stuff



-- REGISTRY


data Registry =
  Registry
    { _count :: !Int
    , _versions :: !(Map.Map Pkg.Name KnownVersions)
    }

instance Show Registry where
  show (Registry {_count, _versions}) = "Registry (count: " ++ show _count ++ ")"

data KnownVersions =
  KnownVersions
    { _newest :: V.Version
    , _previous :: ![V.Version]
    }



-- READ


read :: Stuff.PackageCache -> IO (Maybe Registry)
read cache =
  File.readBinary (Stuff.registry cache)



-- GET VERSIONS


getVersions :: Pkg.Name -> Registry -> Maybe KnownVersions
getVersions name (Registry _ versions) =
  Map.lookup name versions



-- BINARY


instance Binary Registry where
  get = liftM2 Registry get get
  put (Registry a b) = put a >> put b


instance Binary KnownVersions where
  get = liftM2 KnownVersions get get
  put (KnownVersions a b) = put a >> put b
