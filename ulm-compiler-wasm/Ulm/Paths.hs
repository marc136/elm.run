module Ulm.Paths
  ( details,
    interfaces,
    objects,
    -- prepublishDir,
    -- elmi,
    -- elmo,
    -- temp,
    -- findRoot,
    -- , withRootLock
    -- , withRegistryLock
    PackageCache,
    getPackageCache,
    packageCacheDir,
    registry,
    package,
    -- , getReplCache
    getElmHome,
  )
where

-- copy of elm-compiler-wasm/builder/src/Stuff.hs

-- import Elm.ModuleName qualified as ModuleName
import Elm.Package qualified as Pkg
import Elm.Version qualified as V
-- import System.Directory qualified as Dir
-- import System.Environment qualified as Env
import System.FilePath ((<.>), (</>))
import System.FilePath qualified as FP

-- PATHS

stuff :: FilePath -> FilePath
stuff root =
  root </> "elm-stuff" </> compilerVersion

details :: FilePath -> FilePath
details root =
  stuff root </> "d.dat"

interfaces :: FilePath -> FilePath
interfaces root =
  stuff root </> "i.dat"

objects :: FilePath -> FilePath
objects root =
  stuff root </> "o.dat"

compilerVersion :: FilePath
compilerVersion =
  V.toChars V.compiler

-- ELMI and ELMO

-- ROOT

-- PACKAGE CACHES

newtype PackageCache = PackageCache FilePath

getPackageCache :: IO PackageCache
getPackageCache =
  pure $ PackageCache packageCacheDir

packageCacheDir :: FilePath
packageCacheDir =
  -- TODO instead bring my paths to the Ulm namespace
  "/packages"

registry :: PackageCache -> FilePath
registry (PackageCache dir) =
  dir </> "registry.dat"

package :: PackageCache -> Pkg.Name -> V.Version -> FilePath
package (PackageCache dir) name version =
  dir </> Pkg.toFilePath name </> V.toChars version

getElmHome :: IO FilePath
getElmHome =
  pure "/elm-home"
