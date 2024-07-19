module Ulm.Paths
  ( details,
    interfaces,
    objects,
    -- prepublishDir,
    elmi,
    elmo,
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

import Elm.ModuleName qualified as ModuleName
import Elm.Package qualified as Pkg
import Elm.Version qualified as V
import System.Directory qualified as Dir
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

elmi :: FilePath -> ModuleName.Raw -> FilePath
elmi root name =
  toArtifactPath root name "elmi"

elmo :: FilePath -> ModuleName.Raw -> FilePath
elmo root name =
  toArtifactPath root name "elmo"

toArtifactPath :: FilePath -> ModuleName.Raw -> String -> FilePath
toArtifactPath root name ext =
  stuff root </> ModuleName.toHyphenPath name <.> ext

-- ROOT

-- LOCKS

-- withRootLock :: FilePath -> IO a -> IO a
-- withRootLock root work =
--   do  let dir = stuff root
--       Dir.createDirectoryIfMissing True dir
--       Lock.withFileLock (dir </> "lock") Lock.Exclusive (\_ -> work)
--
-- Linker fails on wasm32-wasi
-- ```
-- [112 of 112] Linking dist-newstyle/build/wasm32-wasi/ghc-9.11.20240525/ulm-0.19.1.0/x/ulm/opt/build/ulm/ulm.wasm [Objects changed]
-- wasm-ld: error: /home/marc/.ghc-wasm/.cabal/store/ghc-9.11.20240525/filelock-0.1.1.7-17d96421d03402e7a793d9625469109e0b472b30ba8a653d5976f2d56cbbd463/lib/libHSfilelock-0.1.1.7-17d96421d03402e7a793d9625469109e0b472b30ba8a653d5976f2d56cbbd463.a(Flock.o): undefined symbol: flock
-- wasm32-wasi-clang: error: linker command failed with exit code 1 (use -v to see invocation)
-- wasm32-wasi-ghc-9.11.20240525: `wasm32-wasi-clang' failed in phase `Linker'. (Exit code: 1)
-- Error: [Cabal-7125]
-- Failed to build exe:ulm from ulm-0.19.1.0.
-- ```
--
-- withRegistryLock :: PackageCache -> IO a -> IO a
-- withRegistryLock (PackageCache dir) work =
--   Lock.withFileLock (dir </> "lock") Lock.Exclusive (\_ -> work)

-- PACKAGE CACHES

newtype PackageCache = PackageCache FilePath

getPackageCache :: IO PackageCache
getPackageCache =
  do
    Dir.createDirectoryIfMissing True packageCacheDir
    pure $ PackageCache packageCacheDir

packageCacheDir :: FilePath
packageCacheDir =
  -- TODO instead bring my paths to the Ulm namespace
  getElmHome </> compilerVersion </> "packages"

registry :: PackageCache -> FilePath
registry (PackageCache dir) =
  dir </> "registry.dat"

package :: PackageCache -> Pkg.Name -> V.Version -> FilePath
package (PackageCache dir) name version =
  dir </> Pkg.toFilePath name </> V.toChars version

getElmHome :: FilePath
getElmHome =
  "/elm-home"
