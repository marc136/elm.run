module Ulm where

-- See https://gitlab.haskell.org/ghc/ghc/-/commit/317a915bc46fee2c824d595b0d618057bf7fbbf1#82b5a034883a3ede9540d6423738da627660f860
import GHC.Wasm.Prim

main :: IO ()
main = mempty

foreign export javascript "buildArtifacts" buildArtifacts :: IO ()
buildArtifacts = putStrLn "TODO buildArtifacts"

-- The main compilation logic is the same as 
-- `../worker/src/Endpoint/Compile.hs``

foreign export javascript "compile" compile :: IO ()
compile = putStrLn "TODO compile"
