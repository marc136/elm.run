module Main (main) where

-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC

import Data.ByteString.Builder qualified
import Data.ByteString.Lazy qualified
import Data.ByteString.Lazy.Char8 qualified
import Data.List
import Data.Ord
import System.FilePath qualified
import System.IO qualified
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden qualified as Golden
import Test.Tasty.HUnit
import Ulm.ReadArtifacts qualified
import Ulm.Repl qualified
import Ulm.Repl.Lib qualified

main :: IO ()
-- main = defaultMain tests
main = defaultMain =<< repl

-- tests :: TestTree
-- tests = testGroup "Tests" [unitTests, repl]

-- unitTests =
--   testGroup
--     "Unit tests"
--     [ testCase "List comparison (different length)" $
--         [1, 2, 3, Ulm.Repl.Lib.horst] `compare` [1, 2] @?= GT,
--       -- the following test does not hold
--       testCase "List comparison (same length)" $
--         [1, 2, 3] `compare` [1, 2, 2] @?= LT
--     ]

repl :: IO TestTree
repl = do
  artifacts <- Ulm.ReadArtifacts.getHardcodedArtifacts
  files <- Golden.findByExtension [".elm"] "."
  pure $
    testGroup "elm repl golden test" $
      [ Golden.goldenVsString
          (System.FilePath.takeBaseName aFile) -- test name
          aFile -- golden file path
          ((repl1 artifacts Ulm.Repl.initialState) <$> Data.ByteString.Lazy.readFile aFile)
        | aFile <- files,
          let aFile = System.FilePath.replaceExtension aFile ".json"
      ]

repl1 :: Ulm.ReadArtifacts.ArtifactsForWasm -> Ulm.Repl.ReplState -> Data.ByteString.Lazy.ByteString -> Data.ByteString.Lazy.ByteString
repl1 artifacts state byteString =
  let string = Data.ByteString.Lazy.Char8.unpack byteString
      outcome = Ulm.Repl.toOutcome artifacts state string
   in Ulm.Repl.outcomeToJsonString outcome