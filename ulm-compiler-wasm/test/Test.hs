module Main (main) where

import Test.Tasty
-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord
import Ulm.Repl.Lib qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
    [ testCase "List comparison (different length)" $
        [1, 2, 3, Ulm.Repl.Lib.horst] `compare` [1,2] @?= GT

    -- the following test does not hold
    , testCase "List comparison (same length)" $
        [1, 2, 3] `compare` [1,2,2] @?= LT
    ]
