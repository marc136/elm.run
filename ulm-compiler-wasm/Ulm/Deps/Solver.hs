{-# LANGUAGE BangPatterns #-}
module Ulm.Deps.Solver
  ( Solver
  , Result(..)
  -- , Connection(..)
  --
  , Details(..)
  , verify
  --
  -- , AppSolution(..)
  -- , addToApp
  --
  -- , Env(..)
  -- , initEnv
  )
  where

import Control.Monad (foldM)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, readMVar)
import qualified Data.Map as Map
import Data.Map ((!))
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Ulm.Deps.Registry as Registry
-- import qualified Deps.Website as Website
import qualified Elm.Constraint as C
import qualified Elm.Package as Pkg
import qualified Elm.Outline as Outline
import qualified Elm.Version as V
import qualified File
-- import qualified Http
import qualified Json.Decode as D
import qualified Ulm.Reporting.Exit as Exit
import qualified Ulm.Paths
import Debug.Trace (traceShow, traceShowId, traceM, trace)
import ToStringHelper

-- SOLVER


newtype Solver a =
  Solver
  (
    forall b.
      State
      -> (State -> a -> (State -> IO b) -> IO b)
      -> (State -> IO b)
      -> (Exit.Solver -> IO b)
      -> IO b
  )


data State =
  State
    { _cache :: Ulm.Paths.PackageCache
    , _registry :: Registry.Registry
    , _constraints :: Map.Map (Pkg.Name, V.Version) Constraints
    }

instance Show State where
  show (State cache registry constraints) = "<State>"


data Constraints =
  Constraints
    { _elm :: C.Constraint
    , _deps :: Map.Map Pkg.Name C.Constraint
    }
    deriving (Show)



-- RESULT


data Result a
  = Ok a
  -- | NoSolution
  | NoOfflineSolution
  | Err Exit.Solver



-- VERIFY -- used by Elm.Details


data Details =
  Details V.Version (Map.Map Pkg.Name C.Constraint)


verify :: Ulm.Paths.PackageCache -> Registry.Registry -> Map.Map Pkg.Name C.Constraint -> IO (Result (Map.Map Pkg.Name Details))
verify cache registry constraints =
  case try constraints of
    Solver solver ->
      solver (State cache registry Map.empty)
        (\s a _ -> return $ Ok (Map.mapWithKey (addDeps s) a))
        (\(State cache registry map) -> 
          do  putStrLn ("registry" ++ show registry)
              return $ (traceShow "No solution found" NoOfflineSolution)
        )
        (\e     -> return $ Err e)


addDeps :: State -> Pkg.Name -> V.Version -> Details
addDeps (State _ _ constraints) name vsn =
  case Map.lookup (name, vsn) constraints of
    Just (Constraints _ deps) -> Details vsn deps
    Nothing                   -> error "compiler bug manifesting in Ulm.Deps.Solver.addDeps"



-- TRY


try :: Map.Map Pkg.Name C.Constraint -> Solver (Map.Map Pkg.Name V.Version)
try constraints =
  exploreGoals (Goals constraints Map.empty)



-- EXPLORE GOALS


data Goals =
  Goals
    { _pending :: Map.Map Pkg.Name C.Constraint
    , _solved :: Map.Map Pkg.Name V.Version
    }

instance Show Goals where
  show (Goals _pending _solved) = 
    "Goals pending: " ++ show (Map.size _pending)  ++ "solvd: " ++ show (Map.size _solved)  


exploreGoals :: Goals -> Solver (Map.Map Pkg.Name V.Version)
exploreGoals (Goals pending solved) =
  case Map.minViewWithKey pending of
    Nothing ->
      return solved

    Just ((name, constraint), otherPending) ->
      do  let goals1 = Goals otherPending solved
          let addVsn = addVersion goals1 name
          (v,vs) <- getRelevantVersions name constraint 
          goals2 <- oneOf (addVsn v) (map addVsn vs)
          exploreGoals goals2


addVersion :: Goals -> Pkg.Name -> V.Version -> Solver Goals
addVersion (Goals pending solved) name version =
  do  (Constraints elm deps) <- getConstraints name version
      if C.goodElm elm
        then
          do  newPending <- foldM (addConstraint solved) pending (Map.toList deps)
              return (Goals newPending (Map.insert name version solved))
        else
          backtrack


addConstraint :: Map.Map Pkg.Name V.Version -> Map.Map Pkg.Name C.Constraint -> (Pkg.Name, C.Constraint) -> Solver (Map.Map Pkg.Name C.Constraint)
addConstraint solved unsolved (name, newConstraint) =
  case Map.lookup name solved of
    Just version ->
      if C.satisfies newConstraint version
      then return unsolved
      else backtrack

    Nothing ->
      case Map.lookup name unsolved of
        Nothing ->
          return $ Map.insert name newConstraint unsolved

        Just oldConstraint ->
          case C.intersect oldConstraint newConstraint of
            Nothing ->
              backtrack

            Just mergedConstraint ->
              if oldConstraint == mergedConstraint
              then return unsolved
              else return (Map.insert name mergedConstraint unsolved)



-- GET RELEVANT VERSIONS


getRelevantVersions :: Pkg.Name -> C.Constraint -> Solver (V.Version, [V.Version])
getRelevantVersions name constraint =
  Solver $ \state@(State _ registry _) ok back _ ->
    case Registry.getVersions name registry of
      Just (Registry.KnownVersions newest previous) ->
        case filter (C.satisfies constraint) (newest:previous) of
          []   -> back state
          v:vs -> ok state (v,vs) back

      Nothing ->
        back state



-- GET CONSTRAINTS


getConstraints :: Pkg.Name -> V.Version -> Solver Constraints
getConstraints pkg vsn =
  Solver $ \state@(State cache registry cDict) ok back err ->
    do  let key = (pkg, vsn)
        case Map.lookup key cDict of
          Just cs ->
            ok state cs back

          Nothing ->
            do  let toNewState cs = State cache registry (Map.insert key cs cDict)
                let home = Ulm.Paths.package cache pkg vsn
                let path = home </> "elm.json"
                -- putStrLn ("trying to load " ++ path)
                outlineExists <- File.exists path
                if outlineExists
                  then
                    do  bytes <- File.readUtf8 path
                        case D.fromByteString constraintsDecoder bytes of
                          Right cs ->
                            do  srcExists <- Dir.doesDirectoryExist (Ulm.Paths.package cache pkg vsn </> "src")
                                if srcExists
                                  then ok (toNewState cs) cs back
                                  else back state

                          Left  _  ->
                            do  File.remove path
                                err (Exit.SolverBadCacheData pkg vsn)
                  else
                    back state


constraintsDecoder :: D.Decoder () Constraints
constraintsDecoder =
  do  outline <- D.mapError (const ()) Outline.decoder
      case outline of
        Outline.Pkg (Outline.PkgOutline _ _ _ _ _ deps _ elmConstraint) ->
          return (Constraints elmConstraint deps)

        Outline.App _ ->
          D.failure ()



-- INSTANCES


instance Functor Solver where
  fmap func (Solver solver) =
    Solver $ \state ok back err ->
      let
        okA stateA arg backA = ok stateA (func arg) backA
      in
      solver state okA back err


instance Applicative Solver where
  pure a =
    Solver $ \state ok back _ -> ok state a back

  (<*>) (Solver solverFunc) (Solver solverArg) =
    Solver $ \state ok back err ->
      let
        okF stateF func backF =
          let
            okA stateA arg backA = ok stateA (func arg) backA
          in
          solverArg stateF okA backF err
      in
      solverFunc state okF back err


instance Monad Solver where
  return a =
    Solver $ \state ok back _ -> ok state a back

  (>>=) (Solver solverA) callback =
    Solver $ \state ok back err ->
      let
        okA stateA a backA =
          case callback a of
            Solver solverB -> solverB stateA ok backA err
      in
      solverA state okA back err

traceMonad :: (Show a, Monad m) => a -> m a
traceMonad x = trace ("test: " ++ show x) (return x)

oneOf :: Solver a -> [Solver a] -> Solver a
oneOf solver@(Solver solverHead) solvers =
  case solvers of
    [] ->
      solver

    s:ss ->
      Solver $ \state0 ok back err ->
        let
          tryTail state1 =
            let
              (Solver solverTail) = oneOf s ss
            in
            solverTail state1 ok back err
        in
        solverHead state0 ok tryTail err


backtrack :: Solver a
backtrack =
  traceShow "backtrack"  $ Solver $ \state _ back _ -> back state
