{-# LANGUAGE OverloadedStrings #-}
module Ulm.Repl ( read, toOutcome, outcomeToJsonString, initialState, ReplState )
 where

import Prelude hiding (read)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as BS_UTF8 -- from utf8-string
import qualified Data.ByteString.Builder as B
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Map.Utils as Map
import qualified Data.Name as N
import qualified Data.NonEmptyList as NE

import System.IO.Unsafe  (unsafePerformIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified Compile
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified File
import qualified Generate.JavaScript as JS
import qualified Json.Encode
import Json.Encode ((==>))
import qualified Parse.Expression as PE
import qualified Parse.Declaration as PD
import qualified Parse.Module as PM
import qualified Parse.Primitives as P
import qualified Parse.Space as PS
import qualified Parse.Type as PT
import qualified Parse.Variable as PV
import Parse.Primitives (Row, Col)
import qualified Reporting.Annotation as A
import qualified Reporting.Error as Error
import qualified Reporting.Error.Import as Import
import qualified Reporting.Error.Syntax as ES
import qualified Reporting.Exit.Help as Help
import qualified Reporting.Render.Code as Code
import qualified Reporting.Render.Type.Localizer as L
import qualified Reporting.Report as Report

import qualified Ulm.ReadArtifacts

import ToStringHelper ()
import Debug.Trace (traceShow, traceShowId)

-- https://wiki.haskell.org/Top_level_mutable_state#The_problem
globalReplState :: IORef ReplState
{-# NOINLINE globalReplState #-}
globalReplState = unsafePerformIO (newIORef initialState)

-- globalArtifacts :: IORef Artifacts
-- {-# NOINLINE globalArtifacts #-}
-- -- TODO read artifacts.dat files
-- globalArtifacts = unsafePerformIO (newIORef initialState)


read :: String -> IO Json.Encode.Value
read str = do
  putStrLn $ "Evaluate `" ++ str ++ "`"
  state <- readIORef globalReplState
  -- artifacts <- readIORef globalArtifacts
  artifacts <- Ulm.ReadArtifacts.getHardcodedArtifacts
  let outcome = toOutcome artifacts state str
  print outcome
  -- writeIORef globalReplState (state + 1)
  -- wip "Repl.read"
  pure $ outcomeToJson outcome

outcomeToJson :: Outcome -> Json.Encode.Value
outcomeToJson outcome =
  case outcome of
    NewImport name  -> encode "new-import" (show name)
    NewType name    -> encode "new-type" (show name)
    NewWork code    -> encode "new-work" (show code)
    DoNothing       -> encode "do-nothing" "undefined"
    NoPorts         -> encode "no-ports" "undefined"
    Failure source err ->
      Help.reportToJson $
        Help.compilerReport "/" (Error.Module N.replModule "/repl" File.zeroTime source err) []


outcomeToJsonString :: Outcome -> LBS.LazyByteString
outcomeToJsonString outcome =
  B.toLazyByteString $ Json.Encode.encode $ outcomeToJson outcome


wip str =
  pure $
    Json.Encode.object
      [ "wip" ==> Json.Encode.chars str
      -- , "global" ==> Json.Encode.int int
      ]

encode name dat =
  Json.Encode.object
    [ "type" ==> Json.Encode.chars name
    , "data" ==> Json.Encode.chars dat
    ]



--
-- Code below is from elm-compiler/worker/src/Endpoint/Repl.hs
--



-- TO OUTCOME


data Outcome
  = NewImport N.Name
  | NewType N.Name
  | NewWork B.Builder
  --
  | DoNothing
  --
  | NoPorts
  | Failure BS.ByteString Error.Error
  deriving (Show)

toOutcome :: Ulm.ReadArtifacts.ArtifactsForWasm -> ReplState -> String -> Outcome
toOutcome artifacts state entry =
  case reverse (lines entry) of
    [] ->
      DoNothing

    prev : rev ->
      case traceShowId (categorize (Lines prev rev)) of
        Import name src -> compile artifacts state (ImportEntry name src)
        Type name src   -> compile artifacts state (TypeEntry name src)
        Decl name src   -> compile artifacts state (DeclEntry name src)
        Expr src        -> compile artifacts state (ExprEntry src)
        Port            -> NoPorts
        Skip            -> DoNothing
        Partial _       -> DoNothing

-- COMPILE


data EntryType
  = ImportEntry N.Name BS.ByteString
  | TypeEntry N.Name BS.ByteString
  | DeclEntry N.Name BS.ByteString
  | ExprEntry BS.ByteString


compile :: Ulm.ReadArtifacts.ArtifactsForWasm -> ReplState -> EntryType -> Outcome
compile (Ulm.ReadArtifacts.ArtifactsForWasm interfaces objects) state@(ReplState imports types decls) entryType =
  let
    -- source =
    --   case entryType of
    --     ImportEntry name src -> toByteString (state { _imports = Map.insert name (B.byteString src) imports }) OutputNothing
    --     TypeEntry   name src -> toByteString (state { _types = Map.insert name (B.byteString src) types }) OutputNothing
    --     DeclEntry   name src -> toByteString (state { _decls = Map.insert name (B.byteString src) decls }) (OutputDecl name)
    --     ExprEntry        src -> toByteString state (OutputExpr src)
    --
    (nextState, output) =
      case entryType of
        ImportEntry name src -> (state { _imports = Map.insert name (B.byteString src) imports }, OutputNothing)
        TypeEntry   name src -> (state { _types = Map.insert name (B.byteString src) types }, OutputNothing)
        DeclEntry   name src -> (state { _decls = Map.insert name (B.byteString src) decls }, OutputDecl name)
        ExprEntry        src -> (state, OutputExpr src)
    
    source = toByteString nextState output
  in
  case
    do  modul <- mapLeft Error.BadSyntax $ PM.fromByteString PM.Application source
        ifaces <- mapLeft Error.BadImports $ checkImports interfaces (Src._imports modul)
        artifacts <- Compile.compile Pkg.dummyName ifaces modul
        -- TODO store in globalReplState
        return ( modul, artifacts, objects )
  of
    Left err ->
      Failure source (traceShowId err)

    Right info ->
      case entryType of
        ImportEntry name _ -> NewImport name
        TypeEntry name _   -> NewType name
        DeclEntry name _   -> NewWork (toJavaScript info (Just name))
        ExprEntry _        -> NewWork (toJavaScript info Nothing)


toJavaScript :: (Src.Module, Compile.Artifacts, Opt.GlobalGraph) -> Maybe N.Name -> B.Builder
toJavaScript (modul, Compile.Artifacts canModule types locals, objects) maybeName =
  let
    localizer = L.fromModule modul
    graph = Opt.addLocalGraph locals objects
    home = Can._name canModule
    tipe = types Map.! maybe N.replValueToPrint id maybeName
  in
  -- JS.generateForReplWasm localizer graph home maybeName tipe
  -- TODO if I use this, then I can run it directly in a worker?
  JS.generateForReplEndpoint localizer graph home maybeName tipe


mapLeft :: (x -> y) -> Either x a -> Either y a
mapLeft func result =
  either (Left . func) Right result


checkImports :: Map.Map ModuleName.Raw I.Interface -> [Src.Import] -> Either (NE.List Import.Error) (Map.Map ModuleName.Raw I.Interface)
checkImports interfaces imports =
  let
    importDict = Map.fromValues Src.getImportName imports
    missing = Map.difference importDict interfaces
  in
  case Map.elems missing of
    [] ->
      Right (Map.intersection interfaces importDict)

    i:is ->
      let
        unimported =
          Map.keysSet (Map.difference interfaces importDict)

        toError (Src.Import (A.At region name) _ _) =
          Import.Error region name unimported Import.NotFound
      in
      Left (fmap toError (NE.List i is))



--
-- Code below is from elm-compiler/terminal/src/Repl.hs
--



-- READ


data Input
  = Import ModuleName.Raw BS.ByteString
  | Type N.Name BS.ByteString
  | Port
  | Decl N.Name BS.ByteString
  | Expr BS.ByteString
  | Skip
  | Partial String
  -- TODO add support for multiple declarations?
  deriving (Show)



-- LINES


data Lines =
  Lines
    { _prevLine :: String
    , _revLines :: [String]
    }


isBlank :: Lines -> Bool
isBlank (Lines prev rev) =
  null rev && all (==' ') prev


isSingleLine :: Lines -> Bool
isSingleLine (Lines _ rev) =
  null rev


endsWithBlankLine :: Lines -> Bool
endsWithBlankLine (Lines prev _) =
  all (==' ') prev


linesToByteString :: Lines -> BS_UTF8.ByteString
linesToByteString (Lines prev rev) =
  BS_UTF8.fromString (unlines (reverse (prev:rev)))


getFirstLine :: Lines -> String
getFirstLine (Lines x xs) =
  case xs of
    []   -> x
    y:ys -> getFirstLine (Lines y ys)



-- CATEGORIZE INPUT


categorize :: Lines -> Input
categorize lines
  | isBlank lines                    = Skip
  | startsWithKeyword "import" lines = attemptImport lines
  | otherwise                        = attemptDeclOrExpr lines


attemptImport :: Lines -> Input
attemptImport lines =
  let
    src = linesToByteString lines
    parser = P.specialize (\_ _ _ -> ()) PM.chompImport
  in
  case P.fromByteString parser (\_ _ -> ()) src of
    Right (Src.Import (A.At _ name) _ _) ->
      Import name src

    Left () ->
      Partial "import"


attemptDeclOrExpr :: Lines -> Input
attemptDeclOrExpr lines =
  let
    src = linesToByteString lines
    exprParser = P.specialize (toExprPosition src) PE.expression
    declParser = P.specialize (toDeclPosition src) PD.declaration
  in
  case P.fromByteString declParser (,) src of
    Right (decl, _) ->
      case decl of
        PD.Value _ (A.At _ (Src.Value (A.At _ name) _ _ _)) -> Decl name src
        PD.Union _ (A.At _ (Src.Union (A.At _ name) _ _  )) -> Type name src
        PD.Alias _ (A.At _ (Src.Alias (A.At _ name) _ _  )) -> Type name src
        PD.Port  _ _                                        -> Port

    Left declPosition
      | startsWithKeyword "type" lines ->
          traceShowId (Partial "type")

      | startsWithKeyword "port" lines ->
          Port

      | otherwise ->
          case P.fromByteString exprParser (,) src of
            Right _ ->
              Expr src

            Left exprPosition ->
              let _ = traceShowId "Left exprPosition" in
              if exprPosition >= declPosition then
                Expr src
              else
                Skip
                

startsWithKeyword :: [Char] -> Lines -> Bool
startsWithKeyword keyword lines =
  let
    line = getFirstLine lines
  in
  List.isPrefixOf keyword line &&
    case drop (length keyword) line of
      [] -> True
      c:_ -> not (Char.isAlphaNum c)



toExprPosition :: BS.ByteString -> ES.Expr -> Row -> Col -> (Row, Col)
toExprPosition src expr row col =
  let
    decl = ES.DeclDef N.replValueToPrint (ES.DeclDefBody expr row col) row col
  in
  toDeclPosition src decl row col


toDeclPosition :: BS.ByteString -> ES.Decl -> Row -> Col -> (Row, Col)
toDeclPosition src decl r c =
  let
    err = ES.ParseError (ES.Declarations decl r c)
    report = ES.toReport (Code.toSource src) err

    (Report.Report _ (A.Region (A.Position row col) _) _ _) = report
  in
  (row, col)


annotation :: P.Parser () N.Name
annotation =
  let
    err _ _ = ()
    err_ _ _ _ = ()
  in
  do  name <- PV.lower err
      PS.chompAndCheckIndent err_ err
      P.word1 0x3A {-:-} err
      PS.chompAndCheckIndent err_ err
      (_, _) <- P.specialize err_ PT.expression
      PS.checkFreshLine err
      return name



-- STATE


data ReplState =
  ReplState
    { _imports :: Map.Map N.Name B.Builder
    , _types :: Map.Map N.Name B.Builder
    , _decls :: Map.Map N.Name B.Builder
    }


initialState :: ReplState
initialState =
  ReplState Map.empty Map.empty Map.empty



-- EVAL



-- ATTEMPT EVAL


data Output
  = OutputNothing
  | OutputDecl N.Name
  | OutputExpr BS.ByteString



-- TO BYTESTRING


toByteString :: ReplState -> Output -> BS.ByteString
toByteString (ReplState imports types decls) output =
  LBS.toStrict $ B.toLazyByteString $
    mconcat
      [ "module ", N.toBuilder N.replModule, " exposing (..)\n"
      , Map.foldr mappend mempty imports
      , Map.foldr mappend mempty types
      , Map.foldr mappend mempty decls
      , outputToBuilder output
      ]


outputToBuilder :: Output -> B.Builder
outputToBuilder output =
  N.toBuilder N.replValueToPrint <> " =" <>
  case output of
    OutputNothing ->
      " ()\n"

    OutputDecl _ ->
      " ()\n"

    OutputExpr expr ->
      foldr (\line rest -> "\n  " <> B.byteString line <> rest) "\n" (BSC.lines expr)
