module Ulm.Install
  ( install,
    installJson,
    Changes,
  )
where

import Data.ByteString.UTF8 qualified as BSU -- from utf8-string
import Data.Map qualified as Map
import Data.Map.Merge.Strict qualified as Map
import Debug.Trace
import Elm.Outline qualified
import Elm.Package qualified
import Elm.Version qualified
import Json.Decode qualified
import Json.Encode ((==>))
import Json.Encode qualified
import Ulm.Deps.Registry qualified
import Ulm.Deps.Solver qualified
import Ulm.Details qualified
import Ulm.Paths qualified
import Ulm.Reporting.Exit qualified

installJson :: String -> IO Json.Encode.Value
installJson packageName =
  -- I saw no good constructor, so I decode it from String
  let wrapped = BSU.fromString ("\"" ++ packageName ++ "\"")
   in case Json.Decode.fromByteString Elm.Package.decoder wrapped of
        Left err ->
          pure (toJson ErrUnknownPackage)
        Right pkg -> do
          result <- install pkg
          pure (toJson result)

toJson :: Changes Elm.Version.Version -> Json.Encode.Value
toJson changes =
  case changes of
    ErrNoOutline -> encodeErr "encodeErr"
    ErrNoRegistry -> encodeErr "ErrNoRegistry"
    OkAlreadyInstalled ->
      Json.Encode.object
        [ "ok" ==> Json.Encode.chars "AlreadyInstalled"
        ]
    PromoteTest _ ->
      Json.Encode.object
        [ "ok" ==> Json.Encode.chars "PromoteTest"
        ]
    PromoteIndirect _ ->
      Json.Encode.object
        [ "ok" ==> Json.Encode.chars "PromoteIndirect"
        ]
    Changes map _ ->
      Json.Encode.object
        [ "ok" ==> Json.Encode.chars "changes",
          "changes" ==> Json.Encode.dict Elm.Package.toJsonString (Json.Encode.chars . show) map
        ]
    ErrUnknownPackage -> encodeErr "ErrUnknownPackage"
    ErrNoSolution -> encodeErr "ErrNoSolution"
    NotImplemented string -> encodeErr ("NotImplemented: " ++ string)

encodeErr :: String -> Json.Encode.Value
encodeErr err =
  Json.Encode.object
    [ "err" ==> Json.Encode.chars err
    ]

-- Copied from `run` in elm-compiler-wasm/terminal/src/Install.hs
install :: Elm.Package.Name -> IO (Changes Elm.Version.Version)
install pkg = do
  let root = "/"
  outline <- Elm.Outline.read root
  cache <- Ulm.Paths.getPackageCache
  maybeRegistry <- Ulm.Deps.Registry.read cache
  case (outline, maybeRegistry) of
    (Left _, _) -> pure ErrNoOutline
    (_, Nothing) -> pure ErrNoRegistry
    (Right oldOutline, Just registry) ->
      case oldOutline of
        Elm.Outline.App outline -> do
          changes <- makeAppPlan cache registry pkg outline
          attemptChanges root registry oldOutline changes
        Elm.Outline.Pkg outline -> do
          -- do  changes <- makePkgPlan env pkg outline
          --     attemptChanges root env oldOutline C.toChars changes
          pure $ NotImplemented "makePkgPlan"

-- ATTEMPT CHANGES

data Changes vsn
  = ErrNoOutline
  | ErrNoRegistry
  | OkAlreadyInstalled
  | PromoteTest Elm.Outline.AppOutline
  | PromoteIndirect Elm.Outline.AppOutline
  | Changes (Map.Map Elm.Package.Name (Change vsn)) Elm.Outline.AppOutline
  | ErrUnknownPackage
  | ErrNoSolution
  | NotImplemented String

attemptChanges :: FilePath -> Ulm.Deps.Registry.Registry -> Elm.Outline.Outline -> Changes a -> IO (Changes a)
attemptChanges root registry oldOutline changes =
  let maybeOutline =
        case changes of
          PromoteIndirect newOutline -> Just newOutline
          PromoteTest newOutline -> Just newOutline
          Changes changeDict newOutline -> Just newOutline
          _ -> Nothing
   in case maybeOutline of
        Just newOutline -> do
          result <- attemptChangesHelp root registry oldOutline newOutline
          case result of
            Left _ ->
              pure (NotImplemented "attemptChangesHelp has failed")
            Right () ->
              pure changes
        _ ->
          pure changes

attemptChangesHelp :: FilePath -> Ulm.Deps.Registry.Registry -> Elm.Outline.Outline -> Elm.Outline.AppOutline -> IO (Either Ulm.Reporting.Exit.Details ())
attemptChangesHelp root registry oldOutline newOutline = do
  Elm.Outline.write root (Elm.Outline.App newOutline)
  result <- Ulm.Details.verifyInstall root registry (Elm.Outline.App newOutline)
  case result of
    Left exit -> do
      putStrLn "verifyInstall failed, will revert outline"
      Elm.Outline.write root oldOutline
      return (Left exit)
    Right () -> do
      putStrLn "Success!"
      return (Right ())

-- MAKE APP PLAN

-- makeAppPlan :: Solver.Env -> Elm.Package.Name -> Outline.AppOutline -> Task (Changes V.Version)
-- makeAppPlan (Solver.Env cache _ connection registry) pkg outline@(Outline.AppOutline _ _ direct indirect testDirect testIndirect) =
makeAppPlan :: Ulm.Paths.PackageCache -> Ulm.Deps.Registry.Registry -> Elm.Package.Name -> Elm.Outline.AppOutline -> IO (Changes Elm.Version.Version)
makeAppPlan cache registry pkg outline@(Elm.Outline.AppOutline _ _ direct indirect testDirect testIndirect) =
  if Map.member pkg direct
    then
      return OkAlreadyInstalled
    else
      -- is it already indirect?
      case Map.lookup pkg indirect of
        Just vsn ->
          return $
            PromoteIndirect $
              outline
                { Elm.Outline._app_deps_direct = Map.insert pkg vsn direct,
                  Elm.Outline._app_deps_indirect = Map.delete pkg indirect
                }
        Nothing ->
          -- is it already a test dependency?
          case Map.lookup pkg testDirect of
            Just vsn ->
              return $
                PromoteTest $
                  outline
                    { Elm.Outline._app_deps_direct = Map.insert pkg vsn direct,
                      Elm.Outline._app_test_direct = Map.delete pkg testDirect
                    }
            Nothing ->
              -- is it already an indirect test dependency?
              case Map.lookup pkg testIndirect of
                Just vsn ->
                  return $
                    PromoteTest $
                      outline
                        { Elm.Outline._app_deps_direct = Map.insert pkg vsn direct,
                          Elm.Outline._app_test_indirect = Map.delete pkg testIndirect
                        }
                Nothing ->
                  -- finally try to add it from scratch
                  case Ulm.Deps.Registry.getVersions pkg registry of
                    Nothing ->
                      pure ErrUnknownPackage
                    Just _ -> do
                      result <- Ulm.Deps.Solver.addToApp cache registry pkg outline
                      case traceShowId result of
                        Ulm.Deps.Solver.Ok (Ulm.Deps.Solver.AppSolution old new app) ->
                          pure (Changes (detectChanges old new) app)
                        Ulm.Deps.Solver.NoOfflineSolution ->
                          -- Task.throw (Exit.InstallNoOfflineAppSolution pkg)
                          pure ErrNoSolution
                        Ulm.Deps.Solver.Err exit ->
                          -- Task.throw (Exit.InstallHadSolverTrouble exit)
                          pure (NotImplemented "Handling for Ulm.Deps.Solver.Err")

-- CHANGES

data Change a
  = Insert a
  | Change a a
  | Remove a
  deriving (Show)

detectChanges :: (Eq a) => Map.Map Elm.Package.Name a -> Map.Map Elm.Package.Name a -> Map.Map Elm.Package.Name (Change a)
detectChanges old new =
  Map.merge
    (Map.mapMissing (\_ v -> Remove v))
    (Map.mapMissing (\_ v -> Insert v))
    (Map.zipWithMaybeMatched keepChange)
    old
    new

keepChange :: (Eq v) => k -> v -> v -> Maybe (Change v)
keepChange _ old new =
  if old == new
    then Nothing
    else Just (Change old new)

keepNew :: Change a -> Maybe a
keepNew change =
  case change of
    Insert a -> Just a
    Change _ a -> Just a
    Remove _ -> Nothing
