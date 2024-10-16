module UlmEditor exposing (main)

import Browser
import Data.Problem
import Dict exposing (Dict)
import Elm.Error
import Elm.Package
import Elm.Version
import Examples exposing (Example)
import Heroicons.Solid as Icon
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Extra
import Html.Keyed
import InteropDefinitions
import InteropPorts
import Json.Decode
import List.Extra
import Maybe.Extra
import Theme exposing (Theme)
import TsJson.Decode



-- MAIN


main : Program Json.Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = \msg model -> update msg model |> Tuple.mapSecond performEffects
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = InitFailure Json.Decode.Error
    | Editor EditorModel


type alias EditorModel =
    { file : String
    , isCompiling : Bool
    , lastCompilation : LastCompilation
    , visibleProgram : Maybe ObjectUrl
    , outputView : OutputView
    , outputPreference : OutputPreference
    , theme : Theme
    , installedPackages : List InstalledPackage
    , knownPackages : List KnownPackage
    , showPackageList : Bool
    , searchForPackages : String
    }


type LastCompilation
    = NotStarted
    | Success ObjectUrl
    | Failed (List Data.Problem.Problem)


type alias ObjectUrl =
    String


type OutputPreference
    = PreferProblems
    | PreferProgram


type alias InstalledPackage =
    ( Elm.Package.Name, InstalledPackageState )


type InstalledPackageState
    = InstallingPackage
    | InstalledPackage Elm.Version.Version
    | RemovingPackage
    | InstallingPackageFailed


type alias KnownPackage =
    ( Elm.Package.Name, Elm.Version.Version )



-- INIT


init : Json.Decode.Value -> ( Model, Cmd Msg )
init json =
    case InteropPorts.decodeFlags json of
        Ok flags ->
            ( Editor
                { file = flags.file
                , isCompiling = False
                , lastCompilation = NotStarted
                , visibleProgram = Nothing
                , outputView = ViewIntroduction
                , outputPreference = PreferProblems
                , theme = flags.theme
                , installedPackages = []
                , knownPackages = []
                , showPackageList = False
                , searchForPackages = ""
                }
            , Cmd.none
            )

        Err err ->
            ( InitFailure err, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | CompiledNewDocument String
    | CompilationFailed Elm.Error.Error
    | TriggeredCompile
    | ToElm (Result Json.Decode.Error InteropDefinitions.ToElm)
    | PressedInsertExample Example
    | PreferForOutput OutputPreference
    | SwitchProgram
    | SelectedTheme Theme
    | PressedWipButton
    | PressedPackagesButton
    | PressedAddPackageButton Elm.Package.Name
    | PressedRemovePackageButton Elm.Package.Name
    | SearchingForPackage String


type Effect
    = SendFromElm InteropDefinitions.FromElm


update : Msg -> Model -> ( Model, List Effect )
update msg global =
    case ( msg, global ) of
        ( NoOp, _ ) ->
            ( global, [] )

        ( _, InitFailure _ ) ->
            ( global, [] )

        ( _, Editor model ) ->
            updateEditor msg model
                |> Tuple.mapFirst Editor


performEffects : List Effect -> Cmd Msg
performEffects =
    Cmd.batch << List.map performEffect


performEffect : Effect -> Cmd Msg
performEffect effect =
    case effect of
        SendFromElm event ->
            InteropPorts.fromElm event


updateEditor : Msg -> EditorModel -> ( EditorModel, List Effect )
updateEditor msg model =
    case msg of
        NoOp ->
            ( model, [] )

        ToElm (Err err) ->
            let
                _ =
                    Debug.log "ToElm Err" err
            in
            ( model, [] )

        ToElm (Ok ok) ->
            updateToElm ok model

        CompiledNewDocument url ->
            let
                ( visibleProgram, outputView ) =
                    if model.isCompiling && model.lastCompilation == NotStarted then
                        ( Just url, ViewCompiled )

                    else
                        ( model.visibleProgram
                            |> Maybe.withDefault url
                            |> Just
                        , model.outputView
                        )
            in
            ( { model
                | lastCompilation = Success url
                , isCompiling = False
                , visibleProgram = visibleProgram
                , outputView = outputView
              }
            , model.visibleProgram |> revokeObjectUrl
            )

        CompilationFailed error ->
            ( { model
                | lastCompilation = Failed <| Data.Problem.toIndexedProblems error
                , isCompiling = False
              }
            , []
            )

        TriggeredCompile ->
            ( { model | isCompiling = True, lastCompilation = NotStarted }
            , [ InteropDefinitions.TriggerCompile model.file |> SendFromElm ]
            )

        PressedInsertExample example ->
            ( { model
                | lastCompilation = NotStarted
                , visibleProgram = Nothing
                , outputView = ViewCompiled
              }
            , (Examples.getCode example |> InteropDefinitions.ReplaceCodeWith |> SendFromElm)
                :: revokeObjectUrl model.visibleProgram
            )

        PreferForOutput preference ->
            ( { model | outputPreference = preference }
            , []
            )

        SwitchProgram ->
            ( { model
                | visibleProgram =
                    case model.lastCompilation of
                        Success url ->
                            Just url

                        _ ->
                            Nothing
              }
            , revokeObjectUrl model.visibleProgram
            )

        SelectedTheme theme ->
            ( { model | theme = theme }
            , []
            )

        PressedWipButton ->
            ( model, [ InteropDefinitions.WipJs |> SendFromElm ] )

        PressedPackagesButton ->
            ( { model | showPackageList = not model.showPackageList }
            , [ InteropDefinitions.LoadPackageList |> SendFromElm ]
            )

        PressedAddPackageButton package ->
            ( { model
                | installedPackages =
                    ( package, InstallingPackage )
                        :: model.installedPackages
                        |> List.Extra.uniqueBy Tuple.first
              }
            , [ InteropDefinitions.AddPackage package |> SendFromElm ]
            )

        PressedRemovePackageButton package ->
            ( { model
                | installedPackages =
                    List.map
                        (\( pkg, state ) ->
                            if pkg == package then
                                ( pkg, RemovingPackage )

                            else
                                ( pkg, state )
                        )
                        model.installedPackages
              }
            , [ InteropDefinitions.RemovePackage package |> SendFromElm ]
            )

        SearchingForPackage string ->
            ( { model | searchForPackages = string }, [] )


updateToElm : InteropDefinitions.ToElm -> EditorModel -> ( EditorModel, List Effect )
updateToElm msg model =
    case msg of
        InteropDefinitions.OnCompileResult compileResult ->
            Debug.todo "not implemented"

        InteropDefinitions.PackageListLoaded (Err err) ->
            -- Either an error in the Haskell code,
            --  or decoding of package name or version has failed
            Debug.todo "not implemented"

        InteropDefinitions.PackageListLoaded (Ok list) ->
            ( { model | knownPackages = sortKnownPackages list }, [] )

        InteropDefinitions.PackageAdded (Err err) ->
            Debug.todo "not implemented"

        InteropDefinitions.PackageAdded (Ok data) ->
            ( { model | installedPackages = packageAdded data model.installedPackages }
            , []
            )

        InteropDefinitions.PackageRemoved (Err err) ->
            Debug.todo "not implemented"

        InteropDefinitions.PackageRemoved (Ok name) ->
            ( { model | installedPackages = packageRemoved name model.installedPackages }
            , []
            )


sortKnownPackages : InteropDefinitions.PackageList -> List KnownPackage
sortKnownPackages list =
    let
        recommendedPackages : Dict String Int
        recommendedPackages =
            [ [ "elm/core", "elmcraft/core-extra", "elm/html", "elm-browser", "elm/random" ]
            , [ "elm/parser" ]
            , [ "elm/http", "lukewestby/elm-http-builder", "elm/file" ]
            , [ "elm/json", "miniBill/elm-codec", "NoRedInk/elm-json-decode-pipeline" ]

            -- URL parsing won't be a default use case in editor
            -- , [ "elm/url", "lydell/elm-app-url" ]
            , [ "elm/svg", "dzuk-mutant/elm-css", "mdgriffith/elm-ui" ]
            , [ "dillonkearns/elm-markdown", "mdgriffith/elm-markup" ]
            , --working with time and date
              [ "elm/time", "justinmimbs/time-extra", "ryan-haskell/date-format", "justinmimbs/date" ]
            , -- animations
              [ "mdgriffith/elm-animator", "mdgriffith/elm-style-animation" ]
            , -- for games
              [ "eigenwijskids/elm-playground-eigenwijs", "justgook/webgl-playground", "w0rm/elm-physics" ]
            ]
                |> List.concat
                |> List.indexedMap (\index value -> ( value, index ))
                |> Dict.fromList

        recommendedAuthors : List String
        recommendedAuthors =
            [ "elm/", "elmcraft/", "elm-explorations/", "elm-community/" ]

        prioritizeAuthor : Int -> List String -> String -> Int
        prioritizeAuthor prio authors name =
            case authors of
                [] ->
                    9999

                author :: rest ->
                    if String.startsWith author name then
                        prio

                    else
                        prioritizeAuthor (prio + 1) rest name

        priority : KnownPackage -> Int
        priority ( pkg, _ ) =
            let
                name =
                    Elm.Package.toString pkg
            in
            case Dict.get name recommendedPackages of
                Just index ->
                    index

                Nothing ->
                    prioritizeAuthor 99 recommendedAuthors name
    in
    List.sortBy priority list


packageAdded : InteropDefinitions.PackageAddedOk -> List InstalledPackage -> List InstalledPackage
packageAdded added =
    List.map
        (\( pkg, state ) ->
            if added.name == pkg then
                ( pkg, InstalledPackage added.version )

            else
                ( pkg, state )
        )


packageRemoved : Elm.Package.Name -> List InstalledPackage -> List InstalledPackage
packageRemoved name =
    List.filter (\( pkg, _ ) -> pkg /= name)


revokeObjectUrl : Maybe ObjectUrl -> List Effect
revokeObjectUrl maybeUrl =
    Maybe.map (SendFromElm << InteropDefinitions.RevokeObjectUrl) maybeUrl
        |> Maybe.Extra.toList



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map ToElm InteropPorts.toElm



-- VIEW


type OutputView
    = ViewIntroduction
    | ViewCompiled


view : Model -> Html Msg
view model =
    case model of
        InitFailure err ->
            Html.pre [] [ Json.Decode.errorToString err |> Html.text ]

        Editor model_ ->
            viewEditor model_


viewEditor : EditorModel -> Html Msg
viewEditor model =
    Html.div [ Html.Attributes.class "wrapper", Theme.toAttribute model.theme ]
        [ viewMainNav model
        , Html.main_
            [ Html.Attributes.id "main"
            ]
            [ Html.Extra.viewIf model.showPackageList <|
                viewPackages model
            , Html.node "ulm-editor"
                [ Html.Attributes.attribute "file" model.file
                , Theme.toAttribute model.theme
                , onCustomEvent compileResultEvent compileResultDecoder
                ]
                []
            , Html.div [ Html.Attributes.id "output" ] <|
                viewOutput model
            ]
        ]


viewMainNav : EditorModel -> Html Msg
viewMainNav model =
    Html.nav [ Html.Attributes.class "main" ]
        [ Html.button [ Html.Events.onClick PressedPackagesButton ] [ Html.text "packages" ]
        , Html.button [] [ Html.text "editor" ]
        , Html.button [ Html.Events.onClick TriggeredCompile ] [ Html.text "run code" ]
        , Html.button [] [ Html.text "examples" ]
        , Html.button [] [ Html.text "show program" ]
        , Html.button [] [ Html.text "show errros" ]
        , Html.button [] [ Html.text "wip" ]
        , if model.theme == Theme.Dark then
            Html.button [ Html.Events.onClick <| SelectedTheme Theme.Light ]
                [ Html.text <| Theme.toString Theme.Light ]

          else
            Html.button [ Html.Events.onClick <| SelectedTheme Theme.Dark ]
                [ Html.text <| Theme.toString Theme.Dark ]
        ]


viewPackages :
    { m
        | installedPackages : List ( Elm.Package.Name, InstalledPackageState )
        , knownPackages : List KnownPackage
        , searchForPackages : String
    }
    -> Html Msg
viewPackages { installedPackages, knownPackages, searchForPackages } =
    Html.section [ Html.Attributes.id "packages" ]
        [ Html.h2 [] [ Html.text "Installed" ]
        , installedPackages
            |> List.map viewInstalledPackage
            |> Html.ul []
        , Html.h2 [] [ Html.text "Package registry" ]
        , Html.input
            [ Html.Attributes.placeholder "Search"
            , Html.Events.onInput SearchingForPackage
            ]
            []
        , knownPackages
            |> searchInKnownPackages searchForPackages
            |> List.take 26
            |> List.map viewKnownPackage
            |> Html.ul []
        , Html.text (String.fromInt (List.length knownPackages) ++ " in total")
        ]


viewInstalledPackage : ( Elm.Package.Name, InstalledPackageState ) -> Html Msg
viewInstalledPackage ( name, state ) =
    Html.li [] <|
        case state of
            InstallingPackage ->
                [ viewPackage name Nothing
                , Html.text "installing"
                , Html.button [ Html.Events.onClick <| PressedRemovePackageButton name ] [ Html.text "remove" ]
                ]

            InstalledPackage version ->
                [ viewPackage name (Just version)
                , Html.text <| Elm.Version.toString version
                , Html.button [ Html.Events.onClick <| PressedRemovePackageButton name ] [ Html.text "remove" ]
                ]

            RemovingPackage ->
                [ viewPackage name Nothing
                , Html.text "removing"
                , Html.button [ Html.Events.onClick <| PressedRemovePackageButton name ] [ Html.text "remove" ]
                ]

            InstallingPackageFailed ->
                [ Html.text <| "Could not add " ++ Elm.Package.toString name
                , Html.button [ Html.Events.onClick <| PressedAddPackageButton name ] [ Html.text "try again" ]
                ]


searchInKnownPackages : String -> List KnownPackage -> List KnownPackage
searchInKnownPackages key list =
    -- TODO maybe I want to cache the results (or maybe keyed/lazy will be enough)
    -- TODO search by distance
    List.filter (\( name, _ ) -> String.contains key <| Elm.Package.toString name) list


viewKnownPackage : KnownPackage -> Html Msg
viewKnownPackage ( name, version ) =
    Html.li []
        [ viewPackage name (Just version)
        , Html.text <| Elm.Version.toString version
        , Html.button [ Html.Events.onClick <| PressedAddPackageButton name ]
            [ Html.text "+" ]
        ]


viewPackage : Elm.Package.Name -> Maybe Elm.Version.Version -> Html msg
viewPackage package maybeVersion =
    let
        name =
            Elm.Package.toString package

        version =
            Maybe.map Elm.Version.toString maybeVersion
                |> Maybe.withDefault "latest"
    in
    Html.a
        [ Html.Attributes.href <|
            -- or maybe https://elm.dmy.fr/ and https://dark.elm.dmy.fr/ based on theme?
            "https://package.elm-lang.org/packages/"
                ++ name
                ++ "/"
                ++ version
        ]
        [ Html.text name ]


viewOutput : EditorModel -> List (Html Msg)
viewOutput model =
    let
        programIsVisible =
            isProgramVisible model
    in
    [ Html.menu []
        [ Html.button
            [ Html.Events.onClick TriggeredCompile
            , Html.Attributes.title "Run code"
            ]
            [ Icon.playCircle [ Html.Attributes.style "color" "green" ]
            , Html.text "Run code"
            ]
        , Html.button
            [ Html.Attributes.class "circle-icon"
            , Html.Events.onClick TriggeredCompile
            , Html.Attributes.title "compile"
            , Html.Attributes.attribute "aria-label" "compile"
            ]
            [ Icon.playCircle [ Html.Attributes.style "color" "green" ]
            ]
        , case ( model.lastCompilation, model.visibleProgram ) of
            ( Success newUrl, Just oldUrl ) ->
                if newUrl /= oldUrl then
                    Html.button
                        [ Html.Events.onClick SwitchProgram ]
                        [ Html.text "show new program" ]

                else
                    Html.Extra.nothing

            _ ->
                Html.Extra.nothing
        , Html.Extra.viewIf (not programIsVisible) <|
            Html.button
                [ Html.Events.onClick <| PreferForOutput PreferProgram
                ]
                [ Html.text "show program" ]
        , case ( programIsVisible, model.lastCompilation ) of
            ( True, Failed [ _ ] ) ->
                Html.button
                    [ Html.Events.onClick <| PreferForOutput PreferProblems ]
                    [ Html.text "show problem" ]

            ( True, Failed _ ) ->
                Html.button
                    [ Html.Events.onClick <| PreferForOutput PreferProblems ]
                    [ Html.text "show problems" ]

            _ ->
                Html.Extra.nothing
        , if model.theme == Theme.Dark then
            Html.button [ Html.Events.onClick <| SelectedTheme Theme.Light ]
                [ Html.text <| Theme.toString Theme.Light ]

          else
            Html.button [ Html.Events.onClick <| SelectedTheme Theme.Dark ]
                [ Html.text <| Theme.toString Theme.Dark ]
        , Html.button [ Html.Events.onClick <| PressedWipButton ]
            [ Html.text "wip" ]
        ]
    , Html.Keyed.node "article" [] <| viewCompiled model
    ]


viewCompiled : EditorModel -> List ( String, Html Msg )
viewCompiled model =
    let
        programIsHidden =
            not <| isProgramVisible model
    in
    [ ( "doc"
      , Html.Extra.viewMaybe
            (\blobUrl ->
                Html.iframe
                    [ Html.Attributes.src blobUrl
                    , Html.Attributes.classList [ ( "hidden", programIsHidden ) ]
                    ]
                    []
            )
            model.visibleProgram
      )
    , ( "intro"
      , Html.Extra.viewIf (model.outputView == ViewIntroduction) viewIntroduction
      )
    , ( "problems"
      , case ( programIsHidden, model.lastCompilation ) of
            ( True, Failed problems ) ->
                Data.Problem.viewList (\_ -> NoOp) problems

            _ ->
                Html.Extra.nothing
      )
    ]


isProgramVisible : EditorModel -> Bool
isProgramVisible model =
    if model.outputView == ViewIntroduction then
        False

    else
        case model.lastCompilation of
            Failed _ ->
                model.outputPreference == PreferProgram

            _ ->
                True


viewIntroduction : Html Msg
viewIntroduction =
    Html.div [ Html.Attributes.class "notification" ]
        [ Html.h1 [] [ Html.text "Elm Editor" ]
        , Html.p [] [ Html.text "Write and compile code in your browser!" ]
        , Html.p []
            [ Html.text "Not sure how to get started?"
            , Html.br [] []
            , Html.text "Maybe start with one of these examples:"
            ]
        , Html.ul [] examples
        , Html.p []
            [ Html.text "Check out "
            , Html.a [ Html.Attributes.href "https://guide.elm-lang.org/" ] [ Html.text "the official guide" ]
            , Html.text " for a tutorial on Elm or visit "
            , Html.a [ Html.Attributes.href "https://elmcraft.org/learn" ] [ Html.text "this overview page" ]
            , Html.text " maintained by the Elm community."
            ]
        ]


examples : List (Html Msg)
examples =
    List.map
        (\( name, example ) ->
            Html.li [] [ Html.button [ Html.Events.onClick <| PressedInsertExample example ] [ Html.text name ] ]
        )
        Examples.list
        ++ [ Html.li []
                [ Html.a
                    [ Html.Attributes.href "https://elm-lang.org/examples"
                    , Html.Attributes.target "_blank"
                    ]
                    [ Html.text "More!" ]
                ]
           ]


onCustomEvent : String -> Json.Decode.Decoder a -> Html.Attribute a
onCustomEvent event decoder =
    Html.Events.on event (Json.Decode.field "detail" decoder)


compileResultDecoder : Json.Decode.Decoder Msg
compileResultDecoder =
    -- Json.Decode.map ToElm
    --     (TsJson.Decode.decoder InteropDefinitions.interop.toElm  )
    Json.Decode.field "type" Json.Decode.string
        |> Json.Decode.andThen compileResultDecoder2



-- compileResultDecoder3 : Json.Decode.Value -> Result Json.Decode.Error InteropDefinitions.CompileResult
-- compileResultDecoder3 value =
--     Json.Decode.decodeValue (TsJson.Decode.decoder InteropDefinitions.compileResult) value


compileResultEvent : String
compileResultEvent =
    "compile-result"


compileResultDecoder2 : String -> Json.Decode.Decoder Msg
compileResultDecoder2 type_ =
    case type_ of
        "success" ->
            Json.Decode.map CompiledNewDocument
                (Json.Decode.field "url" Json.Decode.string)

        _ ->
            -- Json.Decode.fail <| compileResultEvent ++ " with type='" ++ type_ ++ "' is not supported"
            Json.Decode.map CompilationFailed Elm.Error.decoder
