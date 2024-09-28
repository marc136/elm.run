module UlmEditor exposing (main)

import Browser
import Data.Problem
import Elm.Error
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
import Maybe.Extra
import Theme exposing (Theme)
import TsJson.Decode



-- MAIN


main : Program Json.Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
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
    | InsertExample Example
    | PreferForOutput OutputPreference
    | SwitchProgram
    | SelectedTheme Theme
    | PressedWipButton


update : Msg -> Model -> ( Model, Cmd Msg )
update msg global =
    case ( msg, global ) of
        ( NoOp, _ ) ->
            ( global, Cmd.none )

        ( _, InitFailure _ ) ->
            ( global, Cmd.none )

        ( ToElm (Err _), _ ) ->
            ( global, Cmd.none )

        -- ( ToElm (Ok OnCompileResult), Editor model ) ->
        --     ( Editor { model | lastDocument = Just url }
        --     , model.visibleProgram |> revokeObjectUrl
        --     )
        ( ToElm (Ok _), _ ) ->
            ( global, Cmd.none )

        ( CompiledNewDocument url, Editor model ) ->
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
            ( Editor
                { model
                    | lastCompilation = Success url
                    , isCompiling = False
                    , visibleProgram = visibleProgram
                    , outputView = outputView
                }
            , model.visibleProgram |> revokeObjectUrl
            )

        ( CompilationFailed error, Editor model ) ->
            ( Editor
                { model
                    | lastCompilation = Failed <| Data.Problem.toIndexedProblems error
                    , isCompiling = False
                }
            , Cmd.none
            )

        ( TriggeredCompile, Editor model ) ->
            ( Editor { model | isCompiling = True, lastCompilation = NotStarted }
            , InteropPorts.fromElm <| InteropDefinitions.TriggerCompile model.file
            )

        ( InsertExample example, Editor model ) ->
            ( Editor
                { model
                    | lastCompilation = NotStarted
                    , visibleProgram = Nothing
                    , outputView = ViewCompiled
                }
            , Cmd.batch
                [ Examples.getCode example
                    |> InteropDefinitions.ReplaceCodeWith
                    |> InteropPorts.fromElm
                , model.visibleProgram |> revokeObjectUrl
                ]
            )

        ( PreferForOutput preference, Editor model ) ->
            ( Editor { model | outputPreference = preference }
            , Cmd.none
            )

        ( SwitchProgram, Editor model ) ->
            ( Editor
                { model
                    | visibleProgram =
                        case model.lastCompilation of
                            Success url ->
                                Just url

                            _ ->
                                Nothing
                }
            , revokeObjectUrl model.visibleProgram
            )

        ( SelectedTheme theme, Editor model ) ->
            ( Editor { model | theme = theme }
            , Cmd.none
            )

        ( PressedWipButton, model ) ->
            ( model, InteropDefinitions.WipJs |> InteropPorts.fromElm )


revokeObjectUrl : Maybe ObjectUrl -> Cmd msg
revokeObjectUrl maybeUrl =
    maybeUrl
        |> Maybe.map (InteropDefinitions.RevokeObjectUrl >> InteropPorts.fromElm)
        |> Maybe.withDefault Cmd.none



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
    Html.main_
        [ Html.Attributes.id "main"
        , Theme.toAttribute model.theme
        ]
        [ Html.node "ulm-editor"
            [ Html.Attributes.attribute "file" model.file
            , Theme.toAttribute model.theme
            , onCustomEvent compileResultEvent compileResultDecoder

            -- , onCustomEvent compileResultEvent (InteropDefinitions.interop.toElm |> TsJson.Decode.decoder |> ToElm )
            ]
            []
        , Html.div [ Html.Attributes.id "output" ] <|
            viewOutput model
        ]


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
            Html.li [] [ Html.button [ Html.Events.onClick <| InsertExample example ] [ Html.text name ] ]
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
