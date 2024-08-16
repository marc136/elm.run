module UlmRepl exposing (main)

import Browser
import Data.Problem
import Elm.Error
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


type alias Model =
    { history : List ( InteropDefinitions.Input, HistoryEntry )
    , theme : Theme
    , hintBelowInput : Maybe InteropDefinitions.Output
    }


type HistoryEntry
    = Failure (List Data.Problem.Problem)
    | Output { maybeName : Maybe String, value : String, type_ : String }


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
    let
        flags =
            InteropPorts.decodeFlags json
                |> Result.withDefault InteropDefinitions.default
    in
    ( { history =
            [ ( "abc = 123"
              , Output
                    { maybeName = Just "abc"
                    , type_ = "number"
                    , value = "\u{001B}[95m123\u{001B}[0m"
                    }
              )
            , ( "String.length"
              , Output
                    { maybeName = Nothing
                    , type_ = "String -> Int"
                    , value = "\u{001B}[36m<function>\u{001B}[0m"
                    }
              )
            ]
      , theme = flags.theme
      , hintBelowInput =
            -- { name = Just "abc"
            -- , type_ = "number"
            -- , value = "\u{001B}[95m123\u{001B}[0m"
            -- }
            { name = Nothing
            , type_ = "String -> Int"
            , value = "\u{001B}[36m<function>\u{001B}[0m"
            }
                |> Just
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
      -- | CompilationFailed { input : String, error : Elm.Error.Error }
      -- | NewWorkDone { input: String, output: HistoryEntry }
    | AddHistoryEntry InteropDefinitions.Input HistoryEntry
    | TriggeredCompile
    | ToElm (Result Json.Decode.Error InteropDefinitions.ToElm)
    | SelectedTheme Theme


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "update msg" msg of
        NoOp ->
            ( model, Cmd.none )

        ToElm (Err _) ->
            ( model, Cmd.none )

        ToElm (Ok _) ->
            ( model, Cmd.none )

        SelectedTheme theme ->
            ( { model | theme = theme }
            , Cmd.none
            )

        AddHistoryEntry input output ->
            ( { model
                | history = ( input, output ) :: model.history
              }
            , Cmd.none
            )

        TriggeredCompile ->
            ( model
            , InteropPorts.fromElm InteropDefinitions.TriggerCompile
            )



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
    Html.main_
        [ Html.Attributes.id "main"
        , Theme.toAttribute model.theme
        ]
        [ Html.p [ Html.Attributes.style "margin" "0px" ]
            [ Html.span []
                [ Html.text "> String.length"
                , Html.span [ Html.Attributes.style "color" "rgb(51, 187, 200)" ]
                    [ Html.text "<function>" ]
                , Html.span [ Html.Attributes.style "color" "rgb(203, 204, 205)" ]
                    [ Html.text ": String -> Int" ]
                ]
            ]
        , Html.ul [ Html.Attributes.class "logs" ] <|
            List.map
                (\( input, output ) ->
                    let
                        rest =
                            case output of
                                Failure problems ->
                                    -- TODO remove `Jump to problem` button
                                    [ Data.Problem.viewList (\_ -> NoOp) problems ]

                                Output { maybeName, value, type_ } ->
                                    [ Html.Extra.viewMaybe
                                        (\name ->
                                            Html.span [] [ Html.text name ]
                                        )
                                        maybeName
                                    , Html.span [] [ Html.text value ]
                                    , Html.span [] [ Html.text type_ ]
                                    ]
                    in
                    Html.li []
                        (Html.span [] [ Html.text input ] :: rest)
                )
            <|
                List.reverse model.history
        , Html.node "ulm-editor"
            [ Theme.toAttribute model.theme
            , onCustomEvent compileResultEvent compileResultDecoder

            -- , onCustomEvent compileResultEvent (InteropDefinitions.interop.toElm |> TsJson.Decode.decoder |> ToElm )
            ]
            []
        , Html.menu []
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
            , if model.theme == Theme.Dark then
                Html.button [ Html.Events.onClick <| SelectedTheme Theme.Light ]
                    [ Html.text <| Theme.toString Theme.Light ]

              else
                Html.button [ Html.Events.onClick <| SelectedTheme Theme.Dark ]
                    [ Html.text <| Theme.toString Theme.Dark ]
            ]
        ]


viewIntroduction : Html Msg
viewIntroduction =
    Html.div [ Html.Attributes.class "notification" ]
        [ Html.h1 [] [ Html.text "Elm Repl" ]
        , Html.p [] [ Html.text "Write and compile code in your browser!" ]
        , Html.p []
            [ Html.text "Not sure how to get started?"
            , Html.br [] []
            ]
        , Html.p []
            [ Html.text "Check out "
            , Html.a [ Html.Attributes.href "https://guide.elm-lang.org/" ] [ Html.text "the official guide" ]
            , Html.text " for a tutorial on Elm or visit "
            , Html.a [ Html.Attributes.href "https://elmcraft.org/learn" ] [ Html.text "this overview page" ]
            , Html.text " maintained by the Elm community."
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
    case Debug.log "compileResultDecoder2" type_ of
        "success" ->
            -- Json.Decode.map CompiledNewDocument
            -- (Json.Decode.field "url" Json.Decode.string)
            Debug.todo "ok decoder"

        "compile-errors" ->
            Json.Decode.map2
                (\input err ->
                    Data.Problem.toIndexedProblems err
                        |> Failure
                        |> AddHistoryEntry input
                )
                (Json.Decode.field "input" Json.Decode.string)
                Elm.Error.decoder

        "new-work" ->
            Json.Decode.map2
                (\input output -> AddHistoryEntry input output)
                (Json.Decode.field "input" Json.Decode.string)
                (Json.Decode.field "output" newWorkDecoder)

        _ ->
            -- Json.Decode.fail <| compileResultEvent ++ " with type='" ++ type_ ++ "' is not supported"
            -- Json.Decode.map CompilationFailed Elm.Error.decoder
            Debug.todo "err decoder"


newWorkDecoder : Json.Decode.Decoder HistoryEntry
newWorkDecoder =
    Json.Decode.map3
        (\name value type_ ->
            Output { maybeName = name, value = value, type_ = type_ }
        )
        (Json.Decode.field "name" (Json.Decode.maybe Json.Decode.string))
        (Json.Decode.field "value" Json.Decode.string)
        (Json.Decode.field "type" Json.Decode.string)
