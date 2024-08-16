module UlmRepl exposing (main, view)

import AnsiExtra
import Browser
import Data.Problem
import Elm.DSLParser
import Elm.Error
import Elm.Parser
import Elm.Pretty
import Heroicons.Solid as Icon
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Extra
import Html.Keyed
import InteropDefinitions as Io
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
    { history2 : List { input : Io.Input, id : Io.Timestamp, result : Io.EvaluatedInput }
    , theme : Theme
    , inputHint : Maybe Io.Output
    , typeDefinitionsVisible : Bool
    }


type InputHint
    = NoInputHint
    | HintExpr { value : String, type_ : String }
    | HintDecl { name : String, value : String, type_ : String }


type HistoryEntry
    = Failure (List Data.Problem.Problem)
    | Output { maybeName : Maybe String, value : String, type_ : String }



-- INIT


init : Json.Decode.Value -> ( Model, Cmd Msg )
init json =
    let
        flags =
            InteropPorts.decodeFlags json
                |> Result.withDefault Io.default
    in
    ( { history2 = []
      , theme = flags.theme
      , typeDefinitionsVisible = True
      , inputHint =
            -- { name = Just "abc"
            -- , type_ = "number"
            -- , value = "\u{001B}[95m123\u{001B}[0m"
            -- }
            -- { name = Nothing
            -- , type_ = "String -> Int"
            -- , value = "\u{001B}[36m<function>\u{001B}[0m"
            -- }
            --     |> Just
            Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
      -- | CompilationFailed { input : String, error : Elm.Error.Error }
      -- | NewWorkDone { input: String, output: HistoryEntry }
    | AddHistoryEntry Io.Input HistoryEntry
    | ClearHint
    | TriggeredCompile
    | ToElm (Result Json.Decode.Error Io.ToElm)
    | SelectedTheme Theme
    | SetVisibilityOfTypeDefinitions Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "update msg" msg of
        NoOp ->
            ( model, Cmd.none )

        ToElm (Err _) ->
            ( model, Cmd.none )

        ToElm (Ok ok) ->
            updateToElm ok model

        SelectedTheme theme ->
            ( { model | theme = theme }
            , Cmd.none
            )

        SetVisibilityOfTypeDefinitions bool ->
            ( { model | typeDefinitionsVisible = bool }, Cmd.none )

        AddHistoryEntry input (Failure err) ->
            -- TODO distinguish between events on keystroke and forced compilations
            ( model
                -- |> addHistoryEntry input (Failure err)
                |> clearHint
            , Cmd.none
            )

        AddHistoryEntry input (Output output) ->
            -- TODO distinguish between events on keystroke and forced compilations
            ( model
              -- |> addHistoryEntry input (Output output)
              -- |> addHint { name output
            , Cmd.none
            )

        TriggeredCompile ->
            ( model
            , InteropPorts.fromElm Io.TriggerCompile
            )

        ClearHint ->
            ( model |> clearHint, Cmd.none )


updateToElm : Io.ToElm -> Model -> ( Model, Cmd msg )
updateToElm msg model =
    case msg of
        Io.CheckedTextInput Io.FoundNothing ->
            ( model |> clearHint, Cmd.none )

        Io.CheckedTextInput (Io.FoundExpression { type_, value }) ->
            ( { model | inputHint = Just { name = Nothing, type_ = type_, value = value } }
            , Cmd.none
            )

        Io.CheckedTextInput (Io.FoundDeclaration { name, type_, value }) ->
            ( { model | inputHint = Just { name = Just name, type_ = type_, value = value } }
            , Cmd.none
            )

        Io.OnCompileResult (Io.CompileSuccess _) ->
            Debug.todo "branch 'OnCompileResult (CompileSuccess _)' not implemented"

        Io.OnCompileResult (Io.CompileError _) ->
            Debug.todo "branch 'OnCompileResult (CompileError _)' not implemented"

        Io.OnCompileResult (Io.CompileErrors { errors }) ->
            ( model |> clearHint, Cmd.none )

        Io.Executed _ ->
            Debug.todo "branch 'Executed _' not implemented"

        Io.ClearHint ->
            Debug.todo "branch 'ClearHint' not implemented"

        Io.EvaluatedTextInput data ->
            ( { model | history2 = enhanceEvaluatedTextInput data :: model.history2 }
                |> clearHint
            , Cmd.none
            )


clearHint : Model -> Model
clearHint model =
    { model | inputHint = Nothing }


enhanceEvaluatedTextInput : Io.EvaluatedTextInputData -> Io.EvaluatedTextInputData
enhanceEvaluatedTextInput { input, id, result } =
    { id = id
    , input =
        case result of
            Io.Problems _ ->
                input

            _ ->
                elmFormat input
    , result =
        case result of
            Io.EvaluatedDeclaration decl ->
                prependDeclarationTypes decl |> Io.EvaluatedDeclaration

            _ ->
                result
    }


elmFormat : String -> String
elmFormat unformatted =
    -- TODO see tests/ElmFormat.elm
    unformatted


prependDeclarationTypes : Io.NewDeclaration -> Io.NewDeclaration
prependDeclarationTypes declaration =
    -- TODO use the value and type information to prepend the inferred types of a declaration if they are missing
    -- might need some further work in the compiler because it does not return the actual imported names
    declaration



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
        [ Html.h1 [] [ Html.text "Hello and welcome to the elm.run REPL" ]
        , Html.p []
            [ Html.text "I'm glad you are here. Do you want to start with "
            , Html.a
                [ Html.Attributes.href "#TODO-examples" ]
                [ Html.text "example code" ]
            , Html.text " or a "
            , Html.a
                [ Html.Attributes.href "#TODO-tour" ]
                [ Html.text "tour of the Elm programming language" ]
            , Html.text "?"
            , Html.br [] []
            , Html.text "If not, feel free to enter code below that I will try my best to read what you type and provide useful hints about types and what it would evaluate to. When ready, press Ctrl+Enter to evaluate the code and add it to the history."
            , Html.br [] []
            , Html.text "You can create type definitions and functions, replace a definition or declaration by using the same name again."
            ]
        , Html.p [] [ Html.text "The code you write here does not leave your browser. The Elm compiler runs directly as a WASM module and generates JavaScript that is immediately executed." ]
        , Html.Keyed.ul [ Html.Attributes.class "logs monospace" ] <|
            List.map
                (\{ id, input, result } ->
                    let
                        rest =
                            case result of
                                Io.Problems problems ->
                                    -- TODO remove `Jump to problem` button
                                    [ Data.Problem.viewList (\_ -> NoOp) problems ]

                                Io.NoOutput ->
                                    []

                                Io.EvaluatedDeclaration { value, type_ } ->
                                    viewValue value
                                        ++ [ Html.text " : "
                                           , viewType type_
                                           ]

                                Io.EvaluatedExpression { value, type_ } ->
                                    viewValue value
                                        ++ [ Html.text " : "
                                           , viewType type_
                                           ]
                    in
                    ( String.fromFloat id
                    , Html.li []
                        (Html.pre
                            []
                            [ Html.text input ]
                            :: rest
                        )
                    )
                )
            <|
                List.reverse model.history2
        , inputBox model
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
            , let
                caption =
                    if model.typeDefinitionsVisible then
                        "Hide type definitions"

                    else
                        "Show type definitions"
              in
              Html.button
                [ Html.Events.onClick <| SetVisibilityOfTypeDefinitions <| not model.typeDefinitionsVisible ]
                [ Html.text caption ]
            ]
        ]


inputBox : Model -> Html Msg
inputBox model =
    Html.div [ Html.Attributes.class "repl-input-block" ]
        [ case model.inputHint of
            Nothing ->
                Html.text "Just enter text below"

            Just hint ->
                Html.div
                    [ Html.Attributes.class "with-editor-lines-margin monospace" ]
                    [ Html.Extra.viewMaybe
                        (\name -> Html.text <| name ++ " : ")
                        hint.name
                    , viewType hint.type_
                    ]
        , Html.node "ulm-editor"
            [ Theme.toAttribute model.theme
            , Html.Attributes.autofocus True
            ]
            []
        , Html.Extra.viewMaybe
            (\output ->
                Html.div [ Html.Attributes.class "with-editor-lines-margin monospace" ]
                    (viewValue output.value
                        ++ [ Html.text " : ", viewType output.type_ ]
                    )
            )
            model.inputHint
        ]


viewValue : AnsiExtra.Parsed -> List (Html msg)
viewValue value =
    AnsiExtra.view value


viewType : String -> Html msg
viewType str =
    Html.span [] [ Html.text str ]


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
