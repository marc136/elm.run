module UlmRepl exposing (main, view)

import AnsiExtra
import Browser
import Data.Problem
import Heroicons.Solid as Icon
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Extra
import Html.Keyed
import InteropDefinitions as Io
import InteropPorts
import Json.Decode
import ModalDialog
import Theme exposing (Theme)



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
    { history : List HistoryEntry
    , theme : Theme
    , inputHint : Maybe Io.Output
    , typeDefinitionsVisible : Bool
    , modalDialog : Maybe ModalDialog
    }


type alias HistoryEntry =
    -- TODO state to mark declaration as outdated, needed for visual and for clean up
    { input : Io.Input, id : Io.Timestamp, result : Io.EvaluatedInput }


type ModalDialog
    = ClearDialog



-- INIT


init : Json.Decode.Value -> ( Model, Cmd Msg )
init json =
    let
        flags =
            InteropPorts.decodeFlags json
                |> Result.withDefault Io.default
    in
    ( { history = []
      , theme = flags.theme
      , typeDefinitionsVisible = True
      , inputHint = Nothing
      , modalDialog = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | TriggeredCompile
    | ToElm (Result Json.Decode.Error Io.ToElm)
    | SelectedTheme Theme
    | SetVisibilityOfTypeDefinitions Bool
    | PressedClearButton
    | PressedRemoveButton Io.Timestamp
    | PressedRemoveAll
    | PressedRemoveAllExceptCurrentDeclarations
    | PressedRemoveOutdatedDeclarations
    | PressedRemoveErrors
    | PressedRemoveExpressions
    | CloseModal


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

        TriggeredCompile ->
            ( model
            , InteropPorts.fromElm Io.TriggerCompile
            )

        CloseModal ->
            ( { model | modalDialog = Nothing }, Cmd.none )

        PressedClearButton ->
            ( { model | modalDialog = Just ClearDialog }, Cmd.none )

        PressedRemoveAllExceptCurrentDeclarations ->
            ( model
                |> filterHistory
                    (\entry ->
                        case entry.result of
                            Io.EvaluatedExpression _ ->
                                False

                            Io.NoOutput ->
                                False

                            Io.EvaluatedDeclaration _ ->
                                -- TODO keep current declarations
                                False

                            Io.Problems _ ->
                                False
                    )
            , Cmd.none
            )

        PressedRemoveOutdatedDeclarations ->
            ( model
                |> filterHistory
                    (\entry ->
                        case entry.result of
                            Io.EvaluatedDeclaration _ ->
                                -- TODO keep current declarations
                                False

                            _ ->
                                True
                    )
            , Cmd.none
            )

        PressedRemoveErrors ->
            ( model
                |> filterHistory
                    (\entry ->
                        case entry.result of
                            Io.Problems _ ->
                                False

                            _ ->
                                True
                    )
            , Cmd.none
            )

        PressedRemoveExpressions ->
            ( model
                |> filterHistory
                    (\entry ->
                        case entry.result of
                            Io.EvaluatedExpression _ ->
                                False

                            _ ->
                                True
                    )
            , Cmd.none
            )

        PressedRemoveAll ->
            ( { model | history = [] }
            , List.filterMap
                (\entry ->
                    case entry.result of
                        Io.EvaluatedDeclaration { name } ->
                            Just name

                        _ ->
                            Nothing
                )
                model.history
                |> Io.RemoveFromState
                |> InteropPorts.fromElm
            )

        PressedRemoveButton id ->
            -- Should I ask for confirmation?
            let
                ( keep, remove ) =
                    List.partition (\entry -> entry.id /= id) model.history
            in
            ( { model | history = keep }
            , List.filterMap
                (\entry ->
                    case entry.result of
                        Io.EvaluatedDeclaration { name } ->
                            Just name

                        _ ->
                            Nothing
                )
                remove
                |> Io.RemoveFromState
                |> InteropPorts.fromElm
            )


updateToElm : Io.ToElm -> Model -> ( Model, Cmd Msg )
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

        Io.EvaluatedTextInput data ->
            ( { model | history = enhanceEvaluatedTextInput data :: model.history }
                |> clearHint
            , InteropPorts.fromElm Io.ScrollToBottom
            )


clearHint : Model -> Model
clearHint model =
    { model | inputHint = Nothing }


filterHistory : (HistoryEntry -> Bool) -> Model -> Model
filterHistory fn model =
    { model | history = List.filter fn model.history }


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


view : Model -> Html Msg
view model =
    Html.main_
        [ Html.Attributes.id "main"
        , Theme.toAttribute model.theme
        ]
        [ Html.h1 [] [ Html.text "Hello and welcome to the elm.run REPL" ]
        , Theme.htmlSelectElement SelectedTheme
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
        , Html.nav [ Html.Attributes.class "sticky" ]
            [ Html.span [ Html.Attributes.class "visible-if-sticky" ] [ Html.text "elm.run REPL" ]
            , Html.menu []
                [ Html.Extra.viewIfLazy (List.length model.history > 0)
                    (\() ->
                        Html.button [ Html.Events.onClick PressedClearButton ]
                            [ Icon.archiveBoxXMark [], Html.text "Clean up " ]
                    )
                ]
            , Theme.htmlSelectElement SelectedTheme
            ]
        , Html.Keyed.ul [ Html.Attributes.class "logs monospace" ] <|
            List.map viewHistoryEntry <|
                List.reverse model.history
        , Html.div [ Html.Attributes.class "input-row" ]
            [ Html.button
                [ Html.Events.onClick TriggeredCompile
                , Html.Attributes.title "Run code"
                ]
                [ Icon.playCircle [ Html.Attributes.style "color" "green" ]
                , Html.text "Run code"
                ]
            , inputBox model
            ]
        , viewModalDialog model
        ]


viewHistoryEntry : HistoryEntry -> ( String, Html Msg )
viewHistoryEntry { id, input, result } =
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
        (Html.button [ Html.Events.onClick <| PressedRemoveButton id ]
            -- TODO add icon or screen reader caption
            [ Html.text "x" ]
            :: Html.pre
                []
                [ Html.text input ]
            :: rest
        )
    )


inputBox : Model -> Html Msg
inputBox model =
    Html.div [ Html.Attributes.class "repl-input-column" ]
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


viewModalDialog : Model -> Html Msg
viewModalDialog { modalDialog } =
    case modalDialog of
        Nothing ->
            Html.Extra.nothing

        Just ClearDialog ->
            ModalDialog.view True
                CloseModal
                []
                [ Html.form [ Html.Events.onSubmit CloseModal ]
                    [ Html.h1 [] [ Html.text "What should I do?" ]
                    , clickButton PressedRemoveAll "Clear everything (full reset)"
                    , Html.hr [] []
                    , clickButton PressedRemoveAllExceptCurrentDeclarations
                        "Remove everything except the current declarations"
                    , clickButton PressedRemoveOutdatedDeclarations
                        "Remove only outdated declarations"
                    , clickButton PressedRemoveErrors
                        "Remove all errors"
                    , clickButton PressedRemoveExpressions
                        "Remove evaluated expressions"
                    , Html.hr [] []
                    , Html.button
                        [ Html.Attributes.attribute "formmethod" "dialog"
                        , Html.Events.onClick CloseModal
                        , Html.Attributes.autofocus True
                        ]
                        [ Html.text "Nothing, let's go back to the REPL"
                        ]
                    ]
                ]


clickButton : msg -> String -> Html msg
clickButton msg caption =
    Html.button [ Html.Events.onClick msg ] [ Html.text caption ]


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
