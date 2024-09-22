module UlmRepl exposing (main, view)

import AnsiExtra
import Browser
import Data.Problem
import Heroicons.Solid as Icon
import Html exposing (Html)
import Html.Attributes
import Html.Attributes.Aria
import Html.Events
import Html.Extra
import Html.Keyed
import InteropDefinitions as Io
import InteropPorts
import Json.Decode
import List.Extra
import ModalDialog
import Svg
import Svg.Attributes
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
    | ClickedExampleCode
    | PressedEditButton Io.Timestamp
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
    case msg of
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

        ClickedExampleCode ->
            ( model, InteropPorts.fromElm Io.EnterExampleCode )

        TriggeredCompile ->
            ( model
            , InteropPorts.fromElm Io.TriggerCompile
            )

        CloseModal ->
            ( { model | modalDialog = Nothing }, Cmd.none )

        PressedEditButton id ->
            ( model
            , model.history
                |> List.Extra.find (\entry -> entry.id == id)
                |> Maybe.map (\{ input } -> Io.ReplaceEnteredCode input |> InteropPorts.fromElm)
                |> Maybe.withDefault Cmd.none
            )

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

                            Io.EvaluatedDeclaration { outdated } ->
                                not outdated

                            Io.Problems _ ->
                                False

                            Io.Import ->
                                True

                            Io.TypeDefinition { outdated } ->
                                not outdated
                    )
            , Cmd.none
            )

        PressedRemoveOutdatedDeclarations ->
            ( model
                |> filterHistory
                    (\entry ->
                        case entry.result of
                            Io.EvaluatedDeclaration { outdated } ->
                                not outdated

                            Io.TypeDefinition { outdated } ->
                                not outdated

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
            , [ List.filterMap
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
              , Io.ReplaceEnteredCode "" |> InteropPorts.fromElm
              ]
                |> Cmd.batch
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
            let
                history =
                    case data.result of
                        Io.EvaluatedDeclaration new ->
                            List.map
                                (\entry ->
                                    case entry.result of
                                        Io.EvaluatedDeclaration old ->
                                            if old.name == new.name then
                                                { entry | result = Io.EvaluatedDeclaration { old | outdated = True } }

                                            else
                                                entry

                                        _ ->
                                            entry
                                )
                                model.history

                        Io.TypeDefinition new ->
                            List.map
                                (\entry ->
                                    case entry.result of
                                        Io.TypeDefinition old ->
                                            if old.name == new.name then
                                                { entry | result = Io.TypeDefinition { old | outdated = True } }

                                            else
                                                entry

                                        _ ->
                                            entry
                                )
                                model.history

                        _ ->
                            model.history
            in
            ( { model | history = enhanceEvaluatedTextInput data :: history }
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
        [ Html.header []
            [ Svg.svg []
                [ Svg.use [ Svg.Attributes.xlinkHref "#run-logo" ] []
                ]
            , Html.h1 []
                [ Html.text "Welcome to the elm.run REPL"
                ]
            , Theme.htmlSelectElement SelectedTheme
            ]
        , Html.div [ Html.Attributes.id "intro" ]
            [ Html.p []
                [ Html.text "I'm glad you are here. "
                , Html.text "How could I read, eval and print Elm code in a loop if noone were here to enter code? "
                ]
            , Html.p []
                [ Html.text "If you are unsure how to begin, we can start with "
                , Html.a [ Html.Attributes.href "#example", Html.Events.onClick ClickedExampleCode ] [ Html.text "example code" ]
                , Html.text ", or you can have a look at "
                , textLink { href = "https://elmcraft.org/learn", text = "learning materials" }
                , Html.text " for the "
                , textLink { href = "https://elm-lang.org", text = "Elm programming language" }
                , Html.text ", first."
                , Html.br [] []
                , Html.text "If you are all set, great!"
                ]
            , Html.p []
                [ Html.text "You can enter code below, and I will evaluate it and print results and type hints as you type."
                , Html.br [] []
                , Html.text "If you wish to see more than the hints, you can press Ctrl+Enter or the dedicated button to `run code` and I will print the full compiler output and add it to the history."
                , Html.br [] []
                , Html.text "You can create type definitions and functions, import packages from the "
                , textLink { href = "https://dark.elm.dmy.fr/packages/elm/core/latest", text = "Elm core libraries" }
                , Html.text ", and replace a definition or declaration by using the same name again."
                ]
            ]
        , Html.nav [ Html.Attributes.class "sticky" ]
            [ Html.span [ Html.Attributes.class "visible-if-sticky" ]
                [ Svg.svg [ Svg.Attributes.height "36", Svg.Attributes.width "36" ]
                    [ Svg.use [ Svg.Attributes.xlinkHref "#run-logo" ] []
                    ]
                , Html.text "elm.run REPL"
                ]
            , Html.menu [ Html.Attributes.class "center" ]
                [ Html.Extra.viewIfLazy (List.length model.history > 0)
                    (\() ->
                        Html.button [ Html.Events.onClick PressedClearButton ]
                            [ Icon.archiveBoxXMark [ Html.Attributes.Aria.ariaHidden True ]
                            , Html.text "clean up "
                            ]
                    )
                ]
            , Theme.htmlSelectElement SelectedTheme
            ]
        , Html.Keyed.ul [ Html.Attributes.class "logs monospace" ] <|
            List.map viewHistoryEntry <|
                List.reverse model.history
        , Html.div [ Html.Attributes.id "input-row" ]
            [ Html.button
                [ Html.Events.onClick TriggeredCompile
                , Html.Attributes.title "Run code"
                ]
                [ Icon.playCircle [ Html.Attributes.Aria.ariaHidden True ]
                , Html.text "run code"
                ]
            , inputBox model
            ]
        , Html.footer [] [ Html.small [] [ Html.text "The code you write here does not leave your browser." ] ]
        , viewModalDialog model
        ]


textLink : { href : String, text : String } -> Html msg
textLink { href, text } =
    Html.a [ Html.Attributes.href href, Html.Attributes.target "_blank" ]
        [ Html.text text ]


viewHistoryEntry : HistoryEntry -> ( String, Html Msg )
viewHistoryEntry { id, input, result } =
    let
        entry =
            case result of
                Io.Problems problems ->
                    [ viewCode input
                    , -- TODO enable jump to problem again?
                      -- Would need to replace the input, focus it and move the cursor.
                      -- The cursor position needs to be adapted because the entered REPL text is
                      -- wrapped into an Elm module.
                      Data.Problem.viewList (\_ -> NoOp) problems
                    ]

                Io.NoOutput ->
                    [ viewCode input ]

                Io.EvaluatedDeclaration { name, value, type_, outdated } ->
                    [ [ Html.Extra.viewIf outdated
                            (Html.div [ Html.Attributes.class "outdated" ]
                                [ Html.span [ Html.Attributes.class "label" ] [ Html.text "outdated:" ]
                                , Html.text <| " Replaced by another implementation of `" ++ name ++ "`"
                                ]
                            )
                      , viewCode input
                      ]
                    , viewValue value
                    , [ Html.text " : "
                      , viewType type_
                      ]
                    ]
                        |> List.concat

                Io.EvaluatedExpression { value, type_ } ->
                    viewCode input
                        :: viewValue value
                        ++ [ Html.text " : "
                           , viewType type_
                           ]

                Io.Import ->
                    [ viewCode input ]

                Io.TypeDefinition { name, outdated } ->
                    [ Html.Extra.viewIf outdated
                        (Html.div [ Html.Attributes.class "outdated" ]
                            [ Html.span [ Html.Attributes.class "label" ] [ Html.text "outdated:" ]
                            , Html.text <| " Replaced by another definition of `" ++ name ++ "`"
                            ]
                        )
                    , viewCode input
                    ]
    in
    ( String.fromFloat id
    , Html.li [] <|
        Html.menu []
            [ Html.button [ Html.Events.onClick <| PressedEditButton id ]
                [ Html.text "edit" ]
            , Html.button
                [ Html.Events.onClick <| PressedRemoveButton id
                , Html.Attributes.Aria.ariaLabel "Remove code"
                ]
                [ Html.span [ Html.Attributes.Aria.ariaHidden True ] [ Html.text "x" ] ]
            ]
            :: entry
    )


viewCode : String -> Html msg
viewCode code =
    Html.pre [] [ Html.text code ]


viewValue : AnsiExtra.Parsed -> List (Html msg)
viewValue value =
    AnsiExtra.view value


viewType : String -> Html msg
viewType str =
    Html.span [] [ Html.text str ]


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
