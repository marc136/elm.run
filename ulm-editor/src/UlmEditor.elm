module UlmEditor exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Extra
import InteropDefinitions
import InteropPorts
import Json.Decode



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
    , lastDocument : Maybe String
    }



-- INIT


init : Json.Decode.Value -> ( Model, Cmd Msg )
init json =
    case InteropPorts.decodeFlags json of
        Ok flags ->
            ( Editor
                { file = flags.file
                , lastDocument = Nothing
                }
            , Cmd.none
            )

        Err err ->
            ( InitFailure err, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | CompiledNewDocument String
    | ToElm (Result Json.Decode.Error InteropDefinitions.ToElm)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( NoOp, _ ) ->
            ( model, Cmd.none )

        ( ToElm (Err _), _ ) ->
            ( model, Cmd.none )

        ( ToElm (Ok _), _ ) ->
            ( model, Cmd.none )

        ( CompiledNewDocument url, Editor model_ ) ->
            ( Editor { model_ | lastDocument = Just url }
            , model_.lastDocument
                |> Maybe.map (InteropDefinitions.RevokeObjectUrl >> InteropPorts.fromElm)
                |> Maybe.withDefault Cmd.none
            )

        ( CompiledNewDocument _, _ ) ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map ToElm InteropPorts.toElm



-- VIEW


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
        ]
        [ Html.node "ulm-editor"
            [ Html.Attributes.attribute "file" model.file
            , onCustomEvent compileResultEvent compileResultDecoder
            ]
            []
        , Html.Extra.viewMaybe
            (\doc -> Html.iframe [ Html.Attributes.src doc ] [])
            model.lastDocument
        ]


onCustomEvent : String -> Json.Decode.Decoder a -> Html.Attribute a
onCustomEvent event decoder =
    Html.Events.on event (Json.Decode.field "detail" decoder)


compileResultDecoder : Json.Decode.Decoder Msg
compileResultDecoder =
    Json.Decode.field "type" Json.Decode.string
        |> Json.Decode.andThen compileResultDecoder2


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
            Json.Decode.fail <| compileResultEvent ++ " with type='" ++ type_ ++ "' is not supported"
