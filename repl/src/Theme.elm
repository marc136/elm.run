module Theme exposing
    ( Theme(..)
    , decoder
    , htmlSelectElement
    , toAttribute
    , toString
    )

import Html exposing (Html)
import Html.Attributes
import HtmlEventsExtra
import Json.Decode
import TsJson.Codec exposing (Codec)
import TsJson.Decode exposing (Decoder)


type Theme
    = Light
    | Dark


htmlSelectElement : (Theme -> msg) -> Html msg
htmlSelectElement toMsg =
    Html.node "scheme-selector"
        [ HtmlEventsExtra.onCustomEvent "prefers-color-scheme"
            (Json.Decode.map toMsg (TsJson.Decode.decoder decoder))
        ]
        []


decoder : Decoder Theme
decoder =
    TsJson.Codec.decoder codec


codec : Codec Theme
codec =
    TsJson.Codec.stringUnion [ ( "dark", Dark ), ( "light", Light ) ]


toAttribute : Theme -> Html.Attribute msg
toAttribute =
    Html.Attributes.attribute "theme" << toString


toString : Theme -> String
toString theme =
    case theme of
        Dark ->
            "dark"

        Light ->
            "light"
