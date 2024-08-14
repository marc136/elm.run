module Theme exposing
    ( Theme(..)
    , decoder
    , toAttribute
    , toString
    )

import Html
import Html.Attributes
import TsJson.Codec exposing (Codec)
import TsJson.Decode exposing (Decoder)


type Theme
    = Light
    | Dark


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
