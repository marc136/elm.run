module ModalDialog exposing (view)

-- Adapted from https://discourse.elm-lang.org/t/native-html-element-dialog-modal-opening-closing/9232

import Html exposing (Html, div, node)
import Html.Attributes exposing (attribute, class)
import Html.Events
import Json.Decode


onClose : msg -> Html.Attribute msg
onClose message =
    Html.Events.on "close" (Json.Decode.succeed message)


view : Bool -> msg -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
view isOpen closeMsg attributes children =
    node "modal-dialog"
        (if isOpen then
            [ attribute "open" "" ]

         else
            []
        )
        [ node "dialog" (onClose closeMsg :: attributes) <|
            div [ class "modal-filler" ] []
                :: children
        ]
