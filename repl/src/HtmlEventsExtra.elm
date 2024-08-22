module HtmlEventsExtra exposing 
    ( onCustomEvent
    )

import Html
import Html.Events
import Json.Decode


onCustomEvent : String -> Json.Decode.Decoder a -> Html.Attribute a
onCustomEvent event decoder =
    Html.Events.on event (Json.Decode.field "detail" decoder)
