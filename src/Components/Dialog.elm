port module Components.Dialog exposing (..)

import Html exposing (Attribute, Html, node)
import Html.Attributes as Attr
import Html.Events exposing (on)
import Json.Decode exposing (succeed)


port toggleDialog : String -> Cmd msg


dialog : String -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
dialog elementId attr content =
    Html.node "dialog" (Attr.id elementId :: attr) content


type Msg
    = ToggleDialog String


update : Msg -> Cmd Msg
update msg =
    case msg of
        ToggleDialog id_ ->
            toggleDialog id_
