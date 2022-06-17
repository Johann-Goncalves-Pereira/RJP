port module Utils.Scroll exposing (..)


port onScroll : ({ x : Float, y : Float } -> msg) -> Sub msg


type alias Model =
    { scrollPos : { x : Float, y : Float } }


init : Model
init =
    { scrollPos = { x = 0, y = 0 } }


type Msg
    = OnScroll { x : Float, y : Float }


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnScroll offset ->
            { model | scrollPos = { x = offset.x, y = offset.y } }


subScroll : Sub Msg
subScroll =
    onScroll OnScroll
