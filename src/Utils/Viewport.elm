module Utils.Viewport exposing (..)

import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onResize)
import Task


type alias Model =
    { width : Float, height : Float }


init : ( Model, Cmd Msg )
init =
    ( { width = 0, height = 0 }
    , Task.perform GetViewport getViewport
    )


type Msg
    = GetViewport Viewport
    | GetNewViewport Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetViewport viewport_ ->
            let
                v_ =
                    viewport_.viewport

                ( w_, h_ ) =
                    ( v_.width, v_.height )
            in
            ( { model
                | width = w_
                , height = h_
              }
            , Cmd.none
            )

        GetNewViewport w_ h_ ->
            ( { model
                | width = toFloat w_
                , height = toFloat h_
              }
            , Cmd.none
            )


subs : Model -> Sub Msg
subs _ =
    onResize GetNewViewport
