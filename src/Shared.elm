module Shared exposing
    ( Flags
    , Model
    , Msg
    , init
    , subscriptions
    , update
    )

import Json.Decode as Json
import Request exposing (Request)
import Storage
import Utils.Scroll as Scroll
import Utils.Viewport as Viewport


type alias Flags =
    Json.Value


type alias Model =
    { storage : Storage.Storage
    , inView : Scroll.Model
    , viewport : Viewport.Model
    }


init : Request -> Flags -> ( Model, Cmd Msg )
init _ flags =
    let
        ( inView_, inViewCmd_ ) =
            Scroll.init

        ( viewport_, viewportCmd_ ) =
            Viewport.init
    in
    ( { storage = Storage.fromJson flags
      , inView = inView_
      , viewport = viewport_
      }
    , Cmd.batch
        [ Cmd.map InViewUpdate inViewCmd_
        , Cmd.map ViewportUpdate viewportCmd_
        ]
    )


type Msg
    = StorageUpdated Storage.Storage
    | InViewUpdate Scroll.Msg
    | ViewportUpdate Viewport.Msg


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        StorageUpdated storage_ ->
            ( { model | storage = storage_ }
            , Cmd.none
            )

        InViewUpdate msg_ ->
            let
                ( inView_, cmd_ ) =
                    Scroll.update msg_ model.inView
            in
            ( { model | inView = inView_ }
            , Cmd.map InViewUpdate cmd_
            )

        ViewportUpdate msg_ ->
            let
                ( viewport_, cmd_ ) =
                    Viewport.update msg_ model.viewport
            in
            ( { model | viewport = viewport_ }
            , Cmd.map ViewportUpdate cmd_
            )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ model =
    Sub.batch
        [ Storage.onChange StorageUpdated
        , Scroll.subs model.inView
            |> Sub.map InViewUpdate
        , Viewport.subs model.viewport
            |> Sub.map ViewportUpdate
        ]
