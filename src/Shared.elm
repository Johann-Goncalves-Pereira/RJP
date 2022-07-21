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


type alias Flags =
    Json.Value


type alias Model =
    { storage : Storage.Storage
    , inView : Scroll.Model
    }


init : Request -> Flags -> ( Model, Cmd Msg )
init _ flags =
    let
        ( inView_, inViewCmd_ ) =
            Scroll.init
    in
    ( { storage = Storage.fromJson flags
      , inView = inView_
      }
    , Cmd.map InViewUpdate inViewCmd_
    )


type Msg
    = StorageUpdated Storage.Storage
    | InViewUpdate Scroll.Msg


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


subscriptions : Request -> Model -> Sub Msg
subscriptions _ model =
    Sub.batch
        [ Storage.onChange StorageUpdated
        , Scroll.subs model.inView
            |> Sub.map InViewUpdate
        ]
