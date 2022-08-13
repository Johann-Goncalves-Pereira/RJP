port module Utils.Scroll exposing (..)

import Components.NoteworthyProjects exposing (noteworthyProjectsDataIds)
import InView


port onScroll : ({ x : Float, y : Float } -> msg) -> Sub msg


type alias Model =
    { inView : InView.State
    , offset : { x : Float, y : Float }
    }


init : ( Model, Cmd Msg )
init =
    let
        ( inViewModel, inViewCmd ) =
            InView.init InViewMsg listIds
    in
    ( { inView = inViewModel
      , offset = { x = 0, y = 0 }
      }
    , inViewCmd
    )


type Msg
    = OnScroll { x : Float, y : Float }
    | InViewMsg InView.Msg
    | OnLoadElement String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnScroll offset ->
            ( { model
                | inView = InView.updateViewportOffset offset model.inView
                , offset = { x = offset.x, y = offset.y }
              }
            , Cmd.none
            )

        InViewMsg inViewMsg ->
            let
                ( inView_, cmd_ ) =
                    InView.update InViewMsg inViewMsg model.inView
            in
            ( { model | inView = inView_ }
            , cmd_
            )

        OnLoadElement id_ ->
            let
                ( inView, cmd_ ) =
                    InView.addElements InViewMsg [ id_ ] model.inView
            in
            ( { model | inView = inView }
            , cmd_
            )


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ InView.subscriptions InViewMsg model.inView
        , onScroll OnScroll
        ]


listIds : List String
listIds =
    [ "about"
    , "experience"
    , "work"
    , "other-noteworthy-projects"
    , "contact"
    ]
        ++ noteworthyProjectsDataIds
