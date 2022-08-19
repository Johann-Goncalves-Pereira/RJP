port module Utils.Scroll exposing (..)

import Browser.Events exposing (onResize)
import Components.NoteworthyProjects exposing (noteworthyProjectsDataIds)
import Components.ThingsThatIHaveBuild as ThingsThatIHaveBuild
import Html exposing (Attribute)
import Html.Events exposing (onClick)
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
    | GetAgain


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

        GetAgain ->
            let
                ( model_, cmd_ ) =
                    InView.addElements InViewMsg listIds model.inView
            in
            ( { model | inView = model_ }, cmd_ )


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ InView.subscriptions InViewMsg model.inView
        , onResize <| \_ _ -> GetAgain
        , onScroll OnScroll
        ]


listIds : List String
listIds =
    [ "about"
    , "experience"
    , "work"
    , "contact"
    ]
        ++ noteworthyProjectsDataIds
        ++ ThingsThatIHaveBuild.ids
