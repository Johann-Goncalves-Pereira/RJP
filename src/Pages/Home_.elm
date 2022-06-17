module Pages.Home_ exposing (Model, Msg, page)

import Array exposing (Array)
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onResize)
import Components.Svg as SVG exposing (Logo(..))
import Gen.Params.Home_ exposing (Params)
import Gen.Route as Route
import Html exposing (Html, a, div, h1, h2, h5, header, li, nav, p, section, span, text, ul)
import Html.Attributes exposing (class, href, id, rel, tabindex, target)
import Html.Attributes.Aria exposing (ariaLabel, ariaLabelledby)
import Layout exposing (initLayout)
import Page
import Request
import Shared
import String exposing (right)
import Svg exposing (desc)
import Svg.Attributes exposing (orientation)
import Task
import Utils.View exposing (materialIcon)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page _ _ =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        }



-- INIT


type alias Model =
    { viewport : { w : Float, h : Float }
    }


init : ( Model, Cmd Msg )
init =
    ( { viewport = { w = 0, h = 0 } }
    , Task.perform GetViewport getViewport
    )



-- UPDATE


type Msg
    = GetViewport Viewport
    | GetNewViewport ( Float, Float )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        defaultViewport : { w : Float, h : Float }
        defaultViewport =
            { w = model.viewport.w
            , h = model.viewport.h
            }
    in
    case msg of
        GetViewport v_ ->
            let
                ( w_, h_ ) =
                    ( v_.viewport.width, v_.viewport.height )
            in
            ( { model
                | viewport =
                    { defaultViewport | w = w_, h = h_ }
              }
            , Cmd.none
            )

        GetNewViewport ( w_, h_ ) ->
            ( { model
                | viewport =
                    { defaultViewport | w = w_, h = h_ }
              }
            , Cmd.none
            )


subs : Model -> Sub Msg
subs _ =
    onResize (\w h -> GetNewViewport ( toFloat w, toFloat h ))



-- VIEW


view : Model -> View Msg
view model =
    { title = "Revex - Home"
    , body =
        Layout.viewLayout
            { initLayout
                | route = Route.Home_
                , headerContent = viewHeader model
                , mainAttrs =
                    [ if model.viewport.w <= 1024 then
                        class "main"

                      else
                        class ""
                    ]
                , mainContent = viewPage model
            }
    }


viewHeader : Model -> List (Html Msg)
viewHeader model =
    let
        correctZero =
            String.fromInt >> String.padLeft 2 '0'

        links =
            List.indexedMap
                (\i route ->
                    li []
                        [ a [ href <| "#" ++ route ++ "Id", class "list__link" ]
                            [ span [ class "text-accent-700" ] [ text <| correctZero i ++ ". " ], text route ]
                        ]
                )
                [ "about", "experience", "work", "contact" ]
    in
    [ materialIcon "icon" "hive"
    , nav []
        [ links
            ++ [ a [ href "#", class "list__resume" ] [ text "resume" ] ]
            |> ul [ class "list" ]
        ]
    ]


viewPage : Model -> List (Html Msg)
viewPage model =
    let
        content =
            [ div [ orientation "left", class "main-orientation left-0" ]
                [ div [ class "grid gap-10 select-none mt-auto" ]
                    [ materialIcon "text-3xl up" "south_america"
                    , materialIcon "text-3xl up" "fingerprint"
                    , materialIcon "text-3xl up" "all_inclusive"
                    , materialIcon "text-3xl up" "blur_on"
                    ]
                ]
            , viewMainContent model
                |> div [ class "main" ]
            , div [ orientation "right", class "main-orientation right-0" ]
                [ a [ class "email up" ] [ text "johann.gon.pereira@gmail.com" ]
                ]
            ]

        media =
            if model.viewport.w <= 1024 then
                viewMainContent model

            else
                content
    in
    media


viewMainContent : Model -> List (Html Msg)
viewMainContent _ =
    []
