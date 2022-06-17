module Pages.Home_ exposing (Model, Msg, page)

import Array exposing (Array)
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onResize)
import Components.Svg as SVG exposing (Logo(..))
import Gen.Params.Home_ exposing (Params)
import Gen.Route as Route
import Html exposing (Html, a, button, div, h1, h2, h5, header, li, nav, p, section, span, text, ul)
import Html.Attributes exposing (attribute, class, href, id, rel, tabindex, target)
import Html.Attributes.Aria exposing (ariaLabel, ariaLabelledby)
import Html.Events exposing (onCheck, onClick)
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
    , showNav : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { viewport = { w = 0, h = 0 }
      , showNav = False
      }
    , Task.perform GetViewport getViewport
    )



-- UPDATE


type Msg
    = -- Page
      GetViewport Viewport
    | GetNewViewport ( Float, Float )
      -- Item
    | ShowNav Bool


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

        ShowNav toggler_ ->
            ( { model | showNav = not toggler_ }, Cmd.none )


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
                            [ span [ class "text-accent-600" ]
                                [ text <| correctZero i ++ ". " ]
                            , text route
                            ]
                        ]
                )
                [ "about", "experience", "work", "contact" ]

        checkNav =
            if model.showNav then
                { state = "check" }

            else
                { state = "uncheck" }
    in
    [ materialIcon "icon" "hive"
    , if model.viewport.w <= 1024 then
        button
            [ class <| "nav-toggler " ++ checkNav.state
            , onClick <| ShowNav model.showNav
            ]
            [ materialIcon "nav-toggler__icon segment" "segment"
            , materialIcon "nav-toggler__icon close" "close"
            ]

      else
        text ""
    , nav [ class <| "nav " ++ checkNav.state ]
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
            , div [ orientation "right", class "main-orientation right-0" ]
                [ a [ class "email up" ] [ text "johann.gon.pereira@gmail.com" ]
                ]
            ]

        media =
            if model.viewport.w <= 1024 then
                viewMainContent model
                    |> List.singleton

            else
                content
    in
    media


viewMainContent : Model -> Html Msg
viewMainContent model =
    let
        textSize width str =
            if model.viewport.w >= width then
                str

            else
                ""
    in
    div [ class "main w-[min(100vw_-_2rem,1920px)] lg:w-full mx-auto z-10" ]
        [ section [ class "grid place-content-center gap-5 h-screen  m-auto" ]
            [ Html.i [ class "font-mono text-accent-600 text-sm" ]
                [ text "hi, my name is" ]
            , h1 [ class "text-7xl font-800" ]
                [ [ "Johann", textSize 1920 "Gonçalves", "Pereira" ]
                    |> String.join " "
                    |> text
                ]
            , h2 [ class "text-7xl font-800" ]
                [ [ "I", textSize 1920 "love to", "build", textSize 1440 "things", "for the web." ]
                    |> String.join " "
                    |> text
                ]
            , p [ class "inline-block text-surface-400 w-gold-paragraph" ]
                [ text """I’m a software developer specializing in
             building (and occasionally designing) exceptional digital experiences. Currently,
              I'm focused on building the plataform for """
                , a [ class "text-accent-600", href "https://app.materialize.pro" ] [ text "Materialize" ]
                , text "."
                ]
            , a [ class "btm-accent", href "https://github.com/Johann-Goncalves-Pereira" ] [ text "Check my Github" ]
            ]
        ]
