module Pages.Home_ exposing (Model, Msg, page)

import Array exposing (Array)
import Browser.Dom as BrowserDom exposing (Viewport, getViewport)
import Browser.Events exposing (onResize)
import Components.Svg as SVG exposing (Logo(..))
import Gen.Params.Home_ exposing (Params)
import Gen.Route as Route
import Html exposing (Attribute, Html, a, br, button, div, footer, h1, h2, h3, h5, header, img, li, nav, p, section, span, text, ul)
import Html.Attributes exposing (attribute, class, href, id, rel, src, tabindex, target)
import Html.Attributes.Aria exposing (ariaLabel, ariaLabelledby)
import Html.Events exposing (on, onCheck, onClick)
import Layout exposing (initLayout, rootId)
import Page
import Request
import Shared
import String exposing (right)
import Svg exposing (desc)
import Svg.Attributes exposing (orientation)
import Task
import Utils.Scroll as Scroll
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
    , scroll : Scroll.Model
    , showNav : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { viewport = { w = 0, h = 0 }
      , scroll = Scroll.init
      , showNav = False
      }
    , Task.perform GetViewport getViewport
    )



-- UPDATE


type Msg
    = -- Page Events
      GetViewport Viewport
    | GetNewViewport ( Float, Float )
    | ScrollMsg Scroll.Msg
      -- Item
    | ShowNav Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
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

        ScrollMsg msg_ ->
            ( { model
                | scroll = Scroll.update msg_ model.scroll
              }
            , Cmd.none
            )

        ShowNav toggler_ ->
            ( { model | showNav = not toggler_ }, Cmd.none )


subs : Model -> Sub Msg
subs _ =
    Sub.batch
        [ onResize (\w h -> GetNewViewport ( toFloat w, toFloat h ))
        , Sub.map ScrollMsg Scroll.subScroll
        ]



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
        [ if model.viewport.w <= 1024 then
            div [ onClick <| ShowNav model.showNav ] []

          else
            text ""
        , links
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
            if model.viewport.w <= 1024 || model.viewport.h <= 480 then
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
        [ section
            [ class "grid place-content-center gap-5 min-h-screen m-auto select-none py-24"
            , ariaLabelledby "title--name"
            ]
            [ Html.i [ class "font-mono text-accent-600 text-sm" ]
                [ text "hi, my name is" ]
            , h1 [ class "text-7xl font-800", id "title--name" ]
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
            , a [ class "btm-accent mt-8", href "https://github.com/Johann-Goncalves-Pereira" ]
                [ text "Check my GitHub" ]
            ]
        , section [ class "about-me" ]
            [ header [ class "header" ]
                [ Html.i [ class "header__number" ] [ text "01." ]
                , h3 [ class "header__title" ] [ text "About Me" ]
                ]
            , p [ class "paragraph" ]
                [ text """So perhaps, you've generated some fancy text, 
                and you're content that you can now copy and paste your fancy 
                text in the comments section of funny cat videos, but perhaps 
                you're wondering how it's even possible to change the font of 
                your text? Is it some sort of hack? Are you copying and pasting 
                an actual font?"""
                , br [] []
                , br [] []
                , text """Well, the answer is actually no - rather than generating fancy fonts, 
                this converter creates fancy symbols. The explanation starts with unicode; 
                an industry standard which creates the specification for thousands of different 
                symbols and characters. All the characters that you see on your 
                electronic devices, and printed in books, are likely specified by the 
                unicode standard."""
                ]
            , footer [ class "footer" ]
                [ ul [ class "footer__list" ] <|
                    List.map
                        (\x ->
                            li [ class "footer__item" ]
                                [ materialIcon "footer__icon" "arrow_right"
                                , text x
                                ]
                        )
                        [ "elm", "css/sass/scss", "html", "docker", "shell (bash, zsh)", "go (golang)" ]
                ]
            , div [ class "img" ] [ img [ src "https://picsum.photos/1200" ] [] ]
            ]
        ]
