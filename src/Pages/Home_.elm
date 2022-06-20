module Pages.Home_ exposing (Model, Msg, page)

import Browser.Dom as BrowserDom exposing (Element, Error, Viewport, getElement, getViewport, setViewport)
import Browser.Events exposing (onResize)
import Components.Svg as SVG exposing (Logo(..))
import Gen.Params.Home_ exposing (Params)
import Gen.Route as Route
import Html exposing (Attribute, Html, a, br, button, div, footer, h1, h2, h3, h5, header, img, li, nav, p, section, span, text, ul)
import Html.Attributes exposing (alt, class, classList, href, id, rel, src, tabindex, target)
import Html.Attributes.Aria exposing (ariaLabelledby)
import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse as Mouse
import Layout exposing (initLayout, rootId)
import Page
import Request
import Round
import Shared
import String exposing (right)
import Svg exposing (desc)
import Svg.Attributes exposing (orientation)
import Task
import Utils.Scroll as Scroll
import Utils.View exposing (customProps, materialIcon)
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
    { -- Page Events
      viewport : { w : Float, h : Float }
    , scroll : Scroll.Model

    -- Element States
    , showNav : Bool
    , imageOver : Bool
    , workSelected : Int
    , mousePos : { x : Float, y : Float }

    -- Elements
    , sectionOne : { w : Float, h : Float }
    , sectionPlace : List { id : String, y : Float }
    }


init : ( Model, Cmd Msg )
init =
    ( { -- Page Events
        viewport = { w = 0, h = 0 }
      , scroll = Scroll.init

      -- Element States
      , showNav = False
      , imageOver = False
      , workSelected = 0
      , mousePos = { x = 0, y = 0 }

      -- Elements
      , sectionOne = { w = 0, h = 0 }
      , sectionPlace = []
      }
    , Cmd.batch
        [ Task.perform GetViewport getViewport
        , BrowserDom.getElement secOneId
            |> Task.attempt GetSectionSize
        ]
    )


getSectionPos =
    List.map
        (\x ->
            BrowserDom.getElement x
                |> Task.attempt GetSectionSize
        )



-- UPDATE


type Msg
    = -- Page Events
      NoOp
    | GetViewport Viewport
    | GetNewViewport ( Float, Float )
    | ScrollMsg Scroll.Msg
    | GoToSection Float
      -- Element States
    | ShowNav Bool
    | ImageOver Bool
    | SelectWork Int
    | NewMousePos ( Float, Float )
      -- Elements
    | GetSectionSize (Result Error Element)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        defaultViewport =
            { w = model.viewport.w
            , h = model.viewport.h
            }
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GetViewport v_ ->
            let
                ( w_, h_ ) =
                    ( v_.viewport.width, v_.viewport.height )
            in
            ( { model
                | viewport =
                    { defaultViewport | w = w_, h = h_ }
              }
            , BrowserDom.getElement secOneId
                |> Task.attempt GetSectionSize
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
                | scroll =
                    Scroll.update msg_ model.scroll
              }
            , Cmd.none
            )

        GoToSection y_ ->
            ( model
            , Task.attempt (\_ -> NoOp) <|
                setViewport 0 y_
            )

        ShowNav toggler_ ->
            ( { model | showNav = not toggler_ }, Cmd.none )

        ImageOver isOver_ ->
            ( { model | imageOver = isOver_ }, Cmd.none )

        SelectWork selected_ ->
            ( { model | workSelected = selected_ }, Cmd.none )

        NewMousePos ( x_, y_ ) ->
            ( { model | mousePos = { x = x_, y = y_ } }, Cmd.none )

        GetSectionSize result_ ->
            case result_ of
                Ok e_ ->
                    let
                        ( w_, h_ ) =
                            ( e_.element.width, e_.element.height )
                    in
                    ( { model | sectionOne = { w = w_, h = h_ } }, Cmd.none )

                Err err_ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ onResize <| \w h -> GetNewViewport ( toFloat w, toFloat h )

        -- , Sub.map ScrollMsg Scroll.subScroll
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


correctZero : Int -> String
correctZero =
    String.fromInt >> String.padLeft 2 '0'


viewHeader : Model -> List (Html Msg)
viewHeader model =
    let
        links =
            List.indexedMap
                (\i route ->
                    li []
                        [ a
                            [ href <| "#" ++ route ++ "Id"
                            , class "list__link"

                            -- , onClick <| GoToSection 900
                            ]
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
            ++ [ li [] [ a [ href "#", class "list__resume" ] [ text "resume" ] ] ]
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
    div [ class "main grid gap-10 w-[min(100vw_-_2rem,1920px)] lg:w-full mx-auto z-10" ]
        [ viewSectionOne model
        , viewSectionTwo model
        , viewSectionThree model
        ]


secOneId : String
secOneId =
    "section-one-id"


viewSectionOne : Model -> Html Msg
viewSectionOne model =
    let
        textSize width str =
            if model.viewport.w >= width then
                str

            else
                ""

        e =
            { x = model.mousePos.x
            , y = model.mousePos.y
            , w = model.sectionOne.w / 2
            , h = model.sectionOne.h / 2
            }

        calc =
            { x = (e.x - e.w) / e.w
            , y = (e.y - e.h) / e.h
            }

        r v_ =
            String.concat [ Round.round 3 v_, "rem" ]

        value =
            { x1 = r calc.x
            , y1 = r calc.y
            , x2 = r <| calc.x * -2
            , y2 = r <| calc.y * -2
            }
    in
    section
        [ class "grid place-content-center min-h-screen w-full m-auto select-none py-24 isolate"
        , ariaLabelledby "title--name"
        , customProps
            [ { prop = "pos-x-1", value = value.x1 }
            , { prop = "pos-y-1", value = value.y1 }
            , { prop = "pos-x-2", value = value.x2 }
            , { prop = "pos-y-2", value = value.y2 }
            ]
        , Mouse.onMove (.offsetPos >> NewMousePos)
        ]
        [ div [ class "secOne grid place-content-center gap-5", id secOneId ]
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
            , p [ class "inline-block text-surface-400 sm:w-gold-paragraph" ]
                [ text """I’m a software developer specializing in
             building (and occasionally designing) exceptional digital experiences. Currently,
              I'm focused on building the plataform for """
                , a [ class "text-accent-600", href "https://app.materialize.pro" ] [ text "Materialize" ]
                , text "."
                ]
            , a [ class "btm-accent mt-8", href "https://github.com/Johann-Goncalves-Pereira" ]
                [ text "Check my GitHub" ]
            ]
        ]


headerSection : String -> Int -> String -> Html Msg
headerSection addClass sectNumber title =
    header [ class <| "header-section " ++ addClass ]
        [ Html.i [ class "header-section__number" ] [ text <| correctZero sectNumber ++ "." ]
        , h3 [ class "header-section__title" ] [ text title ]
        ]


viewSectionTwo : Model -> Html Msg
viewSectionTwo model =
    section [ class "about-me", id "aboutId" ]
        [ headerSection "" 1 "About Me"
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
            , br [] []
            , br [] []
            , text """Amongst the hundreds of thousands of symbols which are in the unicode 
                text specifications are certain characters which resemble, or are variations of 
                the alphabet and other keyword symbols. For example, if we can take the phrase 
                "thug life" and convert its characters into the fancy letters "𝖙𝖍𝖚𝖌 𝖑𝖎𝖋𝖊" which 
                are a set of unicode symbols. These different sets of fancy text letters are 
                scattered all throughout the unicode specification, and so to create a fancy 
                text translator, it's just a matter of finding these sets of letters and symbols, 
                and linking them to their normal alphabetical equivalents.
                """
            , br [] []
            , br [] []
            , text
                """
                Unicode has a huge number of symbols, and so we're able to create other 
                things like a wingdings translator too. Also if you're looking for messy 
                text, or glitchy text, visit this creepy zalgo text generator (another 
                translator on LingoJam).
                an actual font?"""
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
                    [ "elm"
                    , "html - css/sass"
                    , "design - figma"
                    , "Js - Ts - React"
                    , "shell (bash, zsh)"
                    , "go (golang)"
                    ]
            ]
        , div
            [ classList [ ( "img", True ), ( "hover", model.imageOver ) ] ]
            [ img [ src "https://picsum.photos/1200", alt "Profile Photo" ] [] ]
        ]


viewSectionThree : Model -> Html Msg
viewSectionThree model =
    let
        listWork =
            List.indexedMap
                (\i x ->
                    li
                        [ classList
                            [ ( "work-list__item", True )
                            , ( "work-list__item--selected", i == model.workSelected )
                            ]
                        , onClick <| SelectWork i
                        ]
                        [ text x ]
                )
                [ "elm", "skljdl", "kjdçslkjd", "dlkjsalk", "djslk" ]
    in
    section [ class "where-have-i-worked", id "experienceId" ]
        [ headerSection "" 2 "Where I’ve Worked"
        , listWork
            |> ul
                [ class <|
                    String.concat
                        [ "work-list "
                        , "work-list--"
                        , String.fromInt model.workSelected
                        ]
                ]
        ]
