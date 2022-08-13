module Components.NoteworthyProjects exposing (..)

import Components.Svg as ESvg
import Html exposing (..)
import Html.Attributes as Attr
import Html.Attributes.Aria exposing (ariaLabelledby)
import Html.Events exposing (onClick)
import Utils.View exposing (customProp, materialIcon)
import Utils.Viewport as Viewport


type alias Model =
    { showMore : Bool
    , viewport : Viewport.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( viewport_, viewportCmd_ ) =
            Viewport.init
    in
    ( { showMore = True
      , viewport = viewport_
      }
    , Cmd.map ViewportUpdate viewportCmd_
    )


type Msg
    = ShowMore Bool
    | ViewportUpdate Viewport.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowMore toggle ->
            ( { model | showMore = not toggle }, Cmd.none )

        ViewportUpdate msg_ ->
            let
                ( viewport_, cmd_ ) =
                    Viewport.update msg_ model.viewport
            in
            ( { model | viewport = viewport_ }
            , Cmd.map ViewportUpdate cmd_
            )


viewNoteworthy : Model -> List String -> Html Msg
viewNoteworthy model listIds =
    let
        showMore =
            if model.showMore then
                "Less"

            else
                "More"
    in
    section [ Attr.class "other-noteworthy-projects", ariaLabelledby "header-noteworthy" ]
        [ header [ Attr.class "grid place-items-center gap-5" ]
            [ h4 [ Attr.class "text-4xl text-center font-800", Attr.tabindex 0, Attr.id "header-noteworthy" ]
                [ text "Other Noteworthy Projects" ]
            , p []
                [ text "View the my side projects" ]
            ]
        , viewNoteworthyProjects model listIds
            |> ul [ Attr.class "grid grid-cols-fit-20 auto-rows-fr gap-6" ]
        , button
            [ Attr.class "btm-accent mx-auto"
            , Attr.tabindex 0
            , onClick <| ShowMore model.showMore
            ]
            [ text <| String.join " " [ "View", showMore ] ]
        ]


viewNoteworthyProjects : Model -> List String -> List (Html Msg)
viewNoteworthyProjects model listIds =
    let
        v_ =
            if model.viewport.width <= 1024 then
                1

            else if model.viewport.width <= 1440 then
                2

            else
                3

        modMedia i =
            modBy v_ i
    in
    List.indexedMap
        (\i { gitHubUrl, projectUlr, title, desc, tags } ->
            let
                head_ =
                    "header--noteworthy--" ++ String.fromInt i

                link_ url_ icon_ =
                    a
                        [ Attr.class "link"
                        , Attr.href url_
                        , Attr.tabindex 0
                        ]
                        [ icon_ ]

                gitHub_ =
                    case gitHubUrl of
                        Nothing ->
                            text ""

                        Just url_ ->
                            link_ url_ <| ESvg.github ""

                siteUrl =
                    case projectUlr of
                        Nothing ->
                            text ""

                        Just url_ ->
                            link_ url_ <| materialIcon "drop-shadow" "open_in_new"

                delay_ =
                    String.fromInt (modMedia i * 200)
                        ++ "ms"
                        |> customProp "in-view-delay"

                generalUrl =
                    if projectUlr == Nothing then
                        Maybe.withDefault "" gitHubUrl

                    else
                        Maybe.withDefault "" projectUlr

                isInView_ =
                    String.contains (noteworthyId i) (String.join " " listIds)
            in
            li
                [ Attr.classList
                    [ ( "card-item view", True )
                    , ( "view--in", isInView_ )
                    , ( "view--out", not isInView_ )
                    ]
                , Attr.id <| noteworthyId i
                , Attr.tabindex 0
                , delay_
                ]
                [ a
                    [ Attr.class "card"
                    , Attr.href generalUrl
                    , ariaLabelledby head_
                    , Attr.target "_blank"
                    ]
                    [ div [ Attr.class "card__wrapper " ]
                        [ materialIcon "folder" "folder"
                        , gitHub_
                        , siteUrl
                        ]
                    , h6 [ Attr.class "card__title", Attr.id head_ ] [ text title ]
                    , p [] [ text desc ]
                    , ul [ Attr.class "card__list" ] <|
                        List.map (\itemText -> li [] [ text itemText ])
                            tags
                    ]
                ]
        )
        (if model.showMore then
            noteworthyProjectsData

         else
            List.take (max 4 <| v_ * 2) noteworthyProjectsData
        )


noteworthyId : Int -> String
noteworthyId idx_ =
    let
        correctZero =
            String.padLeft 2 '0'
    in
    "noteworthy--"
        ++ correctZero (String.fromInt idx_)


noteworthyProjectsDataIds : List String
noteworthyProjectsDataIds =
    List.indexedMap (\i _ -> noteworthyId i) noteworthyProjectsData


noteworthyProjectsData :
    List
        { gitHubUrl : Maybe String
        , projectUlr : Maybe String
        , title : String
        , desc : String
        , tags : List String
        }
noteworthyProjectsData =
    [ { gitHubUrl = Just "https://github.com/Johann-Goncalves-Pereira/the-greate-outdoors"
      , projectUlr = Just "https://app.netlify.com/sites/tourmaline-conkies-5e8bc9/overview"
      , title = "Out Doors Website"
      , desc = """A simple website for a company that sells outdoor gear.
          It's just the home page is responsive and super beautiful.
          Design coped see on repository where."""
      , tags = [ "Elm", "Sass", "Netlify" ]
      }
    , { gitHubUrl = Just "https://github.com/Johann-Goncalves-Pereira/Excel-Week"
      , projectUlr = Nothing
      , title = "Excel on Practice"
      , desc = """A freelance that I worked on, It's a homepage to sell a week course, 
      of how to use excel excel, I was super cool make a in 3 days."""
      , tags = [ "ReactJs", "Sass", "ViteJs" ]
      }
    , { gitHubUrl = Just "https://github.com/Johann-Goncalves-Pereira/Grid-News"
      , projectUlr = Just "https://62e8319e17cd161f972e91ae--luminous-clafoutis-c0bf0f.netlify.app"
      , title = "Grid News"
      , desc = """Website with a grid system that I like, so I made my version.
      The original site is on the description of the github project. Not finished."""
      , tags = [ "Elm", "PostCss", "Tailwind", "Sass" ]
      }
    , { gitHubUrl = Just "https://github.com/Johann-Goncalves-Pereira/bevy-snake"
      , projectUlr = Nothing
      , title = "Snake Game Build with Rust"
      , desc = """An Game made with Bevy, a Rust Framework.
       Just to learn how to use it."""
      , tags = [ "Rust", "Bevy", "Cargo" ]
      }
    , { gitHubUrl = Just "https://github.com/Johann-Goncalves-Pereira/shell-config"
      , projectUlr = Nothing
      , title = "My Shell Config"
      , desc = """The Terminal is the most personal thing of a developer,
       in this project I made a config for my shell as me."""
      , tags = [ "ZShel", "O-My-ZShell", "p10k" ]
      }
    , { gitHubUrl = Just "https://github.com/Johann-Goncalves-Pereira/Developers-Concept"
      , projectUlr = Just "https://app.netlify.com/sites/jgp-pnco/overview"
      , title = "Developers Concept"
      , desc = """This was an attempt to make a portfolio that fail."""
      , tags = [ "Elm", "Elm-spa", "Sass", "Tailwind" ]
      }
    , { gitHubUrl = Just "https://github.com/Johann-Goncalves-Pereira/Kelpie"
      , projectUlr = Nothing
      , title = "Kelpie"
      , desc = """My first website made with Elm,
       I was super cool to make, and super hard because Elm,
        its super hard at start. The site is a copy of Unsplash."""
      , tags = [ "Elm", "Webpack", "Sass", "Tailwind" ]
      }
    , { gitHubUrl = Just "https://github.com/Johann-Goncalves-Pereira/elm-spotify"
      , projectUlr = Just "https://medium.com/@johann.gon.pereira/building-spotify-with-elm-00-b18b6bf815d7"
      , title = "Elm Spotify"
      , desc = """The elm-spotify was an attempt to make a 
      clone of spotify width elm. Not just that, I try to document on Medium and Dev.io."""
      , tags = [ "Elm", "Docker", "Vite", "Sass" ]
      }
    , { gitHubUrl = Just "https://github.com/Johann-Goncalves-Pereira/Email-Constructor"
      , projectUlr = Nothing
      , title = "Email Constructor"
      , desc = """I use this project to make the html of emails"""
      , tags = [ "Elm", "Email", "PostCss", "Sass" ]
      }
    , { gitHubUrl = Just "https://github.com/Johann-Goncalves-Pereira/calc-flutter"
      , projectUlr = Nothing
      , title = "Calc Flutter"
      , desc = """I try to learn flutter, so I made this calculator."""
      , tags = [ "Flutter", "Dart" ]
      }
    , { gitHubUrl = Just "https://github.com/Johann-Goncalves-Pereira/Timer-Countdown"
      , projectUlr = Nothing
      , title = "Timer Countdown"
      , desc = """Timer Countdown is a simple timer that I made with Go(golang),
       I don't know why huahauahua."""
      , tags = [ "Flutter", "Dart" ]
      }
    , { gitHubUrl = Just "https://github.com/Johann-Goncalves-Pereira/flus"
      , projectUlr = Nothing
      , title = "Flus"
      , desc = """A app made with React Native."""
      , tags = [ "ReactNative", "Typescript" ]
      }
    ]
