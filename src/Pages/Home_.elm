module Pages.Home_ exposing (Model, Msg, page)

import Array exposing (Array)
import Browser.Dom as BrowserDom exposing (Element, Error, Viewport, getElement, getViewport, setViewport)
import Browser.Events exposing (onResize)
import Components.Svg as SVG exposing (Logo(..))
import Dict exposing (Dict)
import Gen.Params.Home_ exposing (Params)
import Gen.Route as Route
import Html
    exposing
        ( Attribute
        , Html
        , a
        , br
        , button
        , div
        , footer
        , h1
        , h2
        , h3
        , h4
        , h5
        , header
        , img
        , li
        , nav
        , p
        , section
        , span
        , strong
        , text
        , ul
        )
import Html.Attributes as HA exposing (alt, attribute, class, classList, href, id, rel, src, tabindex, target)
import Html.Attributes.Aria exposing (ariaLabelledby)
import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Wheel as Wheel exposing (onWheel)
import Layout exposing (initLayout, rootId)
import Page
import Request
import Round
import Shared
import String exposing (right)
import Svg exposing (desc)
import Svg.Attributes exposing (orientation)
import Task
import Utils.Func exposing (aplR)
import Utils.Models as Models
import Utils.Scroll as Scroll
import Utils.View exposing (customProp, customProps, materialIcon)
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
    , theme : { scheme : Theme, hue : Int }

    -- Element States
    , showNav : Bool
    , imageOver : Bool
    , workSelected : Int
    , mousePos : { x : Float, y : Float }
    , wheelDelta : Bool

    -- Elements
    , sectionOne : { w : Float, h : Float }
    , elementsPosition : Dict String Float

    -- Models
    , thingsThatIBuild : Models.ThingsThatIBuild
    }


init : ( Model, Cmd Msg )
init =
    ( { -- Page Events
        viewport = { w = 0, h = 0 }
      , scroll = Scroll.init
      , theme = { scheme = Dark, hue = 0 }

      -- Element States
      , showNav = False
      , imageOver = False
      , workSelected = 0
      , mousePos = { x = 0, y = 0 }
      , wheelDelta = False

      -- Elements
      , sectionOne = { w = 0, h = 0 }
      , elementsPosition = Dict.empty

      -- Models
      , thingsThatIBuild = Models.defaultThingsThatIBuild
      }
    , Cmd.batch
        [ Task.perform GetViewport getViewport
        , BrowserDom.getElement secOneId
            |> Task.attempt GetSectionSize
        , getSectionPos listIds
        ]
    )


getSectionPos : List String -> Cmd Msg
getSectionPos =
    List.map
        (\idName ->
            BrowserDom.getElement idName
                |> Task.attempt (GotElementPosition idName)
        )
        >> Cmd.batch


type Theme
    = Dark
    | Light



-- UPDATE


type Msg
    = -- Page Events
      NoOp
    | GetViewport Viewport
    | GetNewViewport ( Float, Float )
    | ScrollMsg Scroll.Msg
    | WheelDelta Bool
    | GoToSection Float
    | ChangeTheme ( Theme, Int )
      -- Element States
    | ShowNav Bool
    | ImageOver Bool
    | SelectWork Int
    | NewMousePos ( Float, Float )
      -- Elements
    | GetSectionSize (Result Error Element)
    | GotElementPosition String (Result Error Element)


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

        WheelDelta delta_ ->
            ( { model
                | wheelDelta = delta_
              }
            , Cmd.none
            )

        GoToSection y_ ->
            ( model
            , Task.attempt (\_ -> NoOp) <|
                setViewport 0 y_
            )

        ChangeTheme ( scheme_, hue_ ) ->
            ( { model
                | theme =
                    { scheme = scheme_
                    , hue = hue_
                    }
              }
            , Cmd.none
            )

        ShowNav toggler_ ->
            ( { model | showNav = not toggler_ }, Cmd.none )

        ImageOver isOver_ ->
            ( { model | imageOver = isOver_ }, Cmd.none )

        SelectWork selected_ ->
            ( { model | workSelected = selected_ }
            , getSectionPos listIds
            )

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

                Err _ ->
                    ( model, Cmd.none )

        GotElementPosition id_ result_ ->
            case result_ of
                Ok e_ ->
                    let
                        y_ =
                            e_.element.y
                    in
                    ( { model | elementsPosition = Dict.insert id_ y_ model.elementsPosition }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )


getPosition : (Msg -> msg) -> String -> Cmd msg
getPosition lift id =
    Task.attempt (lift << GotElementPosition id) <|
        getElement id



-- SUBSCRIPTIONS


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ onResize <| \w h -> GetNewViewport ( toFloat w, toFloat h )
        , Sub.map ScrollMsg Scroll.subScroll
        ]



-- VIEW


listIds : List String
listIds =
    [ "about-me"
    , "where-i-have-worked"
    , "some-things-that-i-build"
    , "other-noteworthy-projects"
    ]


onWheel : (Wheel.Event -> msg) -> Attribute msg
onWheel =
    { stopPropagation = True, preventDefault = False }
        |> Wheel.onWithOptions


wheelDelta : Wheel.Event -> Msg
wheelDelta wheelEvent =
    if wheelEvent.deltaY > 0 then
        WheelDelta True

    else
        WheelDelta False


view : Model -> View Msg
view model =
    let
        sy =
            model.scroll.scrollPos.y

        theme =
            case ( model.theme.scheme, model.theme.hue ) of
                ( Dark, x ) ->
                    { scheme = "", hue = String.fromInt x }

                ( Light, x ) ->
                    { scheme = "light", hue = String.fromInt x }
    in
    { title = "Revex - Home"
    , body =
        Layout.viewLayout
            { initLayout
                | route = Route.Home_
                , rootAttrs = [ class theme.scheme, customProp "page-hue" theme.hue, onWheel wheelDelta ]
                , headerAttrs =
                    [ classList
                        [ ( "wheel-hidden", model.wheelDelta && sy >= 100 )
                        , ( "before:content-none", sy <= 100 )
                        , ( "backdrop-blur", not model.showNav || model.viewport.w >= 1024 )
                        ]
                    ]
                , headerContent = viewHeader model
                , mainContent = viewPage model
            }
    }


correctZero : Int -> String
correctZero =
    String.fromInt >> String.padLeft 2 '0'


getIds : Int -> String
getIds =
    Maybe.withDefault ""
        << aplR (Array.fromList listIds)
        << Array.get


viewHeader : Model -> List (Html Msg)
viewHeader model =
    let
        links =
            List.indexedMap
                (\i route ->
                    li []
                        [ a
                            [ href <| "#"
                            , class "list__link"
                            , getIds i
                                |> Dict.get
                                >> aplR model.elementsPosition
                                |> Maybe.withDefault 0
                                |> GoToSection
                                |> onClick
                            , tabindex 1
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
                { className = "check" }

            else
                { className = "uncheck" }
    in
    [ a [ class "h-full", onClick <| GoToSection 0, tabindex 1 ] [ materialIcon "icon" "hive" ]
    , if model.viewport.w <= 1024 then
        button
            [ class <| "nav-toggler " ++ checkNav.className
            , onClick <| ShowNav model.showNav
            ]
            [ materialIcon "nav-toggler__icon segment" "segment"
            , materialIcon "nav-toggler__icon close" "close"
            ]

      else
        text ""
    , nav [ class <| "nav " ++ checkNav.className ]
        [ if model.viewport.w <= 1024 then
            div [ onClick <| ShowNav model.showNav ] []

          else
            text ""
        , links
            ++ [ li [] [ a [ href "#", class "list__resume", tabindex 1 ] [ text "resume" ] ] ]
            |> ul [ class "list" ]
        ]
    ]


viewPage : Model -> List (Html Msg)
viewPage model =
    let
        content =
            [ div [ orientation "left", class "main-orientation left-0" ]
                [ div [ class "grid gap-10 select-none mt-auto" ] <|
                    List.map
                        (\( icon, url ) ->
                            a [ class "up", href <| url, tabindex 2 ]
                                [ materialIcon "text-3xl" icon ]
                        )
                        [ ( "south_america", "#" )
                        , ( "fingerprint", "#" )
                        , ( "all_inclusive", "#" )
                        , ( "blur_on", "#" )
                        ]
                ]
            , viewMainContent model
            , div [ orientation "right", class "main-orientation right-0" ]
                [ a [ class "email up", href "#", tabindex 2 ] [ text "johann.gon.pereira@gmail.com" ]
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
        [ viewIntroduction model
        , viewThemeConfig model
        , viewAboutMe model
        , viewWhereHaveIWorked model
        , viewThingsThatIHaveBuild model
        ]


secOneId : String
secOneId =
    "introduction-id"


viewIntroduction : Model -> Html Msg
viewIntroduction model =
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
            [ Html.i [ class "font-mono text-accent-600 text-sm", tabindex 4 ]
                [ text "hi, my name is" ]
            , h1 [ class "text-7xl font-800", id "title--name", tabindex 4 ]
                [ [ "Johann", textSize 1920 "Gonçalves", "Pereira" ]
                    |> String.join " "
                    |> text
                ]
            , h2 [ class "text-7xl font-800", tabindex 4 ]
                [ [ "I", textSize 1920 "love to", "build", textSize 1440 "things", "for the web." ]
                    |> String.join " "
                    |> text
                ]
            , p [ class "inline-block text-surface-400 sm:w-gold-paragraph", tabindex 4 ]
                [ text """I’m a software developer specializing in
                building (and occasionally designing) exceptional 
                digital experiences. Currently,
                I'm focused on building the plataform for """
                , a
                    [ class "link-underline"
                    , customProp "c-ch" "-13ch"
                    , href "https://app.materialize.pro"
                    , tabindex 4
                    ]
                    [ text "Materialize" ]
                , text "."
                ]
            , a [ class "btm-accent mt-8", href "https://github.com/Johann-Goncalves-Pereira", tabindex 4 ]
                [ text "Check my GitHub" ]
            ]
        ]


viewThemeConfig : Model -> Html Msg
viewThemeConfig model =
    let
        theme =
            model.theme

        themeScheme =
            case theme.scheme of
                Dark ->
                    { to = Light, icon = materialIcon "" "light_mode" }

                Light ->
                    { to = Dark, icon = materialIcon "" "dark_mode" }

        hueCalc i =
            i * 30

        colors =
            List.indexedMap
                (\i t ->
                    li [ class "list__item" ]
                        [ button
                            [ class "list__button"
                            , tabindex 2
                            , onClick <| ChangeTheme ( theme.scheme, hueCalc i )
                            , customProp "hue" <| String.fromInt <| hueCalc i
                            , HA.title <| "Change to: " ++ t
                            ]
                            []
                        ]
                )
                [ "Red"
                , "Orange"
                , "Yellow"
                , "Yellow Green"
                , "Green"
                , "Green Blue"
                , "Blue Green"
                , "Light Blue"
                , "Blue"
                , "Purple"
                , "Pink"
                , "Hot Pink"
                ]
    in
    section [ class "theme" ] <|
        [ button
            [ class "scheme"
            , tabindex 2
            , onClick <|
                ChangeTheme ( themeScheme.to, theme.hue )
            , HA.title "Change Theme"
            ]
            [ themeScheme.icon ]
        , ul [ class "list" ] colors
        ]


headersSection : Int -> String -> Html Msg
headersSection sectNumber title =
    header [ class <| "header-section", tabindex <| sectNumber + 4 ]
        [ Html.i [ class "header-section__number" ] [ text <| correctZero sectNumber ++ "." ]
        , h3
            [ class "header-section__title"
            , id <| "section--title--" ++ String.fromInt sectNumber
            ]
            [ text title ]
        ]


viewAboutMe : Model -> Html Msg
viewAboutMe model =
    sectionBuilder "about-me" "About Me" 1 <|
        [ p [ class "paragraph", tabindex 5 ]
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
                        li [ class "footer__item", tabindex 5 ]
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


viewWhereHaveIWorked : Model -> Html Msg
viewWhereHaveIWorked model =
    let
        listWork =
            List.indexedMap
                (\i name ->
                    li
                        [ classList
                            [ ( "work-list__item", True )
                            , ( "work-list__item--selected"
                              , i == model.workSelected
                              )
                            ]
                        ]
                        [ button
                            [ class "work-list__item__btm"
                            , onClick <| SelectWork i
                            , tabindex 6
                            ]
                            [ text name ]
                        ]
                )
                [ "Materialize", "Portfolio", "Cssnano", "Elm", "Personal" ]

        workContent =
            List.indexedMap
                (\i { title, atSign, date, content } ->
                    let
                        nCh =
                            [ String.fromInt <| (String.length atSign + 1) * -1, "ch" ]
                                |> String.concat
                                |> customProp "n-ch"
                    in
                    if i == model.workSelected then
                        div [ class "work" ]
                            [ strong [ class "work__title", tabindex 6 ]
                                [ text <| title ++ " "
                                , a
                                    [ class "link-underline"
                                    , href "#"
                                    , nCh
                                    , tabindex 6
                                    ]
                                    [ text <| "@" ++ atSign ]
                                ]
                            , p [ class "work__date", tabindex 6 ] [ text date ]
                            , List.map
                                (\desc ->
                                    li [ class "work__paragraph", tabindex 6 ]
                                        [ materialIcon "list-icon" "arrow_right", text desc ]
                                )
                                content
                                |> ul [ class "grid gap-2" ]
                            ]

                    else
                        text ""
                )
                [ { title = "Front-End Developer"
                  , atSign = "materialize"
                  , date = "August 2021 - Present"
                  , content =
                        [ "Materialize is a free and open-source Material Design Framework for web and mobile applications."
                        , "It is a collection of HTML, CSS, and JavaScript components that are used to build websites and web applications."
                        , """It is a collection of HTML,ns are certain characters which resemble, or are variations of 
                            the alphabet and other keyword symbols. For example, if we can take the phrase 
                            "thug life" and convert its characters into the fancy letters "𝖙𝖍𝖚𝖌 𝖑𝖎𝖋𝖊" which 
                            are a set of unicode symbols. These different sets of fancy text letters are 
                            scattered all throughout the unicode specification, and so to create a fancy 
                            text translator, it's just a matter of finding these sets of letters and symbols, 
                            and linking them to their normal alphabetical equivale CSS, and JavaScript components that are used to build websites and web applications."""
                        , """Amongst the hundreds of thousands of symbols which are in the unicode 
                            text specifications are certain characters which resemble, or are variations of 
                            the alphabet and other keyword symbols. For example, if we can take the phrase 
                            "thug life" and convert its characters into the fancy letters "𝖙𝖍𝖚𝖌 𝖑𝖎𝖋𝖊" which 
                            are a set of unicode symbols. These different sets of fancy text letters are 
                            scattered all throughout the unicode specification, and so to create a fancy 
                            text translator, it's just a matter of finding these sets of letters and symbols, 
                            and linking them to their normal alphabetical equivalents.-
                            """
                        ]
                  }
                , { title = "Front-End Developer"
                  , atSign = "materialize"
                  , date = "August 2021 - Present"
                  , content =
                        [ "Materialize is a free and open-source Material Design Framework for web and mobile applications."
                        , "It is a collection of HTML, CSS, and JavaScript components that are used to build websites and web applications."
                        , "It is a collection of HTML, CSS, and JavaScript components that are used to build websites and web applications."
                        , """Amongst the hundreds of thousands of symbols which are in the unicode 
                            text specifications are certain characters which resemble, or are variations of 
                            the alphabet and other keyword symbols. For example, if we can take the phrase 
                            """
                        ]
                  }
                ]
    in
    sectionBuilder "where-have-i-worked" "Where I’ve Worked" 2 <|
        ul
            [ String.concat
                [ "work-list "
                , "work-list--"
                , String.fromInt model.workSelected
                , " scroll-style"
                ]
                |> class
            ]
            listWork
            :: workContent


sectionBuilder : String -> String -> Int -> List (Html Msg) -> Html Msg
sectionBuilder className title count content =
    section
        [ class className
        , id <| getIds (count - 1)
        , "section--title--"
            ++ String.fromInt count
            |> ariaLabelledby
        ]
        (headersSection count title :: content)


isOdd : Int -> Bool
isOdd x =
    if modBy 2 x == 0 then
        False

    else
        True


viewThingsThatIHaveBuild : Model -> Html Msg
viewThingsThatIHaveBuild model =
    let
        viewProjects =
            List.indexedMap
                (\i { imgUrl, italic, title, desc, list, repositoryUrl, projectLink } ->
                    div
                        [ classList
                            [ ( "projects", True )
                            , ( "projects--left", isOdd i )
                            , ( "projects--right", not <| isOdd i )
                            ]
                        ]
                        [ div [ class "img" ] [ img [ src imgUrl ] [] ]
                        , div [ class "projects__info" ]
                            [ Html.i
                                [ class " font-mono font-500 text-accent-600 text-sm z-10 sm:text-accent-500"
                                , tabindex 7
                                ]
                                [ text <| Maybe.withDefault "Featured Project" italic ]
                            , strong [ class " font-800 text-1xl md:text-3xl z-10", tabindex 7 ] [ text title ]
                            , div [ class "paragraph", tabindex 7 ]
                                [ p [ class "paragraph__text", tabindex 7 ] [ text desc ]
                                ]
                            , ul [ class "list" ] <|
                                List.map (\itemText -> li [ tabindex 7 ] [ text itemText ])
                                    list
                            , div
                                [ classList
                                    [ ( "flex  items-center gap-4 mt-1 text-surface-100 ", True )
                                    , ( "md:justify-end", not <| isOdd i )
                                    ]
                                ]
                                [ a [ href repositoryUrl, tabindex 7 ] [ materialIcon "drop-shadow" "blur_on" ]
                                , a [ href projectLink, tabindex 7 ] [ materialIcon "drop-shadow" "open_in_new" ]
                                ]
                            ]
                        ]
                )
                [ { imgUrl = "https://picsum.photos/2000/1000/"
                  , italic = Nothing
                  , title = "Materialize Plataform"
                  , desc = """
                            Materialize is a free and open-source Material Design 
                            Framework for web and mobile applications.
                            And a thing that I Don't understand,
                            Materialize is a free and open-source Material Design 
                            Framework for web and mobile applications."""
                  , list =
                        [ "Elm"
                        , "Json"
                        , "Html"
                        , "Css"
                        ]
                  , repositoryUrl = "#"
                  , projectLink = "#"
                  }
                , { imgUrl = "https://picsum.photos/1800/1200/"
                  , italic = Nothing
                  , title = "Materialize Website"
                  , desc = """
                            Materialize is a free and open-source Material Design 
                            Framework for web and mobile applications.
                            And a thing that I Don't understand,
                            Materialize is a free and open-source Material Design 
                            Framework for web and mobile applications.
                            Materialize is a free and open-source Material Design 
                            Framework for web and mobile applications.
                            And a thing that I Don't understand,
                            Materialize is a free and open-source Material Design 
                            Framework for web and mobile applications."""
                  , list =
                        [ "Wordpress"
                        , "Translate"
                        , "Css"
                        , "SEO"
                        ]
                  , repositoryUrl = "#"
                  , projectLink = "#"
                  }
                ]
                |> List.repeat 2
                |> List.concat
    in
    sectionBuilder "things-that-i-have-build" "Some Things I've Built" 3 <|
        viewProjects
            ++ List.singleton
                (section [ class "other-noteworthy-projects" ]
                    [ header [ class "grid place-items-center gap-5" ]
                        [ h4 [ class "text-4xl font-800", tabindex 7 ] [ text "Other Noteworthy Projects" ]
                        , a [ class "link-underline", href "#", customProp "n-ch" "-13ch", tabindex 7 ] [ text "view the archive" ]
                        ]
                    , div [] <| viewNoteworthyProjects model
                    ]
                )


viewNoteworthyProjects : Model -> List (Html Msg)
viewNoteworthyProjects model =
    let
        modMedia i =
            modBy 3 i
    in
    List.indexedMap
        (\i { gitHubUrl, projectUlr } ->
            let
                head_ =
                    "header--noteworthy--" ++ String.fromInt i

                link_ url_ icon_ =
                    a [ href url_, tabindex 7 ] [ materialIcon "" icon_ ]

                gitHub_ =
                    case gitHubUrl of
                        Nothing ->
                            text ""

                        Just url_ ->
                            link_ url_ "blur_on"
            in
            li []
                [ section [ class "card", ariaLabelledby head_ ]
                    [ div []
                        [ materialIcon "" "folder"
                        , gitHub_
                        , link_ projectUlr "open_in_new"
                        ]
                    , header []
                        [ h6 [ id head_ ] []
                        ]
                    ]
                ]
        )
        [ 1, 1, 1, 1, 1, 1, 1, 11, 1, 1, 1, 1, 1, 1, 1 ]
