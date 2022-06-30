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
        , article
        , br
        , div
        , footer
        , h1
        , h2
        , h3
        , h4
        , h5
        , h6
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
import Html.Attributes.Aria exposing (ariaChecked, ariaControls, ariaLabel, ariaLabelledby, ariaSelected, role)
import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Wheel as Wheel exposing (onWheel)
import Layout exposing (initLayout, rootId)
import Page
import Process
import Request
import Round
import Shared
import String exposing (right)
import Svg exposing (desc)
import Svg.Attributes exposing (display, orientation, type_)
import Task
import Utils.Func exposing (aplR)
import Utils.Models as Models
import Utils.Scroll as Scroll
import Utils.View exposing (button, customProp, customProps, materialIcon)
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
    , wheelDelta : Bool

    -- Element States
    , showNav : Bool
    , imageOver : Bool
    , showMore : Bool
    , workSelected : Int
    , mousePos : { x : Float, y : Float }

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
      , wheelDelta = False

      -- Element States
      , showNav = False
      , imageOver = False
      , showMore = False
      , workSelected = 0
      , mousePos = { x = 0, y = 0 }

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
    | ShowMore Bool
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

        ShowMore toggler_ ->
            ( { model | showMore = not toggler_ }, Cmd.none )

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



-- SUBSCRIPTIONS


subs : Model -> Sub Msg
subs _ =
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
    , "contact-me"
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
                , rootAttrs =
                    [ class theme.scheme
                    , customProp "page-hue" theme.hue
                    , onWheel wheelDelta
                    ]
                , headerAttrs =
                    [ classList
                        [ ( "wheel-hidden", model.wheelDelta && sy >= 100 )
                        , ( "before:content-none", sy <= 100 )
                        , ( "backdrop-blur", not model.showNav || model.viewport.w >= 1024 )
                        ]
                    ]
                , headerContent = viewHeader model
                , mainContent = viewPage model
                , footerAttrs = (viewFooter model).attrs
                , footerContent = (viewFooter model).content
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
                            , tabindex 0
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
                { className = "check", ariaChecked_ = "true" }

            else
                { className = "uncheck", ariaChecked_ = "false" }
    in
    [ a [ class "h-full", href "#", tabindex 0, onClick <| GoToSection 0 ] [ materialIcon "icon" "hive" ]
    , if model.viewport.w <= 1024 then
        button
            [ class <| "nav-toggler " ++ checkNav.className
            , role "switch"
            , ariaChecked checkNav.ariaChecked_
            , onClick <| ShowNav model.showNav
            ]
            [ materialIcon "nav-toggler__icon segment" "segment"
            , materialIcon "nav-toggler__icon close" "close"
            ]

      else
        text ""
    , nav [ class <| "nav " ++ checkNav.className ]
        [ if model.viewport.w <= 1024 then
            button [ role "switch", onClick <| ShowNav model.showNav ] []

          else
            text ""
        , links
            ++ [ li [] [ a [ href "#", class "list__resume", tabindex 0 ] [ text "resume" ] ] ]
            |> ul [ class "list" ]
        ]
    ]


viewPage : Model -> List (Html Msg)
viewPage model =
    let
        content =
            [ Html.address [ orientation "left", class "main-orientation left-0" ]
                [ nav [ class "grid gap-10 select-none mt-auto" ] <|
                    List.map
                        (\( icon, url ) ->
                            a
                                [ class "up"
                                , href <| url
                                , tabindex 0
                                , target "_blank"
                                ]
                                [ materialIcon "text-3xl" icon ]
                        )
                        [ ( "south_america", "#" )
                        , ( "fingerprint", "#" )
                        , ( "all_inclusive", "#" )
                        , ( "blur_on", "#" )
                        ]
                ]
            , Html.address [ orientation "right", class "main-orientation right-0" ]
                [ a
                    [ class "email up"
                    , href "#"
                    , tabindex 0
                    , target "_blank"
                    ]
                    [ text "johann.gon.pereira@gmail.com" ]
                ]
            , viewMainContent model
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
    article [ class "main grid gap-10 w-[min(100vw_-_2rem,var(--size-xxl))] lg:w-full mx-auto z-10" ]
        [ viewIntroduction model
        , viewThemeConfig model
        , viewAboutMe model
        , viewWhereHaveIWorked model
        , viewThingsThatIHaveBuild model
        , viewWhatsNext model
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
        [ div [ class "secOne grid place-content-center justify-items-start gap-5", id secOneId ]
            [ Html.i [ class "font-mono text-accent-600 text-sm", tabindex 0 ]
                [ text "hi, my name is" ]
            , h1 [ class "text-7xl font-800", id "title--name", tabindex 0 ]
                [ [ "Johann", textSize 1920 "Gonçalves", "Pereira" ]
                    |> String.join " "
                    |> text
                ]
            , h2 [ class "text-7xl font-800", tabindex 0 ]
                [ [ "I", textSize 1920 "love to", "build", textSize 1440 "things", "for the web." ]
                    |> String.join " "
                    |> text
                ]
            , p [ class "inline-block text-surface-400 sm:w-gp", tabindex 0 ]
                [ text """I’m a software developer specializing in
                building (and occasionally designing) exceptional 
                digital experiences. Currently,
                I'm focused on building the plataform for """
                , a
                    [ class "link-underline"
                    , customProp "c-ch" "-13ch"
                    , href "https://app.materialize.pro"
                    , tabindex 0
                    , target "_blank"
                    ]
                    [ text "Materialize" ]
                , text "."
                ]
            , a [ class "btm-accent mt-8", href "https://github.com/Johann-Goncalves-Pereira", tabindex 0 ]
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
                            , tabindex 0
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
            , tabindex 0
            , onClick <|
                ChangeTheme ( themeScheme.to, theme.hue )
            , HA.title "Change Theme"
            ]
            [ themeScheme.icon ]
        , ul [ class "list" ] colors
        ]


headersSection : Int -> String -> Html Msg
headersSection sectNumber title =
    header [ class <| "header-section", tabindex 0 ]
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
        [ p [ class "paragraph", tabindex 0 ]
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
            [ ul [ class "footer__list", tabindex 0 ] <|
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
            [ classList [ ( "img", True ), ( "hover", model.imageOver ) ]
            , tabindex 0
            , ariaLabel "Profile Photo"
            ]
            [ img [ src "https://picsum.photos/1200", alt "Profile Photo" ] [] ]
        ]


viewWhereHaveIWorked : Model -> Html Msg
viewWhereHaveIWorked model =
    let
        tabControls index_ =
            "header--where--" ++ String.fromInt index_ ++ "--tp"

        listWork =
            List.indexedMap
                (\i name ->
                    let
                        selected_ =
                            if i == model.workSelected then
                                { selectedB = True, selectedS = "true" }

                            else
                                { selectedB = False, selectedS = "false" }
                    in
                    button
                        [ classList
                            [ ( "work-list__btn", True )
                            , ( "work-list__btn--selected", selected_.selectedB )
                            ]
                        , role "tab"
                        , ariaSelected selected_.selectedS
                        , ariaControls <| tabControls i
                        , tabindex 0
                        , onClick <| SelectWork i
                        ]
                        [ text name ]
                )
                [ "Materialize", "Portfolio", "Cssnano", "Elm", "Personal" ]

        workContent =
            List.indexedMap
                (\i { title, atSign, date, content } ->
                    let
                        head_ =
                            "header--where--" ++ String.fromInt i

                        nCh =
                            [ String.fromInt <| (String.length atSign + 1) * -1, "ch" ]
                                |> String.concat
                                |> customProp "n-ch"
                    in
                    if i == model.workSelected then
                        section
                            [ class "work"
                            , role "tabpanel"
                            , ariaLabelledby head_
                            , id <| tabControls i
                            ]
                            [ header [ class "work__header" ]
                                [ h5
                                    [ class ""
                                    , id head_
                                    , tabindex 0
                                    ]
                                    [ text <| title ++ " "
                                    ]
                                , a
                                    [ class "link-underline"
                                    , href "#"
                                    , nCh
                                    , tabindex 0
                                    , target "_blank"
                                    ]
                                    [ text <| "@" ++ atSign ]
                                ]
                            , p [ class "work__date", tabindex 0 ] [ text date ]
                            , List.map
                                (\desc ->
                                    li [ class "work__paragraph" ]
                                        [ materialIcon "list-icon" "arrow_right", text desc ]
                                )
                                content
                                |> ul [ class "grid gap-2", tabindex 0 ]
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
        div
            [ String.concat
                [ "work-list "
                , "work-list--"
                , String.fromInt model.workSelected
                , " scroll-style"
                ]
                |> class
            , role "tablist"
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



-- tabPainel -> tabList -> tab


viewThingsThatIHaveBuild : Model -> Html Msg
viewThingsThatIHaveBuild model =
    let
        viewProjects =
            List.indexedMap
                (\i { imgUrl, altImg, italic, title, desc, list, repositoryUrl, projectLink } ->
                    div
                        [ classList
                            [ ( "projects", True )
                            , ( "projects--left", isOdd i )
                            , ( "projects--right", not <| isOdd i )
                            ]
                        ]
                        [ a
                            [ class "img"
                            , href projectLink
                            , ariaLabel altImg
                            , tabindex 0
                            , target "_blank"
                            ]
                            [ img [ src imgUrl, alt altImg ] [] ]
                        , div [ class "projects__info" ]
                            [ Html.i
                                [ class "font-mono font-500 text-accent-600 text-sm z-10 sm:text-accent-600"
                                , tabindex 0
                                ]
                                [ text <| Maybe.withDefault "Featured Project" italic ]
                            , h5 [ class " font-800 text-1xl md:text-3xl z-10", tabindex 0 ]
                                [ text title ]
                            , div [ class "paragraph" ]
                                [ p [ class "paragraph__text", tabindex 0 ] [ text desc ]
                                ]
                            , ul [ class "list", tabindex 0 ] <|
                                List.map (\itemText -> li [] [ text itemText ])
                                    list
                            , div
                                [ classList
                                    [ ( "flex  items-center gap-4 mt-1 text-surface-100 ", True )
                                    , ( "md:justify-end", not <| isOdd i )
                                    ]
                                ]
                                [ a
                                    [ class "inline-grid place-content-center"
                                    , href repositoryUrl
                                    , tabindex 0
                                    , target "_blank"
                                    ]
                                    [ materialIcon "drop-shadow" "blur_on" ]
                                , a
                                    [ class "inline-grid place-content-center"
                                    , href projectLink
                                    , tabindex 0
                                    , target "_blank"
                                    ]
                                    [ materialIcon "drop-shadow" "open_in_new" ]
                                ]
                            ]
                        ]
                )
                thingsThatIHaveBuild

        showMore =
            if model.showMore then
                "Less"

            else
                "More"
    in
    sectionBuilder "things-that-i-have-build" "Some Things I've Built" 3 <|
        viewProjects
            ++ List.singleton
                (section [ class "other-noteworthy-projects", ariaLabelledby "header-noteworthy" ]
                    [ header [ class "grid place-items-center gap-5" ]
                        [ h4 [ class "text-4xl text-center font-800", tabindex 0, id "header-noteworthy" ]
                            [ text "Other Noteworthy Projects" ]
                        , a [ class "link-underline", href "#", customProp "n-ch" "-13ch", tabindex 0, target "_blank" ]
                            [ text "view the archive" ]
                        ]
                    , ul [ class "grid grid-cols-fit-20 auto-rows-fr gap-6" ] <| viewNoteworthyProjects model
                    , button
                        [ class "btm-accent mx-auto"
                        , tabindex 0
                        , onClick <| ShowMore model.showMore
                        ]
                        [ text <| String.join " " [ "View", showMore ] ]
                    ]
                )


thingsThatIHaveBuild :
    List
        { imgUrl : String
        , altImg : String
        , italic : Maybe String
        , title : String
        , desc : String
        , list : List String
        , repositoryUrl : String
        , projectLink : String
        }
thingsThatIHaveBuild =
    [ { imgUrl = "https://picsum.photos/2000/1000/"
      , altImg = "Materialize Plataform"
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
      , altImg = "Materialize Website"
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


viewNoteworthyProjects : Model -> List (Html Msg)
viewNoteworthyProjects model =
    let
        v_ =
            if model.viewport.w <= 1024 then
                1

            else if model.viewport.w <= 1440 then
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
                        [ class "link"
                        , href url_
                        , tabindex 0
                        ]
                        [ materialIcon "" icon_ ]

                gitHub_ =
                    case gitHubUrl of
                        Nothing ->
                            text ""

                        Just url_ ->
                            link_ url_ "blur_on"
            in
            li [ class "card-item", tabindex 0 ]
                [ a
                    [ class "card"
                    , href projectUlr
                    , ariaLabelledby head_
                    , target "_blank"
                    ]
                    [ div [ class "card__wrapper " ]
                        [ materialIcon "folder" "folder"
                        , gitHub_
                        , link_ projectUlr "open_in_new"
                        ]
                    , h6 [ class "card__title", id head_ ] [ text title ]
                    , p [] [ text desc ]
                    , ul [ class "card__list" ] <|
                        List.map (\itemText -> li [] [ text itemText ])
                            tags
                    ]
                ]
        )
        (if model.showMore then
            noteworthyProjectsData

         else
            List.take (max 4 (v_ * 2)) noteworthyProjectsData
        )



-- noteworthyProjectsData :


noteworthyProjectsData :
    List
        { gitHubUrl : Maybe String
        , projectUlr : String
        , title : String
        , desc : String
        , tags : List String
        }
noteworthyProjectsData =
    [ { gitHubUrl = Just "#"
      , projectUlr = "#"
      , title = "Out Doors website"
      , desc = """A simple website for a company that sells outdoor gear.
          It's just the home page bug is responsive and super beautiful"""
      , tags = [ "elm", "sass", "html" ]
      }
    , { gitHubUrl = Nothing
      , projectUlr = "#"
      , title = "Out Doors website"
      , desc = """A simple website for a company that sells outdoor gear.
          It's just the home page bug is responsive and super beautiful
          A simple website for a company that sells outdoor gear.
          It's just the home page bug is responsive and super beautiful"""
      , tags = [ "elm", "sass", "html" ]
      }
    , { gitHubUrl = Just "#"
      , projectUlr = "#"
      , title = "Out Doors website Out Doors website Out Doors websiteOut Doors website"
      , desc = """A simple website for a company that sells outdoor gear.
          It's just the home page bug is responsive and super beautiful"""
      , tags = [ "elm", "sass", "html" ]
      }
    ]
        |> List.repeat 5
        |> List.concat


viewWhatsNext : Model -> Html Msg
viewWhatsNext model =
    let
        al_ =
            "section--title--4"
    in
    section [ class "whats-now", ariaLabelledby al_, id <| getIds 3 ]
        [ header [ class "flex gap-2 text-accent-600 font-mono" ]
            [ Html.i [] [ text "04. " ]
            , h3 [ id al_ ] [ text "What’s Next?" ]
            ]
        , h6 [ class "font-900 text-5xl" ] [ text "Get In Touch" ]
        , p [ class "sm:w-gp text-center" ] [ text """
            Although I’m not currently looking for any new opportunities, 
            my inbox is always open. Whether you have a question or just 
            want to say hi, I’ll try my best to get back to you!""" ]
        , button
            [ class "btm-accent mt-6 mx-auto"
            , tabindex 0
            , onClick <| ShowMore model.showMore
            ]
            [ text "Say Hello" ]
        ]


viewFooter : Model -> { attrs : List (Attribute msg), content : List (Html msg) }
viewFooter model =
    { attrs = [ class "grid place-items-center pb-3 mt-40" ]
    , content =
        [ a
            [ class "grid gap-3 whitespace-pre-wrap text-center text-xs font-mono"
            , href "#"
            , tabindex 0
            , target "_blank"
            ]
            [ text "Design by Brittany Chiang && Johann Pereira\n"
            , text "Build by Johann Pereira"
            , p [ class "flex items-center justify-center gap-2 font-600" ]
                [ materialIcon "" "auto_awesome"
                , text "23.40"
                , materialIcon "" "fork_right"
                , text "202.3"
                ]
            ]
        ]
    }
