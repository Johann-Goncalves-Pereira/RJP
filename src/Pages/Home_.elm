module Pages.Home_ exposing (Model, Msg, page)

import Array
import Browser.Dom as BrowserDom exposing (Element, Error, setViewport)
import Browser.Events exposing (onResize)
import Components.Dialog as Dialog
import Components.NoteworthyProjects as NoteworthyProjects
import Components.Svg as ESvg
import Components.ThingsThatIHaveBuild as TTIHB
import Debouncer.Basic as Debouncer exposing (Debouncer, fromSeconds, settleWhenQuietFor, toDebouncer)
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
        , h5
        , h6
        , header
        , img
        , li
        , nav
        , p
        , section
        , span
        , text
        , ul
        )
import Html.Attributes as Attr
    exposing
        ( alt
        , class
        , classList
        , href
        , id
        , src
        , tabindex
        , target
        )
import Html.Attributes.Aria exposing (ariaChecked, ariaControls, ariaLabel, ariaLabelledby, ariaSelected, role)
import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Wheel as Wheel exposing (onWheel)
import InView
import Layout exposing (initLayout)
import Page
import Request
import Round
import Shared
import Storage exposing (Storage)
import Svg exposing (desc)
import Svg.Attributes exposing (offset, orientation)
import Task
import Utils.Func exposing (aplR)
import Utils.Models as Models
import Utils.View exposing (button, customProp, customProps, materialIcon, picture)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared _ =
    Page.element
        { init = init
        , update = update shared
        , view = view shared
        , subscriptions = subs
        }



-- INIT


type alias Model =
    { -- Page Events
      quietForSomeTime : Debouncer Msg Msg
    , wheelDelta : Bool

    -- Element States
    , showNav : Bool
    , imageOver : Bool
    , showMore : Bool
    , workSelected : Int
    , mousePos : { x : Float, y : Float }
    , sectionOne : { w : Float, h : Float }
    , elementsPosition : Dict String Float

    -- Models
    , thingsThatIBuild : Models.ThingsThatIBuild
    , noteworthyProjects : NoteworthyProjects.Model

    -- Inputs
    , dialog : Dialog.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( noteworthy_, noteworthyCmd_ ) =
            NoteworthyProjects.init
    in
    ( { -- Page Events
        quietForSomeTime = manualDebouncer 0.25
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
      , noteworthyProjects = noteworthy_

      -- Inputs
      , dialog = Dialog.init
      }
    , Cmd.batch
        [ --
          BrowserDom.getElement secOneId
            |> Task.attempt GetSectionSize
        , getSectionPos listIds
        , Cmd.map NoteworthyProjectsMsg noteworthyCmd_
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


manualDebouncer : Float -> Debouncer Msg Msg
manualDebouncer time_ =
    Debouncer.manual
        |> settleWhenQuietFor (Just <| fromSeconds time_)
        |> toDebouncer



-- UPDATE


type Msg
    = -- Page Events
      NoOp
    | MsgQuietForSomeTime (Debouncer.Msg Msg)
    | ScrollTo (Maybe Float)
    | WheelDelta Bool
    | ChangeTheme ( Storage.Scheme, Int )
      -- Element States
    | ShowNav Bool
    | ImageOver Bool
    | ShowMore Bool
    | SelectWork Int
    | NewMousePos ( Float, Float )
    | DialogMsg Dialog.Msg
    | NoteworthyProjectsMsg NoteworthyProjects.Msg
      -- Elements
    | GetSectionSize (Result Error Element)
    | GotElementPosition String (Result Error Element)
    | GotElementPositionAgain


update : Shared.Model -> Msg -> Model -> ( Model, Cmd Msg )
update shared msg model =
    let
        storage =
            shared.storage

        offset =
            shared.inView.offset
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        MsgQuietForSomeTime subMsg ->
            let
                ( model_, cmd_, msg_ ) =
                    Debouncer.update subMsg model.quietForSomeTime

                mappedCmd =
                    Cmd.map MsgQuietForSomeTime cmd_

                updatedModel =
                    { model | quietForSomeTime = model_ }
            in
            case msg_ of
                Just emitted ->
                    update shared emitted updatedModel
                        |> Tuple.mapSecond
                            (\cmd -> Cmd.batch [ cmd, mappedCmd ])

                Nothing ->
                    ( updatedModel, mappedCmd )

        ScrollTo y_ ->
            ( model
            , Task.attempt (\_ -> NoOp) <|
                setViewport 0 <|
                    case y_ of
                        Nothing ->
                            offset.y

                        Just vpt_ ->
                            vpt_
            )

        WheelDelta delta_ ->
            ( { model
                | wheelDelta = delta_
              }
            , Cmd.none
            )

        ChangeTheme ( scheme_, hue_ ) ->
            ( model
            , Storage.changeHue storage scheme_ hue_
            )

        ShowNav toggler_ ->
            ( { model | showNav = not toggler_ }, Cmd.none )

        DialogMsg msg_ ->
            let
                ( model_, cmd_ ) =
                    Dialog.update msg_ model.dialog
            in
            ( { model | dialog = model_ }
            , Cmd.map DialogMsg cmd_
            )

        NoteworthyProjectsMsg msg_ ->
            let
                ( model_, cmd_ ) =
                    NoteworthyProjects.update msg_ model.noteworthyProjects
            in
            ( { model | noteworthyProjects = model_ }
            , Cmd.map NoteworthyProjectsMsg cmd_
            )

        ShowMore toggler_ ->
            ( { model | showMore = not toggler_ }, getSectionPos listIds )

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
                    ( { model
                        | elementsPosition =
                            Dict.insert id_ y_ model.elementsPosition
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        GotElementPositionAgain ->
            ( model, getSectionPos listIds )



-- SUBSCRIPTIONS


subs : Model -> Sub Msg
subs _ =
    onResize <| \_ _ -> GotElementPositionAgain



-- VIEW


listIds : List String
listIds =
    [ "about"
    , "experience"
    , "work"
    , "contact"
    ]


dialogId : String
dialogId =
    "email-form"


secOneId : String
secOneId =
    "introduction-id"


getIds : Int -> String
getIds =
    Maybe.withDefault ""
        << aplR (Array.fromList listIds)
        << Array.get


onWheel : (Wheel.Event -> msg) -> Attribute msg
onWheel =
    { stopPropagation = True, preventDefault = False }
        |> Wheel.onWithOptions


scrollDeltaTrigger : Wheel.Event -> Msg
scrollDeltaTrigger wheelEvent =
    if wheelEvent.deltaY > 0 then
        WheelDelta True

    else
        WheelDelta False


view : Shared.Model -> Model -> View Msg
view shared model =
    let
        storage =
            shared.storage

        offsetY =
            shared.inView.offset.y

        theme =
            case ( storage.theme.scheme, storage.theme.hue ) of
                ( Storage.Dark, x ) ->
                    { scheme = "", hue = String.fromInt x }

                ( Storage.Light, x ) ->
                    { scheme = "light", hue = String.fromInt x }
    in
    { title = "Johann - Home"
    , body =
        Layout.viewLayout
            { initLayout
                | route = Route.Home_
                , rootAttrs =
                    [ class theme.scheme
                    , customProp "page-hue" theme.hue
                    , scrollDeltaTrigger
                        >> Debouncer.provideInput
                        >> MsgQuietForSomeTime
                        |> onWheel
                    ]
                , headerAttrs =
                    [ classList
                        [ ( "wheel-hidden", model.wheelDelta && offsetY >= 100 )
                        , ( "before:content-none", offsetY <= 100 )
                        , ( "backdrop-blur", not model.showNav || shared.viewport.width >= 1024 )
                        ]
                    ]
                , headerContent = viewHeader shared model
                , mainContent = viewPage shared model
                , footerAttrs = (viewFooter shared).attrs
                , footerContent = (viewFooter shared).content
            }
    }


viewHeader : Shared.Model -> Model -> List (Html Msg)
viewHeader { viewport } model =
    let
        links =
            List.indexedMap
                (\i route ->
                    li []
                        [ a
                            [ href <| "#" ++ getIds i
                            , class "list__link"
                            , getIds i
                                |> Dict.get
                                >> aplR model.elementsPosition
                                -- |> (Just <| Maybe.withDefault 0)
                                |> ScrollTo
                                |> onClick
                            , tabindex 0
                            ]
                            [ span [ class "text-accent-600" ]
                                [ text <| Utils.Func.correctZero i ++ ". " ]
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
    [ a [ class "icon h-full", href "#", tabindex 0, onClick <| ScrollTo <| Just 0 ]
        [ ESvg.myIcon "" ]
    , if viewport.width <= 1024 then
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
        [ if viewport.width <= 1024 then
            button [ role "switch", onClick <| ShowNav model.showNav ] []

          else
            text ""
        , links
            ++ [ li []
                    [ a
                        [ class "list__resume"
                        , href "https://www.upwork.com/freelancers/~011d31917fa3d87f28"
                        , Attr.target "_blank"
                        , tabindex 0
                        ]
                        [ text "resume" ]
                    ]
               ]
            |> ul [ class "list" ]
        ]
    ]


icons : List (Html msg)
icons =
    List.map
        (\( icon, url ) ->
            a
                [ class "up grid place-content-center"
                , href <| url
                , tabindex 0
                , target "_blank"
                ]
                [ icon ]
        )
        [ ( ESvg.linkedin "text-3xl", "https://www.linkedin.com/in/johann-pereira-a798961b3/" )
        , ( ESvg.instagram "text-3xl", "https://www.instagram.com/johanngon_" )
        , ( ESvg.medium "text-3xl", "https://medium.com/@johann.gon.pereira" )
        , ( ESvg.github "text-3xl ", "https://github.com/Johann-Goncalves-Pereira" )
        ]


viewPage : Shared.Model -> Model -> List (Html Msg)
viewPage shared model =
    let
        content =
            [ Html.address [ orientation "left", class "main-orientation left-0" ]
                [ nav [ class "grid gap-10 select-none mt-auto" ] icons
                ]
            , Html.address [ orientation "right", class "main-orientation right-0" ]
                [ a
                    [ class "email up"
                    , href <| "#" ++ dialogId
                    , tabindex 0
                    , Dialog.ToggleDialog dialogId
                        |> onClick
                        |> Attr.map DialogMsg
                    ]
                    [ text "johann.gon.pereira@protonmail.com" ]
                ]
            , viewMainContent shared model
            ]

        media =
            if shared.viewport.width <= 1024 || shared.viewport.width <= 480 then
                viewMainContent shared model
                    |> List.singleton

            else
                content
    in
    media


viewMainContent : Shared.Model -> Model -> Html Msg
viewMainContent shared model =
    article [ class "main grid gap-10 w-[min(100vw_-_2rem,var(--size-xxl))] lg:w-full mx-auto z-10" ] <|
        [ Dialog.dialogForm model.dialog
            |> Html.map DialogMsg
        , viewIntroduction shared model
        , viewThemeConfig shared.storage
        , viewAboutMe shared model
        , viewWhereHaveIWorked shared model
        , TTIHB.view shared.inView.inView
        , loadNoteworthy shared.inView.inView
            NoteworthyProjects.noteworthyProjectsDataIds
            |> NoteworthyProjects.viewNoteworthy
                model.noteworthyProjects
            |> Html.map NoteworthyProjectsMsg
        , viewWhatsNext shared model
        ]


loadElement : InView.State -> String -> Int -> String
loadElement state_ class_ id_ =
    let
        load_ s_ i_ =
            case InView.isInViewWithMargin i_ (InView.Margin 100 0 300 0) s_ of
                Just True ->
                    "view view--in"

                _ ->
                    "view view--out"
    in
    String.join " "
        [ class_
        , (id_ - 1)
            |> getIds
            >> load_ state_
        ]


loadNoteworthy : InView.State -> List String -> List String
loadNoteworthy state_ ids_ =
    List.map
        (\loadedIds_ ->
            case InView.isInViewWithMargin loadedIds_ (InView.Margin 100 0 300 0) state_ of
                Just True ->
                    loadedIds_

                _ ->
                    ""
        )
        ids_


viewIntroduction : Shared.Model -> Model -> Html Msg
viewIntroduction { viewport } model =
    let
        textSize width str =
            if viewport.width >= width then
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
                I'm focused on building the platform for """
                , a
                    [ class "link-underline"
                    , customProp "c-ch" "-13ch"
                    , href "https://app.materialize.pro"
                    , tabindex 0
                    , target "_blank"
                    , Attr.title "Link to Materialize Plataform"
                    ]
                    [ text "Materialize" ]
                , text "."
                ]
            , a
                [ class "btm-accent mt-8"
                , href "https://github.com/Johann-Goncalves-Pereira"
                , tabindex 0
                , Attr.title "Link to GitHub"
                ]
                [ text "Check my GitHub" ]
            ]
        ]


viewThemeConfig : Storage -> Html Msg
viewThemeConfig storage =
    let
        theme =
            storage.theme

        themeScheme =
            case theme.scheme of
                Storage.Dark ->
                    { to = Storage.Light, icon = materialIcon "" "light_mode" }

                Storage.Light ->
                    { to = Storage.Dark, icon = materialIcon "" "dark_mode" }

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
                            , Attr.title <| "Change color to: " ++ t
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
            , Attr.title "Change Theme"
            ]
            [ themeScheme.icon ]
        , ul [ class "list" ] colors
        ]


headersSection : Int -> String -> Html Msg
headersSection sectNumber title =
    header [ class <| "header-section", tabindex 0 ]
        [ Html.i [ class "header-section__number" ] [ text <| Utils.Func.correctZero sectNumber ++ "." ]
        , h3
            [ class "header-section__title"
            , id <| "section--title--" ++ String.fromInt sectNumber
            ]
            [ text title ]
        ]


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


viewAboutMe : Shared.Model -> Model -> Html Msg
viewAboutMe { inView } model =
    let
        externalLink_ url_ name_ =
            a
                [ class "link-underline"
                , customProp "n-ch" <| "-" ++ String.fromInt (String.length name_) ++ "ch"
                , Attr.title "External link"
                , tabindex 0
                , href url_
                ]
                [ text name_ ]

        elementId =
            1

        inView_ =
            inView.inView

        class_ =
            loadElement inView_ "about-me" elementId
    in
    sectionBuilder class_ "About Me" elementId <|
        [ p [ class "paragraph", tabindex 0 ]
            [ text """Hi! I'm Johann a front-end developer from Brazil.
             I love to create for the web, beautiful and functional interfaces.
             My interest for development started back in 2017, when I made a project with the 
            """
            , externalLink_ "https://www.rocketseat.com.br" "RocketSeat"
            , text """ it was my first website, and I fall in love with Html/Css and design, 
            from then I discovered that I wasn't so difficult to make cool websites."""
            , br [] []
            , br [] []
            , text "Jumping to the present day, I'm working at a "
            , externalLink_ "https://www.materialize.pro/for-companies"
                "start-up for instant hiring solution"
            , text """, as a front-end developer, I made the visual of the platform,
             and worked on the website. I'm also working as a freelancer, 
             on my leisure time, and I'm constantly looking for new challenges. 
             The thing that I've the most joy making it, It is accessible and 
             well-designed websites."""
            , br [] []
            , br [] []
            , text "I had work in some open-source projects, like "
            , externalLink_ "https://cssnano.co" "CssNano"
            , text ", "
            , externalLink_ "https://elm-lang.org" "Elm"
            , text " and "
            , externalLink_ "https://open-props.style" "OpenProps"
            , text ". The last things are my personal projects, like "
            , externalLink_ "https://github.com/Johann-Goncalves-Pereira/Revex/" "Revex"
            , text "."
            ]
        , footer [ class "footer" ]
            [ ul [ class "footer__list", tabindex 0 ] <|
                List.map
                    (\language ->
                        li [ class "footer__item" ]
                            [ materialIcon "footer__icon" "arrow_right"
                            , text language
                            ]
                    )
                    [ "elm"
                    , "html - css/sass"
                    , "design - figma"
                    , "Js/Ts - React"
                    , "shell - bash/zsh"
                    , "go(golang)"
                    ]
            ]
        , div
            [ classList [ ( "img", True ), ( "hover", model.imageOver ) ]
            , tabindex 0
            , ariaLabel "Profile Photo"
            ]
            [ picture "/assets/profile-photo" "Profile Photo" ]
        ]


viewWhereHaveIWorked : Shared.Model -> Model -> Html Msg
viewWhereHaveIWorked { inView } model =
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
                [ "Materialize" ]

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
                                        [ materialIcon "list-icon" "arrow_right"
                                        , p [] desc
                                        ]
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
                        [ [ text """An start-up for instant hiring solution,
                         that connects specialists and clients around the word, to work together."""
                          ]
                        , [ text "They have a plataform to management the interaction between the users."
                          , text "I build the plataform from the start, with a variety of different languages, and frameworks. "
                          , text "Such as Elm, Css/Sass, Javascript/Typescript, html, docker, and more."
                          ]
                        , [ text """I make the visual of the platform on the Front-End, 
                        and I work on the website as well. Using WordPress, Html and Css."""
                          ]
                        ]
                  }
                , { title = "Front-End Developer"
                  , atSign = "materialize"
                  , date = "August 2021 - Present"
                  , content =
                        []
                  }
                ]

        elementId =
            2

        inView_ =
            inView.inView

        class_ =
            loadElement inView_ "where-have-i-worked" elementId
    in
    sectionBuilder class_ "Where I've Worked" elementId <|
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


viewWhatsNext : Shared.Model -> Model -> Html Msg
viewWhatsNext { inView } _ =
    let
        al_ =
            "section--title--4"

        elementId =
            4

        inView_ =
            inView.inView

        class_ =
            loadElement inView_ "whats-now" elementId
    in
    section [ class class_, ariaLabelledby al_, id <| getIds (elementId - 1) ]
        [ header [ class "flex gap-2 text-accent-600 font-mono" ]
            [ Html.i [] [ text "04. " ]
            , h3 [ id al_ ] [ text "What’s Next?" ]
            ]
        , h6 [ class "font-900 text-5xl" ] [ text "Get In Touch" ]
        , p [ class "sm:w-gp text-center" ] [ text """
            Although I’m not currently looking for any new opportunities, 
            my inbox is always open. Whether you have a question or just 
            want to say hi, I’ll try my best to get back to you!""" ]
        , a
            [ class "btm-accent mt-6 mx-auto"
            , href <| "#" ++ dialogId
            , tabindex 0
            , Dialog.ToggleDialog dialogId
                |> onClick
                |> Attr.map DialogMsg
            , Dialog.ToggleDialog dialogId
                |> onClick
                |> Attr.map DialogMsg
            ]
            [ text "Say Hello" ]
        ]


viewFooter : Shared.Model -> { attrs : List (Attribute msg), content : List (Html msg) }
viewFooter { viewport } =
    { attrs = [ class "grid gap-5 place-items-center pb-3 mt-12 w-min-base" ]
    , content =
        [ if viewport.width <= 1024 || viewport.height <= 480 then
            div [ class "flex gap-5" ]
                icons

          else
            text ""
        , a
            [ class "grid gap-3 whitespace-pre-wrap text-center text-xs font-mono"
            , href "https://github.com/Johann-Goncalves-Pereira/RJP"
            , tabindex 0
            , target "_blank"
            ]
            [ text "Design by Brittany Chiang && Johann Pereira\n"
            , text "Build by Johann Pereira"

            -- , p [ class "flex items-center justify-center gap-2 font-600" ]
            --     [ materialIcon "" "auto_awesome"
            --     , text "23.40"
            --     , materialIcon "" "fork_right"
            --     , text "202.3"
            --     ]
            ]
        ]
    }
