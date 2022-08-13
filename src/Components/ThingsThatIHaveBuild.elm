module Components.ThingsThatIHaveBuild exposing (..)

import Components.Svg as ESvg
import Html exposing (..)
import Html.Attributes as Attr exposing (class, classList)
import Html.Attributes.Aria exposing (ariaLabel, ariaLabelledby)
import InView
import Utils.Func exposing (correctZero)
import Utils.View exposing (materialIcon, picture)


loadElement : InView.State -> String -> String
loadElement state_ ids_ =
    let
        load_ s_ i_ =
            case InView.isInViewWithMargin i_ (InView.Margin 100 0 300 0) s_ of
                Just True ->
                    "view view--in"

                _ ->
                    "view view--out"
    in
    String.join " "
        [ load_ state_ ids_ ]


ids : List String
ids =
    List.indexedMap (\i_ _ -> buildId i_)
        thingsThatIHaveBuild


isOdd : Int -> Bool
isOdd x =
    if modBy 2 x == 0 then
        False

    else
        True


view : InView.State -> Html msg
view inView =
    let
        inView_ =
            inView

        isInView idx_ =
            buildId idx_
                |> loadElement inView_

        viewProjects =
            List.indexedMap
                (\i { imgUrl, altImg, italic, title, desc, list, repositoryUrl, projectLink } ->
                    let
                        classLink_ =
                            class """inline-grid place-content-center 
                            focus-visible:text-accent-600 hover:text-accent-600 
                            transition-colors"""

                        ariaLabelId =
                            "project-title" ++ String.fromInt i
                    in
                    section
                        [ classList
                            [ ( "projects", True )
                            , ( "projects--left", isOdd i )
                            , ( "projects--right", not <| isOdd i )
                            , ( isInView <| i, True )
                            ]
                        , ariaLabelledby <| ariaLabelId
                        , Attr.id <| buildId i
                        ]
                        [ a
                            [ class "img"
                            , Attr.href projectLink
                            , ariaLabel altImg
                            , Attr.tabindex 0
                            , Attr.target "_blank"
                            ]
                            [ picture imgUrl altImg ]
                        , div [ class "projects__info" ]
                            [ Html.i
                                [ class "font-mono font-500 text-accent-600 text-sm z-10 sm:text-accent-600"
                                , Attr.tabindex 0
                                ]
                                [ text <| Maybe.withDefault "Featured Project" italic ]
                            , h5
                                [ class " font-800 text-1xl md:text-3xl z-10"
                                , Attr.tabindex 0
                                , Attr.id ariaLabelId
                                ]
                                [ text title ]
                            , div [ class "paragraph" ]
                                [ p [ class "paragraph__text", Attr.tabindex 0 ] desc
                                ]
                            , ul [ class "list", Attr.tabindex 0 ] <|
                                List.map (\itemText -> li [] [ text itemText ])
                                    list
                            , div
                                [ classList
                                    [ ( "flex items-center gap-4 mt-1 text-surface-100 text-2xl", True )
                                    , ( "md:justify-end", not <| isOdd i )
                                    ]
                                ]
                                [ case repositoryUrl of
                                    Nothing ->
                                        text ""

                                    Just url_ ->
                                        a
                                            [ classLink_
                                            , Attr.href url_
                                            , Attr.tabindex 0
                                            , Attr.target "_blank"
                                            ]
                                            [ ESvg.github "drop-shadow" ]
                                , a
                                    [ classLink_
                                    , Attr.href projectLink
                                    , Attr.tabindex 0
                                    , Attr.target "_blank"
                                    ]
                                    [ materialIcon "drop-shadow" "open_in_new" ]
                                ]
                            ]
                        ]
                )
                thingsThatIHaveBuild
    in
    section
        [ class "things-that-i-have-build"
        , ariaLabelledby "section--title--3"
        ]
        viewProjects


buildId : Int -> String
buildId idx_ =
    "have-build--" ++ String.fromInt idx_


thingsThatIHaveBuild :
    List
        { imgUrl : String
        , altImg : String
        , italic : Maybe String
        , title : String
        , desc : List (Html msg)
        , list : List String
        , repositoryUrl : Maybe String
        , projectLink : String
        }
thingsThatIHaveBuild =
    [ { imgUrl = "/assets/materialize-plataform"
      , altImg = "Materialize Plataform - Photo"
      , italic = Nothing
      , title = "Materialize Plataform"
      , desc =
            [ text "A plataform to schedule Specialist and Clients to work together. \n"
            , text "There are implementations such as, Teams management,\n "
            , text "Profiles - Hating - Schedule, Chat/Call rooms to work together and opportunities."
            ]
      , list =
            [ "Elm"
            , "Html"
            , "Sass"
            , "Webpack"
            ]
      , repositoryUrl = Nothing
      , projectLink = "https://app.materialize.pro"
      }
    , { imgUrl = "/assets/revex"
      , altImg = "Revex - Photo"
      , italic = Nothing
      , title = "Revex"
      , desc =
            [ text "Open source boilerplate for Elm. Integrated with Vite, EsBuild, and a lot more.\n"
            , text "I build It to do the process of build a new project with elm a lot easer. "
            , text "I'm also the maintainer of the project, and it was build with all the new cool technologies."
            ]
      , list =
            [ "Elm"
            , "Elm-Spa"
            , "Vite"
            , "Sass"
            , "Tailwind"
            , "EsBuild"
            , "Typescript"
            ]
      , repositoryUrl = Just "https://github.com/Johann-Goncalves-Pereira/Revex"
      , projectLink = "https://main--revex.netlify.app"
      }
    ]
