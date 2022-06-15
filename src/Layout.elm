module Layout exposing (Model, initLayout, viewLayout)

import Gen.Route as Route exposing (Route)
import Html exposing (Attribute, Html, a, div, header, li, main_, nav, span, text, ul)
import Html.Attributes exposing (class, classList, href, id, tabindex)
import Regex
import Utils.View exposing (materialIcon)



-- Model


type alias Model msg =
    { route : Route
    , mainContent : List (Html msg)
    , mainAttrs : List (Attribute msg)
    }


initLayout : Model msg
initLayout =
    { route = Route.Home_
    , mainContent = []
    , mainAttrs = []
    }



-- Structure


isRoute : Route -> Route -> Bool
isRoute route compare =
    case ( route, compare ) of
        ( Route.Home_, Route.Home_ ) ->
            True

        _ ->
            False


routeName : Route -> String
routeName route =
    let
        strRoute : String
        strRoute =
            Route.toHref route

        getLength =
            String.length strRoute

        getFirstCharacter =
            String.dropRight (getLength - 2) strRoute
                |> String.toUpper
                |> String.dropLeft 1
    in
    if route == Route.Home_ then
        "Home"

    else
        getFirstCharacter
            ++ String.replace "/" " - " (String.dropLeft 2 strRoute)


userReplace : String -> (Regex.Match -> String) -> String -> String
userReplace userRegex replacer string =
    case Regex.fromString userRegex of
        Nothing ->
            string

        Just regex ->
            Regex.replace regex replacer string


classBuilder : String -> String
classBuilder string =
    userReplace "[ ]" (\_ -> "-") string
        |> String.toLower



-- View


viewLayout : Model msg -> List (Html msg)
viewLayout model =
    let
        mainClass : Attribute msg
        mainClass =
            class <| "root__main main--" ++ classBuilder (routeName model.route)
    in
    [ div
        [ id "root"
        , classList
            [ ( "root", True )
            , ( "root--" ++ classBuilder (routeName model.route), True )
            ]
        ]
        [ viewHeader model
        , main_ (mainClass :: model.mainAttrs) model.mainContent
        ]
    ]


viewHeader : Model msg -> Html msg
viewHeader model =
    let
        correctZero =
            String.fromInt >> String.padLeft 2 '0'

        links =
            List.indexedMap
                (\i route ->
                    li [ class "" ]
                        [ a [ href <| "#" ++ route ++ "Id", class "list__link" ]
                            [ span [ class "text-secondary-30" ] [ text <| correctZero i ++ ". " ], text route ]
                        ]
                )
                [ "about", "experience", "work", "contact" ]
    in
    header [ class "root__header" ]
        [ materialIcon "text-secondary-30 font-light" "hive"
        , nav []
            [ links
                ++ [ a [ href "#", class "list__resume" ] [ text "resume" ] ]
                |> ul [ class "list" ]
            ]
        ]
