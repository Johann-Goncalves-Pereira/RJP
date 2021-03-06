module Layout exposing (Model, initLayout, rootId, viewClean, viewLayout)

import Gen.Route as Route exposing (Route)
import Html exposing (Attribute, Html, div, footer, header, main_)
import Html.Attributes exposing (class, classList, id)
import Regex



-- Model


type alias Model msg =
    { route : Route
    , rootContent : List (Html msg)
    , rootAttrs : List (Attribute msg)
    , headerContent : List (Html msg)
    , headerAttrs : List (Attribute msg)
    , mainContent : List (Html msg)
    , mainAttrs : List (Attribute msg)
    , footerAttrs : List (Attribute msg)
    , footerContent : List (Html msg)
    }


initLayout : Model msg
initLayout =
    { route = Route.Home_
    , rootContent = []
    , rootAttrs = []
    , headerContent = []
    , headerAttrs = []
    , mainContent = []
    , mainAttrs = []
    , footerAttrs = []
    , footerContent = []
    }



-- Structure


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


rootId : String
rootId =
    "root-id"



-- View


viewLayout : Model msg -> List (Html msg)
viewLayout model =
    let
        mainClass : Attribute msg
        mainClass =
            class <| "root__main main--" ++ classBuilder (routeName model.route)
    in
    div
        ([ id rootId
         , classList
            [ ( "root", True )
            , ( "root--" ++ classBuilder (routeName model.route)
              , True
              )
            ]
         ]
            ++ model.rootAttrs
        )
        ([ header (class "root__header" :: model.headerAttrs) model.headerContent
         , main_ (mainClass :: model.mainAttrs) model.mainContent
         , footer (class "root__footer" :: model.footerAttrs) model.footerContent
         ]
            ++ model.rootContent
        )
        |> List.singleton


viewClean : Model msg -> List (Html msg)
viewClean { rootAttrs, rootContent, route } =
    div
        ([ id rootId
         , classList
            [ ( "root", True )
            , ( "root--"
                    ++ classBuilder (routeName route)
              , True
              )
            ]
         ]
            ++ rootAttrs
        )
        rootContent
        |> List.singleton
