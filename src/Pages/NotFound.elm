module Pages.NotFound exposing (page)

import Gen.Params.NotFound exposing (Params)
import Gen.Route as Route exposing (Route, toHref)
import Html exposing (Attribute, Html, a, h1, h2, text)
import Html.Attributes as Attr exposing (class, href, tabindex)
import Layout exposing (initLayout, viewClean)
import Page exposing (Page)
import Request
import Shared
import Storage exposing (Scheme(..), Storage, Theme)
import Utils.View exposing (customProp)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page
page shared req =
    Page.static
        { view = view shared.storage
        }


view : Storage -> View msg
view storage =
    { title = "Johann - 404"
    , body =
        viewClean
            { initLayout
                | route = Route.NotFound
                , rootAttrs = pageAttr storage
                , rootContent =
                    viewPage
            }
    }


pageAttr : Storage -> List (Attribute msg)
pageAttr storage =
    let
        theme_ =
            storage.theme

        hue_ =
            String.fromInt theme_.hue

        scheme_ =
            if theme_.scheme == Light then
                "light"

            else
                ""
    in
    [ class <| scheme_ ++ "h-full"
    , customProp "page-hue" hue_
    ]


viewPage : List (Html msg)
viewPage =
    [ Html.main_ [ class "grid place-content-center h-full" ]
        [ a
            [ class "hover:text-accent-600 focus-visible:text-accent-600 transition-colors"
            , tabindex 0
            , href <| toHref Route.Home_
            ]
            [ h1 [ class "font-900 font-mono text-9xl " ] [ text "404" ]
            , h2 [ class "w-fit m-auto" ] [ text "Not Found" ]
            ]
        ]
    ]
