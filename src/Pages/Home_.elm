module Pages.Home_ exposing (Model, Msg, page)

import Components.Svg as SVG exposing (Logo(..))
import Gen.Params.Home_ exposing (Params)
import Gen.Route as Route
import Html exposing (Html, a, div, h1, h2, h5, p, section, text)
import Html.Attributes exposing (class, href, id, rel, tabindex, target)
import Html.Attributes.Aria exposing (ariaLabel, ariaLabelledby)
import Layout exposing (initLayout)
import Page
import Request
import Shared
import String exposing (right)
import Svg exposing (desc)
import Svg.Attributes exposing (orientation)
import Utils.View exposing (materialIcon)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page _ _ =
    Page.sandbox
        { init = init
        , update = update
        , view = view
        }



-- INIT


type alias Model =
    {}


init : Model
init =
    {}



-- UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> Model
update msg model =
    case msg of
        ReplaceMe ->
            model



-- VIEW


view : Model -> View Msg
view model =
    { title = "Revex - Home"
    , body =
        Layout.viewLayout
            { initLayout
                | route = Route.Home_
                , mainContent = viewPage model
            }
    }


viewPage : Model -> List (Html Msg)
viewPage _ =
    [ div [ orientation "left", class "main-orientation left-0" ]
        [ div [ class "grid gap-10 select-none mt-auto" ]
            [ materialIcon "text-3xl up" "south_america"
            , materialIcon "text-3xl up" "fingerprint"
            , materialIcon "text-3xl up" "all_inclusive"
            , materialIcon "text-3xl up" "blur_on"
            ]
        ]
    , div [ class "main" ] []
    , div [ orientation "right", class "main-orientation right-0" ]
        [ a [ class "email up" ] [ text "johann.gon.pereira@gmail.com" ]
        ]
    ]
