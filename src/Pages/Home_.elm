module Pages.Home_ exposing (Model, Msg, page)

import Components.Svg as SVG exposing (Logo(..))
import Gen.Params.Home_ exposing (Params)
import Gen.Route as Route
import Html exposing (Html, a, div, h1, h2, h5, section, text)
import Html.Attributes exposing (class, href, id, rel, tabindex, target)
import Html.Attributes.Aria exposing (ariaLabel, ariaLabelledby)
import Layout exposing (initLayout)
import Page
import Request
import Shared
import Svg exposing (desc)
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
                , mainContent = []
            }
    }
