port module Components.Dialog exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr


port toggleDialog : String -> Cmd msg


type alias Model =
    { emailForm : { email : String, subject : String, message : String }
    }


init : Model
init =
    { emailForm = { email = "", subject = "", message = "" }
    }


type Msg
    = ToggleDialog String
      -- Email Form
    | EmailInput String
    | SubjectInput String
    | MessageInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        emailInit =
            { email = model.emailForm.email
            , subject = model.emailForm.subject
            , message = model.emailForm.message
            }
    in
    case msg of
        ToggleDialog id_ ->
            ( model, toggleDialog id_ )

        EmailInput email_ ->
            ( { model
                | emailForm = { emailInit | email = email_ }
              }
            , Cmd.none
            )

        SubjectInput subject_ ->
            ( { model
                | emailForm = { emailInit | subject = subject_ }
              }
            , Cmd.none
            )

        MessageInput message_ ->
            ( { model
                | emailForm = { emailInit | message = message_ }
              }
            , Cmd.none
            )


dialog : String -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
dialog elementId attr content =
    Html.node "dialog" (Attr.id elementId :: attr) content
