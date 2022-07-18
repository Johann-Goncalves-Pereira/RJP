port module Components.Dialog exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Utils.Func exposing (run)


port toggleDialog : String -> Cmd msg


type alias Model =
    { emailForm : { email : String, subject : String, message : String }
    , wasSend : Bool
    }


init : Model
init =
    { emailForm = { email = "", subject = "", message = "" }
    , wasSend = False
    }


type Msg
    = ToggleDialog String
      -- Email Form
    | EmailInput String
    | SubjectInput String
    | MessageInput String
    | WasSend Bool


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
            ( model
            , Cmd.batch
                [ toggleDialog id_
                , run <| WasSend False
                ]
            )

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

        WasSend wasSend_ ->
            ( { model
                | wasSend = wasSend_
                , emailForm = { email = "", subject = "", message = "" }
              }
            , Cmd.none
            )


dialog : String -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
dialog elementId attr content =
    Html.node "dialog" (Attr.id elementId :: attr) content
