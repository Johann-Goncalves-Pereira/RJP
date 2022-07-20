port module Components.Dialog exposing (..)

import Html exposing (Html, div, form, h4, input, label, p, text)
import Html.Attributes as Attr exposing (class)
import Html.Attributes.Aria exposing (ariaLabel)
import Html.Events as Events
import Json.Decode as Decode
import Utils.Func exposing (Patterns(..), getPattern, regexValidate, run)
import Utils.View exposing (button, materialIcon)


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


dialogId : String
dialogId =
    "email-form"


dialogForm : Model -> Html Msg
dialogForm model =
    let
        toggleDialogEvent =
            Decode.succeed
                { message = ToggleDialog dialogId
                , stopPropagation = True
                , preventDefault = True
                }
                |> Events.custom "click"

        model_ =
            model.emailForm

        emailValidation =
            regexValidate (getPattern Email) model_.email

        emailError =
            if empty_.email then
                text ""

            else if emailValidation then
                text ""

            else
                Html.small [ class "error" ]
                    [ text "Please enter a valid email address" ]

        empty_ =
            { email = String.isEmpty model_.email
            , subject = String.isEmpty model_.subject
            }

        datalist_ =
            List.map
                (\x ->
                    Html.option [] [ text x ]
                )
                >> Html.datalist [ Attr.id "subjects" ]

        sendButton =
            [ text "Send" ]
                |> (if emailValidation /= True || String.isEmpty model_.message then
                        button
                            [ class "submit cursor-not-allowed"
                            , Attr.title "Make sure that you has a correct email address and a message"
                            ]

                    else
                        input
                            [ class "submit submit--available cursor-pointer"
                            , Attr.type_ "submit"
                            , Attr.value "Send"
                            , Events.onClick <| WasSend True
                            ]
                   )

        emailForm =
            [ --
              h4 [ class "mt-1 mb-4 font-900 text-3xl" ] [ text "Get In Touch" ]
            , form
                [ class "form"
                , Attr.action "https://formsubmit.co/fa08be0985adfb900e4f77e019cd8557"
                , Attr.method "POST"
                , Attr.novalidate True
                ]
                [ Html.fieldset [ class "form__send-info" ]
                    [ Html.legend [ class "legend" ] [ text "Send Information" ]
                    , div
                        [ class "wrapper"
                        ]
                        [ label
                            [ class "label"
                            , Attr.for "email-user"
                            , if empty_.email then
                                class ""

                              else
                                Attr.style "transform" "translate(0)"
                            ]
                            [ text "Your Email" ]
                        , input
                            [ class "input"
                            , Attr.id "email-user"
                            , Attr.type_ "email"
                            , Attr.name "email"
                            , Events.onInput EmailInput
                            , Attr.required True
                            ]
                            []
                        , emailError
                        ]
                    , div [ class "wrapper" ]
                        [ label
                            [ class "label"
                            , Attr.for "email-subject"
                            , if empty_.subject then
                                class ""

                              else
                                Attr.style "transform" "translate(0)"
                            ]
                            [ text "Subject" ]
                        , input
                            [ class "input"
                            , Attr.id "email-subject"
                            , Attr.list "subjects"
                            , Attr.name "_subject"
                            , Attr.type_ "text"
                            , Events.onInput SubjectInput
                            ]
                            []
                        , datalist_
                            [ "Job Offer"
                            , "Chat about my portfolio"
                            , "Want my help on a project"
                            , "Just get in touch"
                            ]
                        ]
                    ]
                , Html.fieldset [ class "form__message" ]
                    [ Html.legend [ class "legend" ]
                        [ text "Message" ]
                    , Html.textarea
                        [ class "message scroll-style"
                        , Attr.id "email-message"
                        , Attr.name "message"
                        , Attr.placeholder "Your message..."
                        , Attr.required True
                        , Attr.maxlength 10000
                        , Events.onInput MessageInput
                        ]
                        []
                    ]
                , Html.input
                    [ Attr.type_ "hidden"
                    , Attr.name "_next"
                    , Attr.value "https://johann-goncalves-pereira.netlify.app/#email-form"
                    ]
                    []
                , Html.input [ Attr.type_ "hidden", Attr.name "_captcha", Attr.value "false" ] []
                , Html.input [ Attr.type_ "text", Attr.name "_honey", class "hidden" ] []
                , sendButton
                ]
            ]

        emailThanks =
            [ Html.h4 [ class "mt-1 mb-4 font-900 text-3xl" ] [ text "Thank you for getting in contact" ]
            , p [] [ text "I will get back to you as soon as possible." ]
            , materialIcon "mt-1 text-accent-600 text-4xl" "mark_email_unread"
            ]
    in
    dialog dialogId
        [ class "email-from" ]
        (button
            [ class "fixed inset-0 -z-10"
            , ariaLabel "Exit from Email Form"
            , toggleDialogEvent
            ]
            []
            :: (if model.wasSend then
                    emailThanks

                else
                    emailForm
               )
        )
