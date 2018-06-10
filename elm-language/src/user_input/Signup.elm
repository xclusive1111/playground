module Main exposing (..)

import Html exposing (Html, beginnerProgram, br, button, form, input, label, table, td, text, tr)
import Html.Attributes exposing (class, placeholder, style, type_, value)
import Html.Events exposing (onInput, onSubmit)
import List exposing (all)
import Validate exposing (Validator, ifBlank, ifNotInt, validate)


main =
    Html.beginnerProgram { view = viewForm, update = update, model = model }



-- Model


type alias Model =
    { username : String
    , email : String
    , password : String
    , rePassword : String
    , errors : List Error
    }


type FormField
    = Email
    | Username
    | Password
    | ConfirmPassword


model : Model
model =
    Model "" "" "" "" []



-- Update


type Action
    = SetUserName String
    | SetEmail String
    | SetPassword String
    | SetConfirmPassword String
    | Submit


update : Action -> Model -> Model
update msg model =
    case msg of
        SetUserName uname ->
            { model | username = uname }

        SetEmail email ->
            { model | email = email }

        SetPassword pwd ->
            { model | password = pwd }

        SetConfirmPassword rePwd ->
            { model | rePassword = rePwd }

        Submit ->
            case validate modelValidator model of
                [] ->
                    { model | errors = [] }

                errList ->
                    { model | errors = errList }


type alias Error =
    ( FormField, String )


modelValidator : Validator Error Model
modelValidator =
    Validate.all
        [ ifBlank .username ( Username, "Please enter your username." )
        , ifBlank .email ( Email, "Please enter your email address." )
        , ifBlank .password ( Password, "Please enter your password." )
        , ifBlank .rePassword ( ConfirmPassword, "Please confirm your password." )
        ]


viewFormErrors : FormField -> List Error -> Html Action
viewFormErrors field errors =
    errors
        |> List.filter (\( fieldError, _ ) -> fieldError == field)
        |> List.map (\( _, err ) -> text err)
        |> td [ style [ ( "color", "red" ) ] ]



-- TODO: getOrElse
-- View


viewForm : Model -> Html Action
viewForm model =
    form [ onSubmit Submit, class "ui form" ]
        [ table []
            [ tr []
                [ td [] [ text "username" ]
                , td [] [ input [ type_ "text", placeholder "username", value model.username, onInput SetUserName ] [] ]
                , viewFormErrors Username model.errors
                ]
            , tr []
                [ td [] [ text "email" ]
                , td [] [ input [ type_ "email", placeholder "email", value model.email, onInput SetEmail ] [] ]
                , viewFormErrors Email model.errors
                ]
            , tr []
                [ td [] [ text "password" ]
                , td [] [ input [ type_ "password", placeholder "password", value model.password, onInput SetPassword ] [] ]
                , viewFormErrors Password model.errors
                ]
            , tr []
                [ td [] [ text "confirm password" ]
                , td [] [ input [ type_ "password", placeholder "confirm password", value model.rePassword, onInput SetConfirmPassword ] [] ]
                , viewFormErrors ConfirmPassword model.errors
                ]
            , tr []
                [ td [] []
                , td [ style [ ( "text-align", "left" ) ] ] [ button [] [ text "submit" ] ]
                ]
            ]
        ]
