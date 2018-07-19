module Main exposing (..)

import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, style, type_)
import Html.Events exposing (onClick, onInput)


main : Program Never Model Action
main =
    Html.beginnerProgram { model = model, view = view, update = update }

-- Model

type alias Model =
    { name: String
    , password: String
    , rePassword: String
    }

model : Model
model = Model "" "" ""


-- Update

type Action = Name String
         | Password String
         | RePassword String


update : Action -> Model -> Model
update msg model = case msg of
        Name name -> { model | name = name }
        Password pwd -> { model | password = pwd }
        RePassword rpwd -> { model | rePassword = rpwd }



-- View

view : Model -> Html Action
view model =
    div []
        [ input [ type_ "text", placeholder "Name", onInput Name ] []
        , input [ type_ "password", placeholder "Password", onInput Password ] []
        , input [ type_ "password", placeholder "Confirm password", onInput RePassword ] []
        , viewValidation model
        ]


viewValidation : Model -> Html mgs
viewValidation model =
    let
        (color, message) =
            if model.password == model.rePassword then
                ("green", "OK")
            else
                ("green", "Password do not match!")
    in
        div [style [("color", color)]]
            [ text message ]
