module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


main =
    Html.beginnerProgram { model = model, view = view, update = update }

-- Model


type alias Model =
    Int


model : Model
model =
    0

-- Update


type Msg
    = Inc
    | Dec


update : Msg -> Model -> Model
update msg model =
    case msg of
        Inc ->
            model + 1

        Dec ->
            model - 1


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Dec ] [ text "-" ]
        , div [] [ text (toString model) ]
        , button [ onClick Inc ] [ text "-" ]
        ]
