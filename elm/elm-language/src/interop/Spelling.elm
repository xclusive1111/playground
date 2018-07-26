port module Spelling exposing (..)

import Html exposing (button, div, input, program, text, Html)
import Html.Events exposing (onClick, onInput)

main : Program Never Model Msg
main = program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

-- MODEL

type alias Model =
  { word : String
  , suggestions: List String
  }

init : (Model, Cmd msg)
init = (Model "" [], Cmd.none)


-- UPDATE

type Msg
      = Change String
      | Check
      | Suggest (List String)

port check : String -> Cmd msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Change newWord ->
            (Model newWord [], Cmd.none)
        Check ->
            (model, check model.word)
        Suggest newSuggestions ->
            (Model model.word newSuggestions, Cmd.none)

-- Supcriptions

port suggestions : (List String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  suggestions Suggest

-- view

view : Model -> Html Msg
view model =
    div []
        [ input [onInput Change] []
        , button [ onClick Check ] [ text "Check" ]
        , div [] [ text (String.join ", " model.suggestions) ]
        ]
