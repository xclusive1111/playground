module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)


---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Action )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Action
    = MorePlease


update : Action -> Model -> ( Model, Cmd Action )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Action
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        ]



---- PROGRAM ----


main : Program Never Model Action
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
