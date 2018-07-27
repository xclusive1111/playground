module Main exposing (..)

import Commands exposing (fetchPlayers)
import Models exposing (Model, initialModel)
import Msgs exposing (Msg)
import Navigation exposing (Location)
import Routing
import Update exposing (update)
import View exposing (view)


init : Location -> ( Model, Cmd Msg )
init location =
  let
      currentRoute =
        Routing.parseLocation location
  in
    (initialModel currentRoute, fetchPlayers)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


---- PROGRAM ----


main : Program Never Model Msg
main =
    Navigation.program Msgs.OnLocationChange
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
