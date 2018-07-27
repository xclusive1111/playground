module Update exposing (..)

import Models exposing (..)
import Msgs exposing (..)
import Routing exposing (parseLocation)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
      OnFetchPlayers response   ->
        ({ model | players = response }, Cmd.none)
      OnLocationChange location ->
        let
            newRoute = parseLocation location
        in
          ({ model | route = newRoute }, Cmd.none)
