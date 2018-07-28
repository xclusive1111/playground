module Update exposing (..)

import Commands exposing (savePlayerCmd)
import Models exposing (..)
import Msgs exposing (..)
import RemoteData exposing (RemoteData)
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
      ChangeLevel player amount ->
          let
              updatedPlayer = { player | level = player.level + amount }
          in
              (model, savePlayerCmd updatedPlayer)
      OnPlayerSave (Ok player)  ->
          (updatePlayer model player, Cmd.none)
      OnPlayerSave (Err err)  ->
          (model, Cmd.none)


updatePlayer : Model -> Player -> Model
updatePlayer model updatedPlayer =
  let
      pick currentPlayer =
        if updatedPlayer.id == currentPlayer.id then
          updatedPlayer
        else
          currentPlayer

      updatedPlayerList players =
          List.map pick players
      updatedPlayers =
          RemoteData.map updatedPlayerList model.players
  in
      { model | players = updatedPlayers }
