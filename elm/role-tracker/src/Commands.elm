module Commands exposing (..)

import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import Models exposing (Player)
import Msgs exposing (Msg)
import RemoteData

fetchPlayers : Cmd Msg
fetchPlayers =
    Http.get fetchPlayersUrl (list playerDecoder)
      |> RemoteData.sendRequest
      |> Cmd.map Msgs.OnFetchPlayers


fetchPlayersUrl : String
fetchPlayersUrl =
    "http://localhost:4000/players"

playerDecoder : Decoder Player
playerDecoder =
  decode Player
    |> required "id" string
    |> required "name" string
    |> required "level" int
