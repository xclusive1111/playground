module Commands exposing (..)

import Http exposing (expectJson)
import Json.Decode exposing (..)
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (decode, required)
import Models exposing (Player, PlayerId)
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

savePlayerUrl : PlayerId -> String
savePlayerUrl playerId =
    "http://localhost:4000/players/" ++ playerId

savePlayerRequest : Player -> Http.Request Player
savePlayerRequest player =
    Http.request
        { body = playerEncoder player |> Http.jsonBody
        , method = "PATCH"
        , expect = Http.expectJson playerDecoder
        , headers = []
        , timeout = Nothing
        , url = savePlayerUrl player.id
        , withCredentials = False
        }

savePlayerCmd : Player -> Cmd Msg
savePlayerCmd player =
    savePlayerRequest player
        |> Http.send Msgs.OnPlayerSave

playerEncoder : Player -> Encode.Value
playerEncoder player =
    let
        attributes =
            [ ("id", Encode.string player.id)
            , ("name", Encode.string player.name)
            , ("level", Encode.int player.level)
            ]
    in
        Encode.object attributes
