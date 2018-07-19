module Main exposing (..)

import Html exposing (Html, button, div, input, text)
import Html.Events exposing (onClick, onInput)
import WebSocket

main : Program Never Model Action
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Action )
init =
    ( Model "" [], Cmd.none )



-- MODEL


type alias Message = String

type alias Model =
    { input : String
    , messages : List Message
    }

-- UPDATE


type Action
    = Input String
    | Send
    | NewMessage Message


update : Action -> Model -> ( Model, Cmd Action )
update msg {input, messages} =
    case msg of
      Input newInput ->
        ( Model newInput messages, Cmd.none )
      Send ->
        ( Model "" messages, WebSocket.send "ws://echo.websocket.org" input )
      NewMessage str ->
        ( Model "" (str :: messages), Cmd.none )


-- Subscriptions
subscriptions : Model -> Sub Action
subscriptions model =
  WebSocket.listen "ws://echo.websocket.org" NewMessage

-- VIEW
view : Model -> Html Action
view model =
  div []
    [ div [] ( List.map viewMessage model.messages )
    , input [ onInput Input ] []
    , button [ onClick Send ] [ text "Send" ]
    ]

viewMessage : Message -> Html node
viewMessage message =
    div [] [ text message ]
