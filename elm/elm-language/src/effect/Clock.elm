module Main exposing (..)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (type_)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)


main : Program Never Model Action
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL
type alias Model =
  { time : Time
  , paused : Bool
  }


init : ( Model, Cmd Action )
init =
    ( Model 0 True, Cmd.none )

type Action
  = Tick Time
  | Pause
  | Start


-- UPDATE

update : Action -> Model -> ( Model, Cmd Action )
update msg model =
    case msg of
        Tick newTime ->
          ( Model newTime False, Cmd.none )
        Pause ->
          ( { model | paused = True }, Cmd.none)
        Start ->
          ( { model | paused = False }, Cmd.none )


-- Subscriptions

subscriptions : Model -> Sub Action
subscriptions model =
  if model.paused then
    Sub.none
  else
    Time.every second Tick

-- VIEW
view : Model -> Html Action
view model =
  let
    angle = turns (Time.inMinutes model.time)
    handX = toString (50 + 40 * cos angle)
    handY = toString (50 + 40 * sin angle)
  in
    div []
    [ svg [ viewBox "0 0 100 100", width "300px"]
      [ circle [cx "50", cy "50", r "45", fill "#0B79CE"] []
      , line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963"] []
      ]
    , button [ Html.Attributes.type_ "button", onClick Pause ] [ Html.text "Pause" ]
    , button [ Html.Attributes.type_ "button", onClick Start ] [ Html.text "Start" ]
    ]
