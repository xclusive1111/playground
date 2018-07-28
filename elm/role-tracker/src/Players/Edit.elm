module Players.Edit exposing (..)

import Html exposing (Html, a, button, div, i, span, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Models exposing (Player)
import Msgs exposing (Msg)
import Routing exposing (playersPath)

view : Player -> Html Msg
view model =
  div []
      [ nav model
      , form model
      ]

nav : Player -> Html Msg
nav player =
    div [ class "clearfix mb2 white bg-black p1" ] [ listBtn ]

form : Player -> Html Msg
form player =
    div [ class "m3" ]
        [ div [ class "col col-5" ]
              [ text "Level" ]
        , div [ class "col col-7" ]
              [ span [ class "h2 bold" ]
                     [ text (toString player.level) ]
              , btnLevelDecrease player
              , btnLevelIncrease player
              ]
        ]

btnLevelDecrease : Player -> Html Msg
btnLevelDecrease player =
  let
      message = Msgs.ChangeLevel player -1
  in
      button [ class "icon-minus", onClick message ] [ text "-" ]

btnLevelIncrease : Player -> Html Msg
btnLevelIncrease player =
  let
      message = Msgs.ChangeLevel player 1
  in
      button [ class "icon-plus", onClick message ] [ text "+" ]

listBtn : Html Msg
listBtn =
    a [ class "btn regular", href playersPath ]
      [ i [ class "fa fa-chevron-left mr1" ] []
      , text "List"
      ]
