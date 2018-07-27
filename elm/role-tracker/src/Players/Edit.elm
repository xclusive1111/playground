module Players.Edit exposing (..)

import Html exposing (Html, a, button, div, i, span, text)
import Html.Attributes exposing (class, href)
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
  button [ class "icon-minus" ] [ text "-" ]

btnLevelIncrease : Player -> Html Msg
btnLevelIncrease player =
  button [ class "icon-plus" ] [ text "+" ]

listBtn : Html Msg
listBtn =
    a [ class "btn regular", href playersPath ]
      [ i [ class "fa fa-chevron-left mr1" ] []
      , text "List"
      ]
