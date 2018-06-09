import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)
import Random


main =
    Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- Model

type alias Model =
    { diceFace : Int
    }

-- Update

type Msg
    = Roll
    | NewFace Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Roll ->
            (model, Random.generate NewFace (Random.int 1 6))
        NewFace newFace ->
            (Model newFace, Cmd.none)


-- View

view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text (toString model.diceFace) ]
        , button [ onClick Roll  ] [ text "Roll" ]
        ]


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- Init

init : (Model, Cmd Msg)
init =
    (Model 1, Cmd.none)
