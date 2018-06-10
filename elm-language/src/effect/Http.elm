import Html exposing (button, div, h2, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Http exposing (decodeUri, request)
import Json.Decode as Decode


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( Model "cats" "waiting.gif", Cmd.none )



-- MODEL


type alias Model =
    { topic : String
    , gifUrl : String
    }


-- UPDATE


type Msg
    = MorePlease
    | NewGif (Result Http.Error String)

fn : (String -> String) -> Int
fn sts = 1

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
          (model, getRandomGif model.topic)
        NewGif (Ok newUrl) ->
          ({model | gifUrl = newUrl}, Cmd.none)
        NewGif (Err _) ->
          (model, Cmd.none)



-- VIEW


getRandomGif : String -> Cmd Msg
getRandomGif topic =
  let
      url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
      request = Http.get url decodeGifUrl
  in
      Http.send NewGif request

decodeGifUrl : Decode.Decoder String
decodeGifUrl =
    Decode.at ["data", "image_url"] Decode.string




view : Model -> Html.Html Msg
view model =
  div []
    [ h2 [] [ text model.topic ]
    ,img [ src model.gifUrl ] []
    , button [ onClick MorePlease ] [ text "More Please!" ]
    ]
