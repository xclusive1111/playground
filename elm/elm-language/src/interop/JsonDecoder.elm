module Main exposing (..)

import Html exposing (div)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)

-- Primitive decoders
-- string : Decoder String
-- int : Decoder Int
-- float : Decoder Float
-- bool : Decoder Bool

-- decodeString : Decoder a -> String -> Result String a

decodeString int "42"
decodeString float "1.234"
decodeString bool "true"
decodeString int "true"

-- Combining decoders
decodeString (list int) "[1,2,3,4]"

-- Decoding Objects

decodeString (field "x" int) """{ "x": 3, "y": 4 }"""
type alias Point = {x : Int, y : Int}

pointDecoder  = map2 Point (field "x" int) (field "y" int)
decodeString pointDecoder """{ "x": 4, "y": 3 }"""

type alias LatLng = { lat: Float, lng: Float }

latLngDecoder : Decoder LatLng
latLngDecoder =
  decode LatLng
    |> required "lat" float
    |> required "lng" float

type alias User =
  { id : Int
  , email : Maybe String
  , name : String
  , percentExcited : Float
  }

userDecoder : Decoder User
userDecoder =
    map4 User
        (field "id" int)
        (field "email" (maybe string))
        (field "name" string)
        (field "percentExcited" float)

userDecoder_ : Decoder User
userDecoder_ =
  decode User
    |> required "id" int
    |> required "email" (nullable string)
    |> optional "name" string "anonymous user"
    |> hardcoded 1.0


Json.Decode.decodeString userDecoder'
  """
    {"id": 123, "email": "someone@example.com", "percentExcited": (hardcoded)}
  """

-- result should be
-- {id = 123,
-- ,email = "someone@example.com"
-- ,name = "anonymous user"
-- ,percentExcited = 1.0
-- }

type alias Address =
  { country : String
  , province : String
  , street : String
  , postalCode : Maybe Int
  }

type alias VipUser =
  { id : Int
  , name : String
  , age : Maybe Int
  , address : Address
  }

addressDecoder : Decoder Address
addressDecoder =
    decode Address
     |> optional "country" string "VN"
     |> required "province" string
     |> optional "street" string "215 St."
     |> required "postalCode" (nullable int)


vipUserDecoder : Decoder VipUser
vipUserDecoder =
  decode VipUser
    |> required "id" int
    |> required "name" string
    |> required "age" (nullable int)
    |> required "address" addressDecoder


vipUserDecoder_ : Decoder VipUser
vipUserDecoder_ =
  map4 VipUser
      (field "id" int)
      (field "name" string)
      (field "age" (maybe int))
      (field "address"
          (map4 Address
              (field "country" string)
              (field "province" string)
              (field "street" string)
              (field "postalCode" (maybe int))
          )
      )
