module Data.Widget.Feed exposing (Feed, decoder)

import Data.Widget as Widget exposing (Widget)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)


type alias Feed =
    { widgets : List (Widget ())
    , widgetsCount : Int
    }



-- SERIALIZATION --


decoder : Decoder Feed
decoder =
    decode Feed
        |> required "widgets" (Decode.list Widget.decoder)
        |> required "widgetsCount" Decode.int
