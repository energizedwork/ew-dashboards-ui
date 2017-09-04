module Data.Widget.Feed exposing (Feed, decoder)

import Data.Widget as Widget exposing (Widget)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)


type alias Feed =
    { articles : List (Widget ())
    , articlesCount : Int
    }



-- SERIALIZATION --


decoder : Decoder Feed
decoder =
    decode Feed
        |> required "articles" (Decode.list Widget.decoder)
        |> required "articlesCount" Decode.int
