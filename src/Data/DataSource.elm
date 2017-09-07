module Data.DataSource exposing (DataSource, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (custom, decode, required)


decoder : Decoder DataSource
decoder =
    decode DataSource
        |> required "uuid" Decode.string
        |> required "name" Decode.string


type alias DataSource =
    { uuid : String
    , name : String
    }
