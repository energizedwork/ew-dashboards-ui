module Data.DataSource exposing (DataSource, decoder, init)

import Json.Decode as Decode exposing (Decoder, field)
import Json.Decode.Pipeline as Pipeline exposing (custom, decode, required)


type alias DataSource =
    { uuid : String
    , name : String
    }


decoder : Decoder DataSource
decoder =
    decode DataSource
        |> required "uuid" Decode.string
        |> required "name" Decode.string


init : DataSource
init =
    DataSource "?" "unknown"
