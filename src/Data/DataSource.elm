module Data.DataSource exposing (DataSource, decoder)

import Json.Decode as Decode exposing (Decoder, field)
import Json.Decode.Pipeline as Pipeline exposing (custom, decode, required)
import Data.Widget.Table as Table exposing (Data, Cell, decoder)


type alias DataSource =
    { uuid : String
    , name : String
    }


decoder : Decoder DataSource
decoder =
    decode DataSource
        |> required "uuid" Decode.string
        |> required "name" Decode.string
