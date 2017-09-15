module Data.DataSourceMessage exposing (DataSourceMessage, decoder)

import Json.Decode as Decode exposing (Decoder, field)
import Json.Decode.Pipeline as Pipeline exposing (custom, decode, required)
import Data.Widget.Table as Table exposing (Data, Cell, decoder)


type alias DataSourceMessage =
    { user : String
    , body : Data
    }


decoder : Decode.Decoder DataSourceMessage
decoder =
    Decode.map2 DataSourceMessage
        (field "user" Decode.string)
        (field "body" Table.decoder)
