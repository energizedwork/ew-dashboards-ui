module Data.DataSourceMessage exposing (DataSourceMessage, decoder)

import Json.Decode as Decode exposing (Decoder, field)
import Data.Widget.Table as Table exposing (Data, Cell, decoder)


type alias DataSourceMessage =
    { user : String
    , body : Data
    , uuid : String
    }


decoder : Decode.Decoder DataSourceMessage
decoder =
    Decode.map3 DataSourceMessage
        (field "user" Decode.string)
        (field "body" Table.decoder)
        (field "uuid" Decode.string)
