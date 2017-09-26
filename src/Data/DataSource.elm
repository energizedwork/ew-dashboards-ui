module Data.DataSource exposing (DataSource, decoder, init, toChannel)

import Json.Decode as Decode exposing (Decoder, field)
import Json.Decode.Pipeline as Pipeline exposing (custom, decode, required, hardcoded)


type alias DataSource =
    { uuid : String
    , name : String
    }


decoder : Decoder DataSource
decoder =
    decode DataSource
        |> hardcoded "de2c5277-1a63-4196-a8e5-c2ed62bf89da"
        -- TODO!
        |>
            required "name" Decode.string


init : DataSource
init =
    DataSource "?" "unknown"


toChannel : DataSource -> String
toChannel dataSource =
    "dataSource:" ++ dataSource.name ++ ":" ++ dataSource.uuid
