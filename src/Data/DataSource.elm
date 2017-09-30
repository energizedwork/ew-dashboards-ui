module Data.DataSource exposing (DataSource, DataSourceAttributes, decoder, defaultAttributes, factory, init, toChannel)

import Json.Decode as Decode exposing (Decoder, field)
import Json.Decode.Pipeline as Pipeline exposing (custom, decode, required, hardcoded)
import Data.UUID as UUID exposing (UUID)


type alias DataSource =
    { uuid : UUID
    , name : String
    }


type alias DataSourceAttributes =
    { name : String
    }


factory : UUID -> DataSourceAttributes -> DataSource
factory uuid atts =
    DataSource
        uuid
        atts.name


decoder : Decoder DataSourceAttributes
decoder =
    decode DataSourceAttributes
        |> required "name" Decode.string


defaultAttributes : DataSourceAttributes
defaultAttributes =
    DataSourceAttributes
        "default datasource"


init : DataSource
init =
    DataSource "?" "unknown"


toChannel : DataSource -> String
toChannel dataSource =
    "dataSource:" ++ dataSource.name ++ ":" ++ dataSource.uuid
