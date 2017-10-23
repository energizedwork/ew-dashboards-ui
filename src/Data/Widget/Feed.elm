module Data.Widget.Feed exposing (Feed, decoder)

import Data.Dashboard as Dashboard exposing (Dashboard)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)


type alias Feed =
    { dashboards : List Dashboard
    , dashboardsCount : Int
    }



-- SERIALIZATION --


decoder : Decoder Feed
decoder =
    decode Feed
        |> required "dashboards" (Decode.list Dashboard.decoder)
        |> required "dashboardsCount" Decode.int
