module Data.Widget.Adapters.Adapter exposing (Adapter(..), decoder)

import Data.Widget.Adapters.MetricAdapter as MetricAdapter exposing (CellConfig, Config, defaultConfig)
import Json.Decode as Decode exposing (Decoder, index, int, map2, maybe)
import Json.Decode.Pipeline as Pipeline exposing (custom, decode, hardcoded, required, optional)


type alias Definition =
    { type_ : String
    , config : Maybe Config
    }


type Adapter
    = TABLE
    | BAR_CHART
    | HEAT_MAP
    | METRIC MetricAdapter.Config


decoder : Decoder Adapter
decoder =
    definitionDecoder
        |> Decode.andThen
            (\definition ->
                case definition.type_ of
                    "TABLE" ->
                        Decode.succeed TABLE

                    "BAR_CHART" ->
                        Decode.succeed BAR_CHART

                    "HEAT_MAP" ->
                        Decode.succeed HEAT_MAP

                    "METRIC" ->
                        Decode.succeed <| METRIC (definition.config |> Maybe.withDefault MetricAdapter.defaultConfig)

                    somethingElse ->
                        Decode.fail <| "Unknown adapter: " ++ somethingElse
            )


definitionDecoder : Decoder Definition
definitionDecoder =
    decode Definition
        |> required "type_" Decode.string
        |> optional "config" (maybe adapterConfigDecoder) Nothing



-- TODO should CellConfig be in it's own module? Not sure it lives here.


adapterConfigDecoder : Decoder Config
adapterConfigDecoder =
    decode Config
        |> required "sourceCell" cellConfigDecoder
        |> required "targetCell" cellConfigDecoder


cellConfigDecoder : Decoder CellConfig
cellConfigDecoder =
    -- Decoding an array to a tuple https://stackoverflow.com/a/47041770
    -- Why is CellConfig not available here? The following line throws a compiler error `Cannot find variable `CellConfig``
    -- But CellConfig is exposed by MetricAdapter 🤔
    -- map2 CellConfig (index 0 int) (index 1 int)
    map2 (,) (index 0 int) (index 1 int)
