module Data.Widget.Adapters.Adapter exposing (Adapter(..), decoder)

import Data.Widget.Adapters.CellPosition as CellPosition exposing (CellPosition, decoder)
import Data.Widget.Adapters.MetricAdapter as MetricAdapter
import Json.Decode as Decode exposing (Decoder, maybe, dict, string)
import Json.Decode.Pipeline as Pipeline exposing (decode, required, optional)
import Dict exposing (Dict)


type alias Config =
    Dict String CellPosition


type alias Definition =
    { type_ : String
    , config : Maybe Config
    }


type Adapter
    = TABLE
    | BAR_CHART
    | HEAT_MAP
    | METRIC Config


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
        |> optional "config" (maybe (dict CellPosition.decoder)) Nothing
