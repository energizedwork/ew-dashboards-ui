module Data.Widget.Adapters.Adapter exposing (Adapter(..), decoder)

import Data.Widget.Adapters.Config as AdapterConfig
import Data.Widget.Adapters.MetricAdapter as MetricAdapter
import Json.Decode as Decode exposing (Decoder, maybe, dict, string, Value)
import Json.Decode.Pipeline as Pipeline exposing (decode, required, optional)


type alias Definition =
    { type_ : String
    , config : Maybe AdapterConfig.Config
    }


type Adapter
    = TABLE
    | BAR_CHART
    | HEAT_MAP
    | METRIC AdapterConfig.Config


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
        |> optional "config" (maybe (Decode.dict Decode.value)) Nothing
