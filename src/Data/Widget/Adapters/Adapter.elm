module Data.Widget.Adapters.Adapter exposing (Adapter(..), decoder)

import Data.Widget.Config as AdapterConfig
import Data.Widget.Adapters.MetricAdapter as MetricAdapter
import Data.Widget.Definition as Definition exposing (decoder)
import Json.Decode as Decode exposing (Decoder, Value, dict, maybe, string)


type Adapter
    = TABLE AdapterConfig.Config
    | CHART AdapterConfig.Config
    | LINE_AND_BAR_CHART AdapterConfig.Config
    | HEAT_MAP
    | METRIC AdapterConfig.Config


decoder : Decoder Adapter
decoder =
    Definition.decoder
        |> Decode.andThen
            (\definition ->
                case definition.type_ of
                    "TABLE" ->
                        Decode.succeed <|
                            TABLE
                                (definition.config |> Maybe.withDefault AdapterConfig.default)

                    "LINE_CHART" ->
                        Decode.succeed <|
                            CHART
                                (definition.config |> Maybe.withDefault AdapterConfig.default)

                    "BAR_CHART" ->
                        Decode.succeed <|
                            CHART
                                (definition.config |> Maybe.withDefault AdapterConfig.default)

                    "BAR_CHART_HORIZONTAL" ->
                        Decode.succeed <|
                            CHART
                                (definition.config |> Maybe.withDefault AdapterConfig.default)

                    "LINE_AND_BAR_CHART" ->
                        Decode.succeed <|
                            LINE_AND_BAR_CHART
                                (definition.config |> Maybe.withDefault AdapterConfig.default)

                    "HEAT_MAP" ->
                        Decode.succeed HEAT_MAP

                    "METRIC" ->
                        Decode.succeed <|
                            METRIC
                                (definition.config |> Maybe.withDefault MetricAdapter.defaultConfig)

                    somethingElse ->
                        Decode.fail <| "Unknown adapter: " ++ somethingElse
            )
