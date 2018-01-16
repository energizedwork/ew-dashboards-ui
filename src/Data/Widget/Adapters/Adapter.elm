module Data.Widget.Adapters.Adapter exposing (Adapter(..), decoder)

import Data.Widget.Adapters.Config as AdapterConfig
import Data.Widget.Adapters.LineAndBarAdapter as LineAndBarAdapter
import Data.Widget.Adapters.MetricAdapter as MetricAdapter
import Data.Widget.Adapters.TableAdapter as TableAdapter exposing (defaultConfig)
import Json.Decode as Decode exposing (Decoder, Value, dict, maybe, string)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)


type alias Definition =
    { type_ : String
    , config : Maybe AdapterConfig.Config
    }


type Adapter
    = TABLE AdapterConfig.Config
    | LINE_AND_BAR_CHART AdapterConfig.Config
    | HEAT_MAP
    | METRIC AdapterConfig.Config


decoder : Decoder Adapter
decoder =
    definitionDecoder
        |> Decode.andThen
            (\definition ->
                case definition.type_ of
                    "TABLE" ->
                        Decode.succeed <|
                            TABLE
                                (definition.config |> Maybe.withDefault TableAdapter.defaultConfig)

                    "LINE_CHART" ->
                        Decode.succeed <|
                            TABLE
                                (definition.config |> Maybe.withDefault TableAdapter.defaultConfig)

                    "BAR_CHART" ->
                        Decode.succeed <|
                            TABLE
                                (definition.config |> Maybe.withDefault TableAdapter.defaultConfig)

                    "LINE_AND_BAR_CHART" ->
                        Decode.succeed <|
                            LINE_AND_BAR_CHART
                                (definition.config |> Maybe.withDefault LineAndBarAdapter.defaultConfig)

                    "HEAT_MAP" ->
                        Decode.succeed HEAT_MAP

                    "METRIC" ->
                        Decode.succeed <|
                            METRIC
                                (definition.config |> Maybe.withDefault MetricAdapter.defaultConfig)

                    somethingElse ->
                        Decode.fail <| "Unknown adapter: " ++ somethingElse
            )


definitionDecoder : Decoder Definition
definitionDecoder =
    decode Definition
        |> required "type_" Decode.string
        |> optional "config"
            (maybe
                (Decode.dict Decode.value)
            )
            Nothing
