module Data.Widget.Adapters.LineAndBarAdapter exposing (defaultConfig, adapt)

import Data.Widget.Config as AdapterConfig
import Data.Widget.Adapters.ChartAdapter as ChartAdapter exposing (..)
import Data.Widget.Chart as Chart exposing (Data)
import Data.Widget.Table as Table exposing (Cell, Data, Row)
import Dict exposing (Dict)
import Json.Decode as Json exposing (Value)


-- Possible values:
-- "lineRows"
-- "lineSeriesLabels"
-- "barRows"
-- "barSeriesLabels"
-- "xLabels"
-- "xAxisLabel"
-- "yAxisLabel"


defaultConfig : Dict String Json.Value
defaultConfig =
    Dict.empty


cleanseConfig : AdapterConfig.Config -> AdapterConfig.Config
cleanseConfig optionalConfig =
    optionalConfig
        |> Dict.remove "lineRows"
        |> Dict.remove "lineSeriesLabels"
        |> Dict.remove "barRows"
        |> Dict.remove "barSeriesLabels"


normalizeLineChartConfig : AdapterConfig.Config -> Dict String Value
normalizeLineChartConfig combinedConfig =
    extractChartConfig combinedConfig "lineRows" "lineSeriesLabels"


normalizeBarChartConfig : AdapterConfig.Config -> Dict String Value
normalizeBarChartConfig combinedConfig =
    extractChartConfig combinedConfig "barRows" "barSeriesLabels"


tempConfig : Maybe Value -> String -> Dict String Value
tempConfig configValue configKey =
    case configValue of
        Just configValue ->
            Dict.fromList
                [ ( configKey, configValue )
                ]

        Nothing ->
            Dict.empty


extractChartConfig : AdapterConfig.Config -> String -> String -> Dict String Value
extractChartConfig combinedConfig rowsKey seriesLabelKey =
    let
        rowsRange =
            Dict.get rowsKey combinedConfig

        seriesLabelsRange =
            Dict.get seriesLabelKey combinedConfig

        cleansedConfig =
            cleanseConfig combinedConfig

        tempRowConfig =
            tempConfig rowsRange "bodyRows"

        tempSeriesLabelConfig =
            tempConfig seriesLabelsRange "seriesLabels"

        tempChartConfig =
            Dict.union
                tempRowConfig
                tempSeriesLabelConfig
    in
        Dict.union
            tempChartConfig
            cleansedConfig


adapt : AdapterConfig.Config -> Table.Data -> ( Chart.Data, Chart.Data, Maybe String, Maybe String )
adapt combinedConfig data =
    let
        lineChartData =
            ChartAdapter.adapt (normalizeLineChartConfig combinedConfig) data

        barChartData =
            ChartAdapter.adapt (normalizeBarChartConfig combinedConfig) data

        xAxisLabel =
            ChartAdapter.extractXAxisLabel combinedConfig data

        yAxisLabel =
            ChartAdapter.extractYAxisLabel combinedConfig data
    in
        ( lineChartData, barChartData, xAxisLabel, yAxisLabel )
