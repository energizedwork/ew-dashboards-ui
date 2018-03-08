module Data.Widget.Adapters.LineAndBarAdapter exposing (defaultConfig, adapt)

import Data.Widget.Config as AdapterConfig
import Data.Widget.Adapters.ChartAdapter as ChartAdapter exposing (..)
import Data.Widget.Adapters.TableAdapter exposing (Orientation(..))
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
        |> Dict.remove "lineXAxisLabel"
        |> Dict.remove "lineYAxisLabel"
        |> Dict.remove "barRows"
        |> Dict.remove "barSeriesLabels"
        |> Dict.remove "barXAxisLabel"
        |> Dict.remove "barYAxisLabel"


normalizeLineChartConfig : AdapterConfig.Config -> Dict String Value
normalizeLineChartConfig combinedConfig =
    extractChartConfig combinedConfig "lineRows" "lineSeriesLabels" "lineXAxisLabel" "lineYAxisLabel"


normalizeBarChartConfig : AdapterConfig.Config -> Dict String Value
normalizeBarChartConfig combinedConfig =
    extractChartConfig combinedConfig "barRows" "barSeriesLabels" "barXAxisLabel" "barYAxisLabel"


tempConfig : Maybe Value -> String -> List ( String, Value )
tempConfig configValue configKey =
    let
        cleansedConfig =
            case configValue of
                Just configValue ->
                    Dict.fromList
                        [ ( configKey, configValue )
                        ]

                Nothing ->
                    Dict.empty
    in
        Dict.toList cleansedConfig


extractChartConfig : AdapterConfig.Config -> String -> String -> String -> String -> Dict String Value
extractChartConfig combinedConfig rowsKey seriesLabelKey xAxisLabelKey yAxisLabelKey =
    let
        rowsRange =
            Dict.get rowsKey combinedConfig

        seriesLabelsRange =
            Dict.get seriesLabelKey combinedConfig

        xAxisLabel =
            Dict.get xAxisLabelKey combinedConfig

        yAxisLabel =
            Dict.get yAxisLabelKey combinedConfig

        cleansedConfig =
            cleanseConfig combinedConfig

        tempRowConfig =
            tempConfig rowsRange "bodyRows"

        tempSeriesLabelConfig =
            tempConfig seriesLabelsRange "seriesLabels"

        tempXAxisConfig =
            tempConfig xAxisLabel "xAxisLabel"

        tempYAxisConfig =
            tempConfig yAxisLabel "yAxisLabel"

        tempChartConfig =
            List.concat
                [ tempRowConfig
                , tempSeriesLabelConfig
                , tempXAxisConfig
                , tempYAxisConfig
                ]
                |> Dict.fromList
    in
        Dict.union
            tempChartConfig
            cleansedConfig


adapt : AdapterConfig.Config -> Table.Data -> ( Chart.Data, Chart.Data )
adapt combinedConfig data =
    let
        lineChartData =
            ChartAdapter.adapt
                (normalizeLineChartConfig combinedConfig)
                data
                Vertical

        barChartData =
            ChartAdapter.adapt
                (normalizeBarChartConfig combinedConfig)
                data
                Vertical

        xAxisLabel =
            ChartAdapter.extractXAxisLabel combinedConfig data

        yAxisLabel =
            ChartAdapter.extractYAxisLabel combinedConfig data
    in
        ( lineChartData, barChartData )
