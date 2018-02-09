module Data.Widget.Adapters.ChartAdapter exposing (adapt, defaultConfig, extractSeriesLabels)

import Data.Widget.Adapters.TableAdapter as TableAdapter
import Data.Widget.Table as Table exposing (Cell, Data, Row)
import Data.Widget.Config as AdapterConfig
import Json.Decode as Json exposing (Value)
import Data.Widget.Adapters.CellRange as CellRange exposing (..)
import Dict exposing (Dict)
import Data.Widget.Chart as Chart
import Array


defaultConfig : Dict String Json.Value
defaultConfig =
    Dict.empty


extractSeriesLabels : String -> Dict String Json.Value -> Table.Data -> Maybe (List String)
extractSeriesLabels key config data =
    let
        defaultSeriesLabelsRange =
            CellRange.emptyRange
    in
        case Dict.get key config of
            Just seriesLabels ->
                let
                    labels =
                        seriesLabels
                            |> Json.decodeValue CellRange.decoder
                            |> Result.withDefault defaultSeriesLabelsRange
                            |> CellRange.extractRows data
                            |> List.map (\a -> Maybe.withDefault "" (List.head a))
                in
                    Just labels

            Nothing ->
                Nothing


adapt : AdapterConfig.Config -> Data -> Chart.Data
adapt optionalConfig data =
    let
        ( headerRow, bodyRows, minValue, maxValue, xLabels ) =
            TableAdapter.adapt optionalConfig data

        chartData =
            List.map (List.map2 (,) headerRow) bodyRows

        indexedChartData =
            Array.toIndexedList (Array.fromList chartData)

        seriesLabels =
            extractSeriesLabels "seriesLabels" optionalConfig data
    in
        Chart.Data
            bodyRows
            chartData
            indexedChartData
            minValue
            maxValue
            xLabels
            seriesLabels
