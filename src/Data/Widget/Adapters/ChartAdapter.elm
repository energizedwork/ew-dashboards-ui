module Data.Widget.Adapters.ChartAdapter
    exposing
        ( adapt
        , defaultConfig
        , extractSeriesLabels
        , extractXAxisLabel
        , extractYAxisLabel
        )

import Data.Widget.Adapters.TableAdapter as TableAdapter
import Data.Widget.Adapters.CellPosition as CellPosition exposing (CellPosition(..), encode, decoder, defaultPosition)
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


extractSeriesLabels : Dict String Json.Value -> Table.Data -> Maybe (List String)
extractSeriesLabels config data =
    let
        defaultSeriesLabelsRange =
            CellRange.defaultRange
    in
        case Dict.get "seriesLabels" config of
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


extractXAxisLabel : Dict String Json.Value -> Table.Data -> Maybe Cell
extractXAxisLabel config data =
    extractAxisLabel "xAxisLabel" config data


extractYAxisLabel : Dict String Json.Value -> Table.Data -> Maybe Cell
extractYAxisLabel config data =
    extractAxisLabel "yAxisLabel" config data


extractAxisLabel : String -> Dict String Json.Value -> Table.Data -> Maybe Cell
extractAxisLabel configKey config data =
    case Dict.get configKey config of
        Just labelPosition ->
            let
                position =
                    labelPosition
                        |> Json.decodeValue CellPosition.decoder
                        |> Result.withDefault CellPosition.defaultPosition

                cell =
                    CellRange.extractCell data position
            in
                Just cell

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
            extractSeriesLabels optionalConfig data

        xAxisLabel =
            extractXAxisLabel optionalConfig data

        yAxisLabel =
            extractYAxisLabel optionalConfig data
    in
        Chart.Data
            bodyRows
            chartData
            indexedChartData
            minValue
            maxValue
            xLabels
            seriesLabels
            xAxisLabel
            yAxisLabel
