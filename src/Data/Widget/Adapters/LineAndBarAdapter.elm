module Data.Widget.Adapters.LineAndBarAdapter exposing (defaultConfig, adapt)

import Array
import Data.Widget.Config as AdapterConfig
import Data.Widget.Adapters.TableAdapter as TableAdapter exposing (..)
import Data.Widget.Chart as Chart exposing (Data)
import Data.Widget.Table as Table exposing (Cell, Data, Row)
import Dict exposing (Dict)
import Json.Decode as Json exposing (Value)


-- Possible values:
-- "lineRows"
-- "barRows"
-- "xLabels"


defaultConfig : Dict String Json.Value
defaultConfig =
    Dict.empty


adapt : AdapterConfig.Config -> Table.Data -> ( Chart.Data, Chart.Data )
adapt optionalConfig data =
    let
        lineRowsRange =
            Dict.get "lineRows" optionalConfig

        barRowsRange =
            Dict.get "barRows" optionalConfig

        cleansedConfig =
            optionalConfig
                |> Dict.remove "lineRows"
                |> Dict.remove "barRows"

        tempLineChartConfig =
            case lineRowsRange of
                Just lineRowsRange ->
                    Dict.fromList
                        [ ( "bodyRows", lineRowsRange )
                        ]

                Nothing ->
                    Dict.empty

        lineChartConfig =
            Dict.union
                cleansedConfig
                tempLineChartConfig

        ( lineChartHeaderRow, lineChartRows, lineChartMinValue, lineChartMaxValue, lineChartXLabels ) =
            TableAdapter.adapt lineChartConfig data

        tempBarChartConfig =
            case barRowsRange of
                Just barRowsRange ->
                    Dict.fromList
                        [ ( "bodyRows", barRowsRange )
                        ]

                Nothing ->
                    Dict.empty

        barChartConfig =
            Dict.union
                cleansedConfig
                tempBarChartConfig

        ( barChartHeaderRow, barChartRows, barChartMinValue, barChartMaxValue, barChartXLabels ) =
            TableAdapter.adapt barChartConfig data

        lineData =
            List.map (List.map2 (,) lineChartHeaderRow) lineChartRows

        barData =
            List.map (List.map2 (,) barChartHeaderRow) barChartRows

        indexedLineData =
            Array.toIndexedList (Array.fromList lineData)

        indexedBarData =
            Array.toIndexedList (Array.fromList barData)

        lineChartData =
            Chart.Data lineChartRows
                lineData
                indexedLineData
                lineChartMinValue
                lineChartMaxValue
                lineChartXLabels

        barChartData =
            Chart.Data
                barChartRows
                barData
                indexedBarData
                barChartMinValue
                barChartMaxValue
                barChartXLabels
    in
        ( lineChartData, barChartData )
