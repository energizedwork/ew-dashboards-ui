module Data.Widget.Adapters.LineAndBarAdapter exposing (defaultConfig, adapt)

import Array
import Data.Widget.Adapters.CellPosition as CellPosition exposing (CellPosition, encode, decoder)
import Data.Widget.Adapters.CellRange as CellRange exposing (..)
import Data.Widget.Adapters.Config as AdapterConfig
import Data.Widget.Adapters.TableAdapter as TableAdapter exposing (..)
import Data.Widget.Table as Table exposing (Cell, Data, Row)
import Dict exposing (Dict)
import Json.Decode as Json exposing (Value)
import Json.Encode as Encode exposing (Value)
import List.Extra
import Views.Widget.Renderers.Utils as Utils exposing (..)


xLabelsIndex : Int
xLabelsIndex =
    1



-- Possible values:
-- "lineRows"
-- "barRows"
-- "xLabelsIndex"


defaultConfig : Dict String Json.Value
defaultConfig =
    Dict.fromList
        [ ( "xLabelsIndex", Encode.int xLabelsIndex )
        ]


adapt : AdapterConfig.Config -> Data -> ( Row, List Row, List Row, Float, Float, List String )
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

        xLabels =
            lineChartHeaderRow

        headerRow =
            xLabels

        minValue =
            List.minimum [ barChartMinValue, lineChartMinValue ]
                |> Maybe.withDefault 0

        maxValue =
            List.maximum [ barChartMaxValue, lineChartMaxValue ]
                |> Maybe.withDefault 0
    in
        ( headerRow, lineChartRows, barChartRows, minValue, maxValue, xLabels )
