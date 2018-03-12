module Data.Widget.Adapters.ChartAdapter
    exposing
        ( adapt
        , defaultConfig
        , extractSeriesLabels
        , extractXAxisLabel
        , extractYAxisLabel
        )

import Data.Widget.Adapters.CellPosition as CellPosition exposing (CellPosition(..), decoder, defaultPosition, encode)
import Data.Widget.Adapters.CellRange as CellRange exposing (..)
import Data.Widget.Adapters.TableAdapter as TableAdapter exposing (Orientation(..))
import Data.Widget.Chart as Chart
import Data.Widget.Config as AdapterConfig
import Data.Widget.Table as Table exposing (Cell, Data, Row)
import Dict exposing (Dict)
import Json.Decode as Json exposing (Value)
import Util


-- Public ----------------------------------------------------------------------


defaultConfig : Dict String Json.Value
defaultConfig =
    Dict.empty


adapt : AdapterConfig.Config -> Data -> Orientation -> Chart.Data
adapt optionalConfig data orientation =
    let
        tableData =
            TableAdapter.adapt optionalConfig data orientation

        chartData =
            { tableData
                | seriesLabels =
                    extractSeriesLabels optionalConfig data
                , xAxisLabel =
                    extractXAxisLabel optionalConfig data
                , yAxisLabel =
                    extractYAxisLabel optionalConfig data
                , forecastPosition =
                    extractCellPosition "forecastPosition" optionalConfig data
                        |> Maybe.map String.toInt
                        |> Maybe.map (Result.withDefault 0)
            }
    in
        chartData


extractSeriesLabels : Dict String Json.Value -> Table.Data -> Maybe Table.Cells
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
                            |> CellRange.extractCells data
                            |> Util.flatten2D
                in
                    Just labels

            Nothing ->
                Nothing


extractXAxisLabel : Dict String Json.Value -> Table.Data -> Maybe Cell
extractXAxisLabel config data =
    extractCellPosition "xAxisLabel" config data


extractYAxisLabel : Dict String Json.Value -> Table.Data -> Maybe Cell
extractYAxisLabel config data =
    extractCellPosition "yAxisLabel" config data



-- Private ----------------------------------------------------------------------


extractCellPosition : String -> Dict String Json.Value -> Table.Data -> Maybe Cell
extractCellPosition configKey config data =
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
