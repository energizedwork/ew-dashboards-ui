module Data.Widget.Adapters.ChartAdapter exposing (adapt, defaultConfig, extractSeriesLabels)

import Data.Widget.Adapters.TableAdapter as TableAdapter
import Data.Widget.Table as Table exposing (Cell, Data, Row)
import Data.Widget.Config as AdapterConfig
import Json.Decode as Json exposing (Value)
import Data.Widget.Adapters.CellRange as CellRange exposing (..)
import Dict exposing (Dict)


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


adapt : AdapterConfig.Config -> Data -> ( Row, List Row, Float, Float, Row, Maybe (List String) )
adapt optionalConfig data =
    let
        ( headerRow, bodyRows, minValue, maxValue, xLabels ) =
            TableAdapter.adapt optionalConfig data

        seriesLabels =
            extractSeriesLabels "seriesLabels" optionalConfig data
    in
        ( headerRow, bodyRows, minValue, maxValue, xLabels, seriesLabels )
