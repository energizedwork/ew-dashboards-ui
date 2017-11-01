module Data.Widget.Adapters.LineAndBarAdapter exposing (Config, defaultConfig, adapt)

import Data.Widget.Table as Table exposing (Data, Row, Cell)
import List.Extra
import Views.Widget.Renderers.Utils as Utils exposing (..)


type alias Config =
    { lineRows : List Row
    , barRows : List Row
    , xLabelsIndex : Int
    , yLabelsIndicies : List Int
    }


defaultConfig : Config
defaultConfig =
    Config [] [] 0 [ 0 ]


adapt : Config -> Data -> ( Row, List Row, List Row, Float, List String, List String )
adapt config data =
    let
        headerRow =
            case List.head data.rows of
                Just header ->
                    header

                Nothing ->
                    []

        xLabels =
            case List.Extra.getAt config.xLabelsIndex data.rows of
                Just xLabel ->
                    xLabel

                Nothing ->
                    []

        yLabels =
            case List.tail data.rows of
                Just body ->
                    body
                        |> List.map
                            (\row ->
                                List.head row |> Maybe.withDefault ""
                            )

                Nothing ->
                    []

        bodyRows =
            case List.tail data.rows of
                Just body ->
                    body

                Nothing ->
                    []

        ( lineChartRows, barChartRows ) =
            List.Extra.splitAt (totalDataRows // 2) bodyRows

        totalDataRows =
            List.length bodyRows

        rowMax row =
            List.maximum (Utils.rowToFloats row) |> Maybe.withDefault 0

        bodyRowMaxes =
            List.map rowMax bodyRows

        maxValue =
            (List.maximum bodyRowMaxes |> Maybe.withDefault 0)

        firstCellPerRow row =
            List.head row |> Maybe.withDefault ""
    in
        ( headerRow, lineChartRows, barChartRows, maxValue, xLabels, yLabels )
