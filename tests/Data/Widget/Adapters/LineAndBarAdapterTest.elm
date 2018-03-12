module Data.Widget.Adapters.LineAndBarAdapterTest exposing (..)

import Data.Widget.Adapters.AdapterTestData as TD
import Data.Widget.Adapters.CellPosition as CellPosition exposing (CellPosition(..), decoder, encode)
import Data.Widget.Adapters.CellRange as CellRange exposing (CellRange, encode)
import Data.Widget.Adapters.LineAndBarAdapter as LineAndBarAdapter exposing (adapt)
import Data.Widget.Config as AdapterConfig
import Data.Widget.Table as Table exposing (Data)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode exposing (..)
import List.Extra
import Test exposing (..)


adapterConfigTest : Test
adapterConfigTest =
    let
        -- setup data
        unlabelledInput =
            Data
                [ TD.headerRow
                , TD.firstRow
                , TD.secondRow
                , TD.thirdRow
                , TD.forthRow
                ]

        labelledInput =
            Data
                [ TD.headerRow
                , TD.firstRow ++ [ "Label 1" ]
                , TD.secondRow ++ [ "Label 2" ]
                , TD.thirdRow ++ [ "Label 3" ]
                , TD.forthRow ++ [ "Label 4" ]
                ]

        defaultConfig =
            LineAndBarAdapter.defaultConfig

        suppliedJSONConfig =
            """
            {
              "line": {
                  "bodyRows": {
                    "start": [5, 2],
                    "end": [10, 3]
                  },
                  "seriesLabels": {
                    "start": [13, 2],
                    "end": [13, 5]
                  },
                  "xLabels": {
                    "start": [5, 3],
                    "end": [10, 3]
                  },
                  "xAxisLabel": [13, 2],
                  "yAxisLabel": [13, 3]
              },
              "bar": {
                  "bodyRows": {
                    "start": [5, 4],
                    "end": [10, 5]
                  },
                  "seriesLabels": {
                    "start": [13, 2],
                    "end": [13, 5]
                  },
                  "xLabels": {
                    "start": [5, 3],
                    "end": [10, 3]
                  },
                  "yAxisLabel": [13, 4]
              }
            }
            """

        suppliedConfig =
            Decode.decodeString AdapterConfig.decoder suppliedJSONConfig
                |> Result.withDefault AdapterConfig.default

        -- functions under test!
        ( defaultLineChart, defaultBarChart ) =
            LineAndBarAdapter.adapt defaultConfig unlabelledInput

        ( suppliedLineChart, suppliedBarChart ) =
            LineAndBarAdapter.adapt suppliedConfig labelledInput

        -- expectations
        defaultLineChartRows =
            \_ ->
                defaultLineChart.rows
                    |> Expect.equal
                        [ TD.firstRow
                        , TD.secondRow
                        , TD.thirdRow
                        , TD.forthRow
                        ]

        defaultLineSeriesLabels =
            \_ ->
                defaultLineChart.seriesLabels
                    |> Expect.equal
                        Nothing

        defaultBarChartRows =
            \_ ->
                defaultBarChart.rows
                    |> Expect.equal
                        [ TD.firstRow
                        , TD.secondRow
                        , TD.thirdRow
                        , TD.forthRow
                        ]

        defaultBarSeriesLabels =
            \_ ->
                defaultBarChart.seriesLabels
                    |> Expect.equal
                        Nothing

        defaultLineMinValue =
            \_ -> defaultLineChart.minValue |> Expect.equal 101

        defaultLineMaxValue =
            \_ -> defaultLineChart.maxValue |> Expect.equal 412

        defaultBarMinValue =
            \_ -> defaultBarChart.minValue |> Expect.equal 101

        defaultBarMaxValue =
            \_ -> defaultBarChart.maxValue |> Expect.equal 412

        defaultXLabels =
            \_ -> defaultLineChart.xLabels |> Expect.equal (Just TD.headerRow)

        defaultXAxisLabel =
            \_ -> defaultLineChart.xAxisLabel |> Expect.equal Nothing

        defaultLineYAxisLabel =
            \_ -> defaultLineChart.yAxisLabel |> Expect.equal Nothing

        defaultBarYAxisLabel =
            \_ -> defaultBarChart.yAxisLabel |> Expect.equal Nothing

        suppliedLineChartRows =
            \_ ->
                suppliedLineChart.rows
                    |> Expect.equal
                        [ [ "105", "106", "107", "108", "109", "110" ]
                        , [ "205", "206", "207", "208", "209", "210" ]
                        ]

        suppliedLineSeriesLabels =
            \_ ->
                suppliedLineChart.seriesLabels
                    |> Expect.equal
                        (Just
                            [ "Label 1", "Label 2", "Label 3", "Label 4" ]
                        )

        suppliedBarChartRows =
            \_ ->
                suppliedBarChart.rows
                    |> Expect.equal
                        [ [ "305", "306", "307", "308", "309", "310" ]
                        , [ "405", "406", "407", "408", "409", "410" ]
                        ]

        suppliedBarSeriesLabels =
            \_ ->
                suppliedBarChart.seriesLabels
                    |> Expect.equal
                        (Just
                            [ "Label 1", "Label 2", "Label 3", "Label 4" ]
                        )

        suppliedLineMinValue =
            \_ -> suppliedLineChart.minValue |> Expect.equal 105

        suppliedLineMaxValue =
            \_ -> suppliedLineChart.maxValue |> Expect.equal 210

        suppliedBarMinValue =
            \_ -> suppliedBarChart.minValue |> Expect.equal 305

        suppliedBarMaxValue =
            \_ -> suppliedBarChart.maxValue |> Expect.equal 410

        suppliedXLabels =
            \_ ->
                suppliedLineChart.xLabels
                    |> Expect.equal
                        (Just
                            [ "205", "206", "207", "208", "209", "210" ]
                        )

        suppliedXAxisLabel =
            \_ -> suppliedLineChart.xAxisLabel |> Expect.equal (Just "Label 1")

        suppliedLineYAxisLabel =
            \_ -> suppliedLineChart.yAxisLabel |> Expect.equal (Just "Label 2")

        suppliedBarYAxisLabel =
            \_ -> suppliedBarChart.yAxisLabel |> Expect.equal (Just "Label 3")
    in
        Test.describe "LineAndBarAdapter.adapt"
            [ Test.describe "with default Config"
                [ Test.test "line chart rows are all body rows" defaultLineChartRows
                , Test.test "line chart series labels are empty" defaultLineSeriesLabels
                , Test.test "bar chart series labels are empty" defaultBarSeriesLabels
                , Test.test "bar chart rows are sll body rows" defaultBarChartRows
                , Test.test "min value is extracted from line rows" defaultLineMinValue
                , Test.test "max value is extracted from line rows" defaultLineMaxValue
                , Test.test "min value is extracted from bar rows" defaultBarMinValue
                , Test.test "max value is extracted from bar rows" defaultBarMaxValue
                , Test.test "x-axis labels are the first row" defaultXLabels
                , Test.test "x-axis label is nothing" defaultXAxisLabel
                , Test.test "line chart y-axis label is nothing" defaultLineYAxisLabel
                , Test.test "bar chart y-axis label is nothing" defaultBarYAxisLabel
                ]
            , Test.describe "with supplied Config"
                [ Test.test "line chart rows are as specified" suppliedLineChartRows
                , Test.test "line chart series labels are as specified" suppliedLineSeriesLabels
                , Test.test "bar chart series labels are as specified" suppliedBarSeriesLabels
                , Test.test "bar chart rows are as specified" suppliedBarChartRows
                , Test.test "min value is extracted from line rows" suppliedLineMinValue
                , Test.test "max value is extracted from line rows" suppliedLineMaxValue
                , Test.test "min value is extracted from bar rows" suppliedBarMinValue
                , Test.test "max value is extracted from bar rows" suppliedBarMaxValue
                , Test.test "x-axis labels are the third row" suppliedXLabels
                , Test.test "x-axis label is as specified" suppliedXAxisLabel
                , Test.test "line chart y-axis label is as specified" suppliedLineYAxisLabel
                , Test.test "bar chart y-axis label is as specified" suppliedBarYAxisLabel
                ]
            ]
