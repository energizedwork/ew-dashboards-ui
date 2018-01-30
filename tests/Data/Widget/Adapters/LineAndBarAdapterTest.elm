module Data.Widget.Adapters.LineAndBarAdapterTest exposing (..)

import Data.Widget.Adapters.AdapterTestData as TD
import Data.Widget.Adapters.CellPosition as CellPosition exposing (CellPosition(..), encode, decoder)
import Data.Widget.Adapters.CellRange as CellRange exposing (CellRange, encode)
import Data.Widget.Adapters.LineAndBarAdapter as LineAndBarAdapter exposing (adapt)
import Data.Widget.Table as Table exposing (Data)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
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

        suppliedConfig =
            Dict.fromList
                [ ( "lineRows"
                  , CellRange.encode <|
                        CellRange
                            (CellPosition ( 5, 2 ))
                            (CellPosition ( 10, 3 ))
                  )
                , ( "lineSeriesLabels"
                  , CellRange.encode <|
                        CellRange
                            (CellPosition ( 13, 2 ))
                            (CellPosition ( 13, 5 ))
                  )
                , ( "barRows"
                  , CellRange.encode <|
                        CellRange
                            (CellPosition ( 5, 4 ))
                            (CellPosition ( 10, 5 ))
                  )
                , ( "xLabels"
                  , CellRange.encode <|
                        CellRange
                            (CellPosition ( 5, 3 ))
                            (CellPosition ( 10, 3 ))
                  )
                ]

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

        defaultLineMinValue =
            \_ -> defaultLineChart.minValue |> Expect.equal 101

        defaultLineMaxValue =
            \_ -> defaultLineChart.maxValue |> Expect.equal 412

        defaultBarMinValue =
            \_ -> defaultBarChart.minValue |> Expect.equal 101

        defaultBarMaxValue =
            \_ -> defaultBarChart.maxValue |> Expect.equal 412

        defaultXLabels =
            \_ -> defaultLineChart.xLabels |> Expect.equal TD.headerRow

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
                        [ "205", "206", "207", "208", "209", "210" ]
    in
        Test.describe "LineAndBarAdapter.adapt"
            [ Test.describe "with default Config"
                [ Test.test "line chart rows are all body rows" defaultLineChartRows
                , Test.test "line chart series labels are empty" defaultLineSeriesLabels
                , Test.test "bar chart rows are sll body rows" defaultBarChartRows
                , Test.test "min value is extracted from line rows" defaultLineMinValue
                , Test.test "max value is extracted from line rows" defaultLineMaxValue
                , Test.test "min value is extracted from bar rows" defaultBarMinValue
                , Test.test "max value is extracted from bar rows" defaultBarMaxValue
                , Test.test "x-axis labels are the first row" defaultXLabels
                ]
            , Test.describe "with supplied Config"
                [ Test.test "line chart rows are as specified" suppliedLineChartRows
                , Test.test "line chart series labels are as specified" suppliedLineSeriesLabels
                , Test.test "bar chart rows are as specified" suppliedBarChartRows
                , Test.test "min value is extracted from line rows" suppliedLineMinValue
                , Test.test "max value is extracted from line rows" suppliedLineMaxValue
                , Test.test "min value is extracted from bar rows" suppliedBarMinValue
                , Test.test "max value is extracted from bar rows" suppliedBarMaxValue
                , Test.test "x-axis labels are the third row" suppliedXLabels
                ]
            ]
