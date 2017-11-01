module Data.Widget.Adapters.LineAndBarAdapterTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Data.Widget.Adapters.AdapterTestData as TD
import Data.Widget.Adapters.LineAndBarAdapter as LineAndBarAdapter exposing (adapt)
import Data.Widget.Table as Table exposing (Data)


adapterConfigTest : Test
adapterConfigTest =
    let
        -- setup data
        input =
            Data
                [ TD.headerRow
                , TD.firstRow
                , TD.secondRow
                , TD.thirdRow
                , TD.forthRow
                ]

        config =
            LineAndBarAdapter.defaultConfig

        -- function under test!
        ( actualHeaderRow, actualLineRows, actualBarRows, actualMaxValue, actualXLabels, actualYLabels ) =
            LineAndBarAdapter.adapt config input

        headers =
            \_ -> Expect.equal actualHeaderRow TD.headerRow

        -- expectations
        lineChartRows =
            \_ -> Expect.equal actualLineRows [ TD.firstRow, TD.secondRow ]

        barChartRows =
            \_ -> Expect.equal actualBarRows [ TD.thirdRow, TD.forthRow ]

        maxValue =
            \_ -> Expect.equal actualMaxValue 412

        xLabels =
            \_ -> Expect.equal actualXLabels TD.headerRow
    in
        Test.describe "HeatMapAdapter.adapt"
            [ Test.describe "with default Config"
                [ Test.test "headers are first row" headers
                , Test.test "line chart rows are first half of body rows" lineChartRows
                , Test.test "bar chart rows are second half of body rows" barChartRows
                , Test.test "max value is extracted from body rows" maxValue
                , Test.test "x-axis labels are the first row" xLabels
                , Test.todo "first y-axis labels are the range of values from the first row of the line chart data"
                , Test.todo "second y-axis labels are the range of values from the second row of the line chart data"
                ]
            , Test.todo "with supplied Config"
            ]
