module Data.Widget.Adapters.HeatMapAdapterTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Data.Widget.Adapters.AdapterTestData as TD
import Data.Widget.Adapters.HeatMapAdapter as HeatMapAdapter exposing (adapt)
import Data.Widget.Table as Table exposing (Data)


adapterTest : Test
adapterTest =
    let
        -- setup data
        input =
            Data
                [ "--" :: TD.headerRow
                , "00:00" :: TD.firstRow
                , "00:30" :: TD.secondRow
                , "01:00" :: TD.thirdRow
                ]

        -- function under test!
        ( actualHeaderRow, actualBodyRows, actualMaxValue, actualXLabels, actualYLabels ) =
            HeatMapAdapter.adapt input

        -- expectations
        -- where A0 is the row/col index
        headersExcludeA0 =
            \_ -> Expect.equal actualHeaderRow TD.headerRow

        -- where AN is the row/col index
        bodyRowsExcludeAN =
            \_ -> Expect.equal actualBodyRows [ TD.firstRow, TD.secondRow, TD.thirdRow ]

        maxValue =
            \_ -> Expect.equal actualMaxValue 312

        xLabels =
            \_ -> Expect.equal actualXLabels TD.headerRow

        yLabels =
            \_ -> Expect.equal actualYLabels [ "00:00", "00:30", "01:00" ]
    in
        Test.describe "HeatMapAdapter.adapt"
            [ Test.test "header rows drops '--'" headersExcludeA0
            , Test.test "body rows drop time cell" bodyRowsExcludeAN
            , Test.test "max value extracted" maxValue
            , Test.test "x-axis labels" xLabels
            , Test.test "y-axis labels" yLabels
            ]
