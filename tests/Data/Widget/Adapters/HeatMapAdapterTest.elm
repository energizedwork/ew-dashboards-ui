module Data.Widget.Adapters.HeatMapAdapterTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Data.Widget.Adapters.HeatMapAdapter as HeatMapAdapter exposing (adapt)
import Data.Widget.Table as Table exposing (Data)


adapterTest : Test
adapterTest =
    let
        headerRow =
            [ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" ]

        firstRow =
            List.range 101 112 |> List.map toString

        secondRow =
            List.range 201 212 |> List.map toString

        thirdRow =
            List.range 301 312 |> List.map toString

        input =
            Data
                [ "--" :: headerRow
                , "00:00" :: firstRow
                , "00:30" :: secondRow
                , "01:00" :: thirdRow
                ]

        ( actualHeaderRow, actualBodyRows, actualMaxValue, actualXLabels, actualYLabels ) =
            HeatMapAdapter.adapt input

        -- where A0 is the row/col index
        headersExcludeA0 =
            \_ -> Expect.equal actualHeaderRow headerRow

        -- where AN is the row/col index
        bodyRowsExcludeAN =
            \_ -> Expect.equal actualBodyRows [ firstRow, secondRow, thirdRow ]

        maxValue =
            \_ -> Expect.equal actualMaxValue 312

        xLabels =
            \_ -> Expect.equal actualXLabels headerRow

        yLabels =
            \_ -> Expect.equal actualYLabels [ "00:00", "00:30", "01:00" ]
    in
        Test.describe "HeatMapAdapter.adapt"
            [ Test.test "header rows drops '--' " headersExcludeA0
            , Test.test "body rows drop time cell" bodyRowsExcludeAN
            , Test.test "max value extracted: " maxValue
            , Test.test "x-axis labels: " xLabels
            , Test.test "y-axis labels: " yLabels
            ]
