module Data.Widget.Adapters.CellRangeTest exposing (..)

import Data.Widget.Adapters.AdapterTestData as TD
import Data.Widget.Adapters.CellPosition as CellPosition exposing (CellPosition, asJsonValue, decoder)
import Data.Widget.Adapters.CellRange as CellRange exposing (asJsonValue)
import Data.Widget.Adapters.LineAndBarAdapter as LineAndBarAdapter exposing (adapt)
import Data.Widget.Table as Table exposing (Data)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Encode as Encode exposing (..)
import List.Extra
import Test exposing (..)


cellRangeTest : Test
cellRangeTest =
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

        -- expectations
        originBasedRange10x2 =
            \_ ->
                CellRange.extractRows input [ ( 1, 1 ), ( 10, 2 ) ]
                    |> Expect.equal
                        [ [ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct" ]
                        , [ "101", "102", "103", "104", "105", "106", "107", "108", "109", "110" ]
                        ]

        originBasedRange6x5 =
            \_ ->
                CellRange.extractRows input [ ( 1, 1 ), ( 6, 5 ) ]
                    |> Expect.equal
                        [ [ "Jan", "Feb", "Mar", "Apr", "May", "Jun" ]
                        , [ "101", "102", "103", "104", "105", "106" ]
                        , [ "201", "202", "203", "204", "205", "206" ]
                        , [ "301", "302", "303", "304", "305", "306" ]
                        , [ "401", "402", "403", "404", "405", "406" ]
                        ]

        offsetBasedRange9x2 =
            \_ ->
                CellRange.extractRows input [ ( 2, 2 ), ( 10, 3 ) ]
                    |> Expect.equal
                        [ [ "102", "103", "104", "105", "106", "107", "108", "109", "110" ]
                        , [ "202", "203", "204", "205", "206", "207", "208", "209", "210" ]
                        ]

        offsetBasedRange4x3 =
            \_ ->
                CellRange.extractRows input [ ( 5, 3 ), ( 8, 5 ) ]
                    |> Expect.equal
                        [ [ "205", "206", "207", "208" ]
                        , [ "305", "306", "307", "308" ]
                        , [ "405", "406", "407", "408" ]
                        ]

        offsetBasedRange2x2 =
            \_ ->
                CellRange.extractRows input [ ( 10, 4 ), ( 11, 5 ) ]
                    |> Expect.equal
                        [ [ "310", "311" ]
                        , [ "410", "411" ]
                        ]

        offsetBasedRange1x1 =
            \_ ->
                CellRange.extractRows input [ ( 6, 4 ), ( 6, 4 ) ]
                    |> Expect.equal [ [ "306" ] ]

        offsetBasedRange3x1 =
            \_ ->
                CellRange.extractRows input [ ( 10, 3 ), ( 12, 3 ) ]
                    |> Expect.equal [ [ "210", "211", "212" ] ]

        offsetBasedRange1x3 =
            \_ ->
                CellRange.extractRows input [ ( 9, 3 ), ( 9, 5 ) ]
                    |> Expect.equal
                        [ [ "209" ]
                        , [ "309" ]
                        , [ "409" ]
                        ]
    in
        Test.describe "CellRangeTest"
            [ Test.describe "extractRows"
                [ Test.describe "from origin (1, 1)"
                    [ Test.test "10x2 starting at origin" originBasedRange10x2
                    , Test.test "6x5  starting at origin" originBasedRange6x5
                    ]
                ]
            , Test.describe "from offset"
                [ Test.test "9x2 starting at (2, 2)" offsetBasedRange9x2
                , Test.test "4x3 starting at (5, 3)" offsetBasedRange4x3
                , Test.test "2x2 starting at (10,4)" offsetBasedRange2x2
                , Test.test "1x1 starting at (6, 4)" offsetBasedRange1x1
                , Test.test "3x1 starting at (10,3)" offsetBasedRange3x1
                , Test.test "1x3 starting at (9, 3)" offsetBasedRange1x3
                ]
            ]
