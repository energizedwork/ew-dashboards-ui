module Data.Widget.Adapters.CellRangeTest exposing (..)

import Data.Widget.Adapters.AdapterTestData as TD
import Data.Widget.Adapters.CellPosition as CellPosition exposing (CellPosition(..), decoder, encode)
import Data.Widget.Adapters.CellRange as CellRange exposing (..)
import Data.Widget.Table as Table exposing (Data)
import Expect exposing (Expectation)
import Json.Decode as Decode exposing (..)
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
                CellRange.extractRows input
                    (CellRange
                        (CellPosition ( 1, 1 ))
                        (CellPosition ( 10, 2 ))
                    )
                    |> Expect.equal
                        [ [ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct" ]
                        , [ "101", "102", "103", "104", "105", "106", "107", "108", "109", "110" ]
                        ]

        originBasedRange6x5 =
            \_ ->
                CellRange.extractRows input
                    (CellRange
                        (CellPosition ( 1, 1 ))
                        (CellPosition ( 6, 5 ))
                    )
                    |> Expect.equal
                        [ [ "Jan", "Feb", "Mar", "Apr", "May", "Jun" ]
                        , [ "101", "102", "103", "104", "105", "106" ]
                        , [ "201", "202", "203", "204", "205", "206" ]
                        , [ "301", "302", "303", "304", "305", "306" ]
                        , [ "401", "402", "403", "404", "405", "406" ]
                        ]

        offsetBasedRange9x2 =
            \_ ->
                CellRange.extractRows input
                    (CellRange
                        (CellPosition ( 2, 2 ))
                        (CellPosition ( 10, 3 ))
                    )
                    |> Expect.equal
                        [ [ "102", "103", "104", "105", "106", "107", "108", "109", "110" ]
                        , [ "202", "203", "204", "205", "206", "207", "208", "209", "210" ]
                        ]

        offsetBasedRange4x3 =
            \_ ->
                CellRange.extractRows input
                    (CellRange
                        (CellPosition ( 5, 3 ))
                        (CellPosition ( 8, 5 ))
                    )
                    |> Expect.equal
                        [ [ "205", "206", "207", "208" ]
                        , [ "305", "306", "307", "308" ]
                        , [ "405", "406", "407", "408" ]
                        ]

        offsetBasedRange2x2 =
            \_ ->
                CellRange.extractRows input
                    (CellRange
                        (CellPosition ( 10, 4 ))
                        (CellPosition ( 11, 5 ))
                    )
                    |> Expect.equal
                        [ [ "310", "311" ]
                        , [ "410", "411" ]
                        ]

        offsetBasedRange1x1 =
            \_ ->
                CellRange.extractRows input
                    (CellRange
                        (CellPosition ( 6, 4 ))
                        (CellPosition ( 6, 4 ))
                    )
                    |> Expect.equal [ [ "306" ] ]

        offsetBasedRange3x1 =
            \_ ->
                CellRange.extractRows input
                    (CellRange
                        (CellPosition ( 10, 3 ))
                        (CellPosition ( 12, 3 ))
                    )
                    |> Expect.equal [ [ "210", "211", "212" ] ]

        offsetBasedRange1x3 =
            \_ ->
                CellRange.extractRows input
                    (CellRange
                        (CellPosition ( 9, 3 ))
                        (CellPosition ( 9, 5 ))
                    )
                    |> Expect.equal
                        [ [ "209" ]
                        , [ "309" ]
                        , [ "409" ]
                        ]

        cellValueAt1_1 =
            \_ ->
                CellRange.extractCell input
                    (CellPosition ( 1, 1 ))
                    |> Expect.equal
                        "Jan"

        cellValueAt2_1 =
            \_ ->
                CellRange.extractCell input
                    (CellPosition ( 2, 1 ))
                    |> Expect.equal
                        "Feb"
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
            , Test.describe "extractCell"
                [ Test.test "at (1,1)" cellValueAt1_1
                , Test.test "at (2, 1)" cellValueAt2_1
                ]
            ]


decoderTest : Test
decoderTest =
    let
        input =
            """
            {
                "start": [2, 2],
                "end": [20, 4]
            }
            """

        runDecode =
            \_ ->
                Decode.decodeString (CellRange.decoder) input
                    |> Expect.equal
                        (Ok <|
                            CellRange
                                (CellPosition ( 2, 2 ))
                                (CellPosition ( 20, 4 ))
                        )
    in
        Test.test "CellRange.decoder" runDecode


encoderTest : Test
encoderTest =
    let
        input =
            CellRange
                (CellPosition ( 3, 3 ))
                (CellPosition ( 10, 4 ))

        runEncode =
            \_ ->
                (toString <| CellRange.encode input)
                    |> Expect.equal "{ start = { 0 = 3, 1 = 3 }, end = { 0 = 10, 1 = 4 } }"
    in
        Test.test "CellRange.encode" runEncode


defaultRangesTest : Test
defaultRangesTest =
    let
        input =
            Data
                [ TD.headerRow
                , TD.firstRow
                , TD.secondRow
                , TD.thirdRow
                , TD.forthRow
                ]

        runFirstRowRange =
            \_ ->
                CellRange.firstRowRange input
                    |> Expect.equal
                        (CellRange
                            (CellPosition ( 1, 1 ))
                            (CellPosition ( 12, 1 ))
                        )

        runRemainingRowsRange =
            \_ ->
                CellRange.remainingRowsRange input
                    |> Expect.equal
                        (CellRange
                            (CellPosition ( 1, 2 ))
                            (CellPosition ( 12, 5 ))
                        )
    in
        Test.describe "CellRangeTest default ranges"
            [ Test.describe "CellRange.firstRowRange"
                [ Test.test "returns the range of the first row" runFirstRowRange
                ]
            , Test.describe "CellRange.remainingRowsRange"
                [ Test.test "returns the rows excluding the first" runRemainingRowsRange
                ]
            ]
