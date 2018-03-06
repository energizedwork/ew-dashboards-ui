module Data.Widget.Adapters.TableAdapterTest exposing (..)

import Data.Widget.Adapters.AdapterTestData as TD
import Data.Widget.Adapters.CellPosition as CellPosition exposing (CellPosition(..), decoder, encode)
import Data.Widget.Adapters.CellRange as CellRange exposing (CellRange, encode)
import Data.Widget.Config as TableConfig
import Data.Widget.Adapters.TableAdapter as TableAdapter exposing (adapt, Orientation(..))
import Data.Widget.Table as Table exposing (Data)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Encode as Encode exposing (..)
import List.Extra
import Test exposing (..)


verticalAdapterConfigTest : Test
verticalAdapterConfigTest =
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

        defaultConfig =
            TableConfig.default

        suppliedConfig =
            Dict.fromList
                [ ( "bodyRows"
                  , CellRange.encode <|
                        CellRange
                            (CellPosition ( 5, 2 ))
                            (CellPosition ( 10, 3 ))
                  )
                , ( "xLabels"
                  , CellRange.encode <|
                        CellRange
                            (CellPosition ( 5, 3 ))
                            (CellPosition ( 10, 3 ))
                  )
                , ( "yLabels"
                  , CellRange.encode <|
                        CellRange
                            (CellPosition ( 5, 4 ))
                            (CellPosition ( 10, 4 ))
                  )
                ]

        -- functions under test!
        defaultActual =
            TableAdapter.adapt defaultConfig input Vertical

        suppliedActual =
            TableAdapter.adapt suppliedConfig input Vertical

        -- expectations
        defaultHeaders =
            \_ ->
                defaultActual.xLabels
                    |> Expect.equal
                        (Just TD.headerRow)

        defaultBodyRows =
            \_ ->
                defaultActual.rows
                    |> Expect.equal
                        [ TD.firstRow
                        , TD.secondRow
                        , TD.thirdRow
                        , TD.forthRow
                        ]

        defaultMinValue =
            \_ -> defaultActual.minValue |> Expect.equal 101

        defaultMaxValue =
            \_ -> defaultActual.maxValue |> Expect.equal 412

        defaultXLabels =
            \_ -> defaultActual.xLabels |> Expect.equal (Just TD.headerRow)

        defaultYLabels =
            \_ -> defaultActual.yLabels |> Expect.equal Nothing

        suppliedXLabels =
            \_ ->
                suppliedActual.xLabels
                    |> Expect.equal
                        (Just
                            [ "205", "206", "207", "208", "209", "210" ]
                        )

        suppliedYLabels =
            \_ ->
                suppliedActual.yLabels
                    |> Expect.equal
                        (Just
                            [ "305", "306", "307", "308", "309", "310" ]
                        )

        suppliedHeaders =
            suppliedXLabels

        suppliedLineChartRows =
            \_ ->
                suppliedActual.rows
                    |> Expect.equal
                        [ [ "105", "106", "107", "108", "109", "110" ]
                        , [ "205", "206", "207", "208", "209", "210" ]
                        ]

        suppliedMinValue =
            \_ -> suppliedActual.minValue |> Expect.equal 105

        suppliedMaxValue =
            \_ -> suppliedActual.maxValue |> Expect.equal 210
    in
        Test.describe "TableAdapter.adapt (Vertical)"
            [ Test.describe "with default Config"
                [ Test.test "headers are first row" defaultHeaders
                , Test.test "body rows are remain rows, excluding the header row" defaultBodyRows
                , Test.test "min value is extracted from body rows" defaultMinValue
                , Test.test "max value is extracted from body rows" defaultMaxValue
                , Test.test "x-axis labels are the first row" defaultXLabels
                , Test.test "y-axis labels are empty" defaultYLabels
                ]
            , Test.describe "with supplied Config"
                [ Test.test "headers are third row" suppliedHeaders
                , Test.test "body rows are as specified" suppliedLineChartRows
                , Test.test "min value is extracted from body rows" suppliedMinValue
                , Test.test "max value is extracted from body rows" suppliedMaxValue
                , Test.test "x-axis labels are the second row" suppliedXLabels
                , Test.test "y-axis labels are the third row" suppliedYLabels
                ]
            ]


horizontalAdapterConfigTest : Test
horizontalAdapterConfigTest =
    let
        -- setup data
        input =
            Data
                [ [ "Client A" ] ++ (Tuple.first <| List.Extra.splitAt 4 TD.firstRow)
                , [ "Client B" ] ++ (Tuple.first <| List.Extra.splitAt 4 TD.secondRow)
                , [ "Client C" ] ++ (Tuple.first <| List.Extra.splitAt 4 TD.thirdRow)
                , [ "Client D" ] ++ (Tuple.first <| List.Extra.splitAt 4 TD.forthRow)
                ]

        defaultConfig =
            TableConfig.default

        suppliedConfig =
            Dict.fromList
                [ ( "bodyRows"
                  , CellRange.encode <|
                        CellRange
                            (CellPosition ( 3, 1 ))
                            (CellPosition ( 5, 4 ))
                  )
                , ( "xLabels"
                  , CellRange.encode <|
                        CellRange
                            (CellPosition ( 4, 1 ))
                            (CellPosition ( 4, 4 ))
                  )
                , ( "yLabels"
                  , CellRange.encode <|
                        CellRange
                            (CellPosition ( 5, 1 ))
                            (CellPosition ( 5, 5 ))
                  )
                ]

        -- functions under test!
        defaultActual =
            TableAdapter.adapt defaultConfig input Horizontal

        suppliedActual =
            TableAdapter.adapt suppliedConfig input Horizontal

        -- expectations
        defaultHeaders =
            \_ ->
                defaultActual.yLabels
                    |> Expect.equal
                        (Just
                            [ "Client A"
                            , "Client B"
                            , "Client C"
                            , "Client D"
                            ]
                        )

        defaultBodyRows =
            \_ ->
                defaultActual.rows
                    |> Expect.equal
                        [ [ "101"
                          , "201"
                          , "301"
                          , "401"
                          ]
                        , [ "102"
                          , "202"
                          , "302"
                          , "402"
                          ]
                        , [ "103"
                          , "203"
                          , "303"
                          , "403"
                          ]
                        , [ "104"
                          , "204"
                          , "304"
                          , "404"
                          ]
                        ]

        defaultMinValue =
            \_ -> defaultActual.minValue |> Expect.equal 101

        defaultMaxValue =
            \_ -> defaultActual.maxValue |> Expect.equal 404

        defaultXLabels =
            \_ ->
                defaultActual.xLabels
                    |> Expect.equal
                        Nothing

        defaultYLabels =
            \_ ->
                defaultActual.yLabels
                    |> Expect.equal
                        (Just
                            [ "Client A"
                            , "Client B"
                            , "Client C"
                            , "Client D"
                            ]
                        )

        suppliedHeaders =
            defaultYLabels

        suppliedXLabels =
            \_ ->
                suppliedActual.xLabels
                    |> Expect.equal
                        (Just
                            [ "103"
                            , "203"
                            , "303"
                            , "403"
                            ]
                        )

        suppliedYLabels =
            \_ ->
                suppliedActual.yLabels
                    |> Expect.equal
                        (Just
                            [ "104"
                            , "204"
                            , "304"
                            , "404"
                            ]
                        )

        suppliedLineChartRows =
            \_ ->
                suppliedActual.rows
                    |> Expect.equal
                        [ [ "102"
                          , "202"
                          , "302"
                          , "402"
                          ]
                        , [ "103"
                          , "203"
                          , "303"
                          , "403"
                          ]
                        , [ "104"
                          , "204"
                          , "304"
                          , "404"
                          ]
                        ]

        suppliedMinValue =
            \_ -> suppliedActual.minValue |> Expect.equal 102

        suppliedMaxValue =
            \_ -> suppliedActual.maxValue |> Expect.equal 404
    in
        Test.describe "TableAdapter.adapt (Horizontal)"
            [ Test.describe "with default Config"
                [ Test.test "headers are first column" defaultHeaders
                , Test.test "body is remaining cols, excluding the first cell per row" defaultBodyRows
                , Test.test "min value is extracted from body" defaultMinValue
                , Test.test "max value is extracted from body" defaultMaxValue
                , Test.test "x-axis labels are empty" defaultXLabels
                , Test.test "y-axis labels are the first col" defaultYLabels
                ]
            , Test.describe "with supplied Config"
                [ Test.test "headers are fith column row" suppliedHeaders
                , Test.test "body rows are as specified" suppliedLineChartRows
                , Test.test "min value is extracted from body rows" suppliedMinValue
                , Test.test "max value is extracted from body rows" suppliedMaxValue
                , Test.test "x-axis labels are the forth col" suppliedXLabels
                , Test.test "y-axis labels are the fifth col" suppliedYLabels
                ]
            ]
