module Data.Widget.Adapters.LineAndBarAdapterTest exposing (..)

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

        defaultConfig =
            LineAndBarAdapter.defaultConfig

        suppliedConfig =
            Dict.fromList
                [ ( "lineRows", CellRange.asJsonValue [ ( 5, 1 ), ( 10, 3 ) ] )
                , ( "barRows", CellRange.asJsonValue [ ( 5, 4 ), ( 10, 5 ) ] )
                , ( "xLabelsIndex", Encode.int 0 )
                , ( "yLabelsIndicies", List.map (\y -> Encode.int y) [ 0 ] |> Encode.list )
                ]

        -- functions under test!
        ( defaultActualHeaderRow, defaultActualLineRows, defaultActualBarRows, defaultActualMaxValue, defaultActualXLabels, defaultActualYLabels ) =
            LineAndBarAdapter.adapt defaultConfig input

        ( suppliedActualHeaderRow, suppliedActualLineRows, suppliedActualBarRows, suppliedActualMaxValue, suppliedActualXLabels, suppliedActualYLabels ) =
            LineAndBarAdapter.adapt suppliedConfig input

        defaultHeaders =
            \_ -> defaultActualHeaderRow |> Expect.equal TD.headerRow

        -- expectations
        defaultLineChartRows =
            \_ ->
                defaultActualLineRows |> Expect.equal [ TD.firstRow, TD.secondRow ]

        defaultBarChartRows =
            \_ -> defaultActualBarRows |> Expect.equal [ TD.thirdRow, TD.forthRow ]

        defaultMaxValue =
            \_ -> defaultActualMaxValue |> Expect.equal 412

        defaultXLabels =
            \_ -> defaultActualXLabels |> Expect.equal TD.headerRow

        suppliedLineChartRows =
            \_ ->
                suppliedActualLineRows
                    |> Expect.equal
                        [ [ "May", "Jun", "Jul", "Aug", "Sep", "Oct" ]
                        , [ "105", "106", "107", "108", "109", "110" ]
                        , [ "205", "206", "207", "208", "209", "210" ]
                        ]

        suppliedBarChartRows =
            \_ ->
                suppliedActualBarRows
                    |> Expect.equal
                        [ [ "305", "306", "307", "308", "309", "310" ]
                        , [ "405", "406", "407", "408", "409", "410" ]
                        ]
    in
        Test.describe "HeatMapAdapter.adapt"
            [ Test.describe "with default Config"
                [ Test.test "headers are first row" defaultHeaders
                , Test.test "line chart rows are first half of body rows" defaultLineChartRows
                , Test.test "bar chart rows are second half of body rows" defaultBarChartRows
                , Test.test "max value is extracted from body rows" defaultMaxValue
                , Test.test "x-axis labels are the first row" defaultXLabels
                , Test.todo "first y-axis labels are the range of values from the first row of the line chart data"
                , Test.todo "second y-axis labels are the range of values from the second row of the line chart data"
                ]
            , Test.describe "with supplied Config"
                [ Test.test "line chart rows are correct" suppliedLineChartRows
                , Test.test "bar chart rows are correct" suppliedBarChartRows
                ]
            ]
