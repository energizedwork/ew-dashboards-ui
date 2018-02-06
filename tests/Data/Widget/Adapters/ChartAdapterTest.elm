module Data.Widget.Adapters.ChartAdapterTest exposing (..)

import Data.Widget.Adapters.AdapterTestData as TD
import Data.Widget.Adapters.CellPosition as CellPosition exposing (CellPosition(..), encode, decoder)
import Data.Widget.Adapters.CellRange as CellRange exposing (CellRange, encode)
import Data.Widget.Adapters.ChartAdapter as ChartAdapter exposing (adapt)
import Data.Widget.Table as Table exposing (Data)
import Dict exposing (Dict)
import Expect exposing (Expectation)
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
            ChartAdapter.defaultConfig

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
                , ( "seriesLabels"
                  , CellRange.encode <|
                        CellRange
                            (CellPosition ( 13, 2 ))
                            (CellPosition ( 13, 5 ))
                  )
                ]

        -- functions under test!
        ( defaultActualHeaderRow, defaultActualBodyRows, defaultActualMinValue, defaultActualMaxValue, defaultActualXLabels, defaultActualSeriesLabels ) =
            ChartAdapter.adapt defaultConfig unlabelledInput

        ( suppliedActualHeaderRow, suppliedActualBodyRows, suppliedActualMinValue, suppliedActualMaxValue, suppliedActualXLabels, suppliedActualSeriesLabels ) =
            ChartAdapter.adapt suppliedConfig labelledInput

        -- expectations
        defaultHeaders =
            \_ -> defaultActualHeaderRow |> Expect.equal TD.headerRow

        defaultBodyRows =
            \_ ->
                defaultActualBodyRows
                    |> Expect.equal
                        [ TD.firstRow
                        , TD.secondRow
                        , TD.thirdRow
                        , TD.forthRow
                        ]

        defaultMinValue =
            \_ -> defaultActualMinValue |> Expect.equal 101

        defaultMaxValue =
            \_ -> defaultActualMaxValue |> Expect.equal 412

        defaultXLabels =
            \_ -> defaultActualXLabels |> Expect.equal TD.headerRow

        defaultSeriesLabels =
            \_ ->
                defaultActualSeriesLabels
                    |> Expect.equal
                        Nothing

        suppliedHeaders =
            \_ ->
                suppliedActualHeaderRow
                    |> Expect.equal
                        [ "205", "206", "207", "208", "209", "210" ]

        suppliedLineChartRows =
            \_ ->
                suppliedActualBodyRows
                    |> Expect.equal
                        [ [ "105", "106", "107", "108", "109", "110" ]
                        , [ "205", "206", "207", "208", "209", "210" ]
                        ]

        suppliedMinValue =
            \_ -> suppliedActualMinValue |> Expect.equal 105

        suppliedMaxValue =
            \_ -> suppliedActualMaxValue |> Expect.equal 210

        suppliedXLabels =
            suppliedHeaders

        suppliedSeriesLabels =
            \_ ->
                suppliedActualSeriesLabels
                    |> Expect.equal
                        (Just
                            [ "Label 1", "Label 2", "Label 3", "Label 4" ]
                        )
    in
        Test.describe "ChartAdapter.adapt"
            [ Test.describe "with default Config"
                [ Test.test "headers are first row" defaultHeaders
                , Test.test "body rows are remain rows, excluding the header row" defaultBodyRows
                , Test.test "min value is extracted from body rows" defaultMinValue
                , Test.test "max value is extracted from body rows" defaultMaxValue
                , Test.test "x-axis labels are the first row" defaultXLabels
                , Test.test "series labels are empty" defaultSeriesLabels
                ]
            , Test.describe "with supplied Config"
                [ Test.test "headers are third row" suppliedHeaders
                , Test.test "body rows are as specified" suppliedLineChartRows
                , Test.test "min value is extracted from body rows" suppliedMinValue
                , Test.test "max value is extracted from body rows" suppliedMaxValue
                , Test.test "x-axis labels are as specified" suppliedXLabels
                , Test.test "series labels are as specified" suppliedSeriesLabels
                ]
            ]
