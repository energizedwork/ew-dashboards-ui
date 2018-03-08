module Data.Widget.Adapters.ChartAdapterTest exposing (..)

import Data.Widget.Adapters.AdapterTestData as TD
import Data.Widget.Adapters.CellPosition as CellPosition exposing (CellPosition(..), encode, decoder)
import Data.Widget.Adapters.CellRange as CellRange exposing (CellRange, encode)
import Data.Widget.Adapters.ChartAdapter as ChartAdapter exposing (adapt)
import Data.Widget.Adapters.TableAdapter as TableAdapter exposing (adapt, Orientation(..))
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
                [ TD.headerRow ++ [ "X Axis Label" ] ++ [ "Forecast position", "9" ]
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
                , ( "xAxisLabel"
                  , CellPosition.encode <|
                        CellPosition ( 13, 1 )
                  )
                , ( "yAxisLabel"
                  , CellPosition.encode <|
                        CellPosition ( 13, 2 )
                  )
                , ( "seriesLabels"
                  , CellRange.encode <|
                        CellRange
                            (CellPosition ( 13, 2 ))
                            (CellPosition ( 13, 5 ))
                  )
                , ( "forecastPosition"
                  , CellPosition.encode <|
                        CellPosition ( 15, 1 )
                  )
                ]

        -- functions under test!
        defaultActualChartData =
            ChartAdapter.adapt defaultConfig unlabelledInput Vertical

        suppliedActualChartData =
            ChartAdapter.adapt suppliedConfig labelledInput Vertical

        -- expectations
        defaultBodyRows =
            \_ ->
                defaultActualChartData.rows
                    |> Expect.equal
                        [ TD.firstRow
                        , TD.secondRow
                        , TD.thirdRow
                        , TD.forthRow
                        ]

        defaultMinValue =
            \_ -> defaultActualChartData.minValue |> Expect.equal 101

        defaultMaxValue =
            \_ -> defaultActualChartData.maxValue |> Expect.equal 412

        defaultXLabels =
            \_ -> defaultActualChartData.xLabels |> Expect.equal (Just TD.headerRow)

        defaultSeriesLabels =
            \_ ->
                defaultActualChartData.seriesLabels
                    |> Expect.equal
                        Nothing

        defaultXAxisLabel =
            \_ ->
                defaultActualChartData.xAxisLabel
                    |> Expect.equal
                        Nothing

        defaultYAxisLabel =
            \_ ->
                defaultActualChartData.yAxisLabel
                    |> Expect.equal
                        Nothing

        defaultForecastPosition =
            \_ ->
                defaultActualChartData.forecastPosition
                    |> Expect.equal
                        Nothing

        suppliedLineChartRows =
            \_ ->
                suppliedActualChartData.rows
                    |> Expect.equal
                        [ [ "105", "106", "107", "108", "109", "110" ]
                        , [ "205", "206", "207", "208", "209", "210" ]
                        ]

        suppliedMinValue =
            \_ -> suppliedActualChartData.minValue |> Expect.equal 105

        suppliedMaxValue =
            \_ -> suppliedActualChartData.maxValue |> Expect.equal 210

        suppliedXLabels =
            \_ -> suppliedActualChartData.xLabels |> Expect.equal (Just [ "205", "206", "207", "208", "209", "210" ])

        suppliedSeriesLabels =
            \_ ->
                suppliedActualChartData.seriesLabels
                    |> Expect.equal
                        (Just
                            [ "Label 1", "Label 2", "Label 3", "Label 4" ]
                        )

        suppliedXAxisLabel =
            \_ ->
                suppliedActualChartData.xAxisLabel
                    |> Expect.equal
                        (Just "X Axis Label")

        suppliedYAxisLabel =
            \_ ->
                suppliedActualChartData.yAxisLabel
                    |> Expect.equal
                        (Just "Label 1")

        suppliedForecastPosition =
            \_ ->
                suppliedActualChartData.forecastPosition
                    |> Expect.equal
                        (Just 9)
    in
        Test.describe "ChartAdapter.adapt"
            [ Test.describe "with default Config"
                [ Test.test "body rows are remain rows, excluding the header row" defaultBodyRows
                , Test.test "min value is extracted from body rows" defaultMinValue
                , Test.test "max value is extracted from body rows" defaultMaxValue
                , Test.test "x-axis labels are the first row" defaultXLabels
                , Test.test "series labels are empty" defaultSeriesLabels
                , Test.test "x-axis label is nothing" defaultXAxisLabel
                , Test.test "y-axis label is nothing" defaultYAxisLabel
                , Test.test "no forecasts included" defaultForecastPosition
                ]
            , Test.describe "with supplied Config"
                [ Test.test "body rows are as specified" suppliedLineChartRows
                , Test.test "min value is extracted from body rows" suppliedMinValue
                , Test.test "max value is extracted from body rows" suppliedMaxValue
                , Test.test "x-axis labels are as specified" suppliedXLabels
                , Test.test "series labels are as specified" suppliedSeriesLabels
                , Test.test "x-axis label is as specified" suppliedXAxisLabel
                , Test.test "y-axis label is as specified" suppliedYAxisLabel
                , Test.test "forecast position is as specified" suppliedForecastPosition
                ]
            ]
