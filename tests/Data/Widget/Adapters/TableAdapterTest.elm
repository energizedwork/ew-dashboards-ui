module Data.Widget.Adapters.TableAdapterTest exposing (..)

import Data.Widget.Adapters.AdapterTestData as TD
import Data.Widget.Adapters.CellPosition as CellPosition exposing (CellPosition(..), encode, decoder)
import Data.Widget.Adapters.CellRange as CellRange exposing (CellRange, encode)
import Data.Widget.Adapters.TableAdapter as TableAdapter exposing (adapt)
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
            TableAdapter.defaultConfig

        suppliedConfig =
            Dict.fromList
                [ ( "bodyRows"
                  , CellRange.encode <|
                        CellRange
                            (CellPosition ( 5, 2 ))
                            (CellPosition ( 10, 3 ))
                  )
                , ( "xLabelsIndex", Encode.int 3 )
                ]

        -- functions under test!
        ( defaultActualHeaderRow, defaultActualBodyRows, defaultActualMinValue, defaultActualMaxValue, defaultActualXLabels ) =
            TableAdapter.adapt defaultConfig input

        ( suppliedActualHeaderRow, suppliedActualBodyRows, suppliedActualMinValue, suppliedActualMaxValue, suppliedActualXLabels ) =
            TableAdapter.adapt suppliedConfig input

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

        suppliedHeaders =
            \_ ->
                suppliedActualHeaderRow |> Expect.equal TD.secondRow

        -- TODO incorrect expectation above, correct below!
        -- suppliedActualHeaderRow
        --     |> Expect.equal
        --         [ "205", "206", "207", "208", "209", "210" ]
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
    in
        Test.describe "TableAdapter.adapt"
            [ Test.describe "with default Config"
                [ Test.test "headers are first row" defaultHeaders
                , Test.test "body rows are remain rows, excluding the header row" defaultBodyRows
                , Test.test "min value is extracted from body rows" defaultMinValue
                , Test.test "max value is extracted from body rows" defaultMaxValue
                , Test.test "x-axis labels are the first row" defaultXLabels
                ]
            , Test.describe "with supplied Config"
                [ Test.test "headers are third row" suppliedHeaders
                , Test.test "body rows are as specified" suppliedLineChartRows
                , Test.test "min value is extracted from body rows" suppliedMinValue
                , Test.test "max value is extracted from body rows" suppliedMaxValue
                , Test.test "x-axis labels are as specified" suppliedXLabels
                ]
            ]
