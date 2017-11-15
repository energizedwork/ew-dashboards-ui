module Data.Widget.Adapters.MetricAdapterTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Data.Widget.Adapters.AdapterTestData as TD
import Data.Widget.Adapters.MetricAdapter as MetricAdapter exposing (adapt)
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
            MetricAdapter.defaultConfig

        -- function under test!
        ( actualSourceFigure, actualTargetFigure ) =
            MetricAdapter.adapt config input

        -- expectations
        source =
            \_ -> Expect.equal actualSourceFigure "Jan"

        target =
            \_ -> Expect.equal actualTargetFigure "Feb"
    in
        Test.describe "MetricAdapter.adapt"
            [ Test.describe "with default Config"
                [ Test.test "source is figure at A1" source
                , Test.test "target is figure at A2" target
                ]
            , Test.todo "with supplied Config"
            ]
