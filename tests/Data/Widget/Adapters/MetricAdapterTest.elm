module Data.Widget.Adapters.MetricAdapterTest exposing (..)

import Expect exposing (Expectation)
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

        defaultConfig =
            MetricAdapter.defaultConfig

        suppliedConfig =
            MetricAdapter.Config ( 1, 1 ) ( 2, 1 )

        -- function under test!
        ( defaultActualSourceFigure, defaultActualTargetFigure ) =
            MetricAdapter.adapt defaultConfig input

        ( suppliedActualSourceFigure, suplliedActualTargetFigure ) =
            MetricAdapter.adapt suppliedConfig input

        -- expectations
        defaultSource =
            \_ -> Expect.equal defaultActualSourceFigure "Jan"

        defaultTarget =
            \_ -> Expect.equal defaultActualTargetFigure "Feb"

        suppliedSource =
            \_ -> Expect.equal suppliedActualSourceFigure "102"

        suppliedTarget =
            \_ -> Expect.equal suplliedActualTargetFigure "202"
    in
        Test.describe "MetricAdapter.adapt"
            [ Test.describe "with default Config"
                [ Test.test "source is figure at A1" defaultSource
                , Test.test "target is figure at A2" defaultTarget
                ]
            , Test.describe "with supplied Config"
                [ Test.test "source is figure at B2" suppliedSource
                , Test.test "target is figure at C2" suppliedTarget
                ]
            ]
