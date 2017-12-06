module Data.Widget.Adapters.MetricAdapterTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Data.Widget.Adapters.AdapterTestData as TD
import Data.Widget.Adapters.CellPosition as CellPosition exposing (..)
import Data.Widget.Adapters.MetricAdapter as MetricAdapter exposing (adapt)
import Data.Widget.Table as Table exposing (Data)
import Dict exposing (Dict)
import Json.Encode as Encode exposing (..)


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

        suppliedConfigSource =
            CellPosition.asJsonValue ( 1, 1 )

        suppliedConfigTarget =
            CellPosition.asJsonValue ( 2, 1 )

        suppliedConfig =
            Dict.fromList
                [ ( "sourceCell", suppliedConfigSource )
                , ( "targetCell", suppliedConfigTarget )
                ]

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
