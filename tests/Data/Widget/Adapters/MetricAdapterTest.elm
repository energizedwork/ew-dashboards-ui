module Data.Widget.Adapters.MetricAdapterTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Data.Widget.Adapters.AdapterTestData as TD
import Data.Widget.Adapters.CellPosition as CellPosition exposing (..)
import Data.Widget.Adapters.MetricAdapter as MetricAdapter exposing (adapt)
import Data.Widget.Table as Table exposing (Data)
import Dict exposing (Dict)


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

        suppliedConfigSubtitle =
            CellPosition.encode <| CellPosition ( 2, 1 )

        suppliedConfigActual =
            CellPosition.encode <| CellPosition ( 2, 2 )

        suppliedConfigTarget =
            CellPosition.encode <| CellPosition ( 2, 3 )

        suppliedConfigChange =
            CellPosition.encode <| CellPosition ( 3, 3 )

        suppliedConfigLastUpdated =
            CellPosition.encode <| CellPosition ( 3, 4 )

        suppliedConfig =
            Dict.fromList
                [ ( "subtitleCell", suppliedConfigSubtitle )
                , ( "actualCell", suppliedConfigActual )
                , ( "targetCell", suppliedConfigTarget )
                , ( "changeCell", suppliedConfigChange )
                , ( "lastUpdatedCell", suppliedConfigLastUpdated )
                ]

        -- function under test!
        ( defaultSubtitle, defaultActualFigure, defaultTargetFigure, defaultChangeFigure, defaultLastUpdated ) =
            MetricAdapter.adapt defaultConfig input

        ( suppliedSubtitle, suppliedActualFigure, suppliedTargetFigure, suppliedChangeFigure, suppliedLastUpdated ) =
            MetricAdapter.adapt suppliedConfig input

        -- expectations
        expectedDefaultSubtitle =
            \_ -> Expect.equal defaultSubtitle "Jan"

        expectedDefaultActual =
            \_ -> Expect.equal defaultActualFigure "Jan"

        expectedDefaultTarget =
            \_ -> Expect.equal defaultTargetFigure "Jan"

        expectedDefaultChange =
            \_ -> Expect.equal defaultChangeFigure "Jan"

        expectedDefaultLastUpdated =
            \_ -> Expect.equal defaultLastUpdated "Jan"

        expectedSuppliedSubtitle =
            \_ -> Expect.equal suppliedSubtitle "Feb"

        expectedSuppliedActual =
            \_ -> Expect.equal suppliedActualFigure "102"

        expectedSuppliedTarget =
            \_ -> Expect.equal suppliedTargetFigure "202"

        expectedSuppliedChange =
            \_ -> Expect.equal suppliedChangeFigure "203"

        expectedSuppliedLastUpdated =
            \_ -> Expect.equal suppliedLastUpdated "303"
    in
        Test.describe "MetricAdapter.adapt"
            [ Test.describe "with default Config"
                [ Test.test "subtitle is figure at A1" expectedDefaultSubtitle
                , Test.test "actual is figure at A1" expectedDefaultActual
                , Test.test "target is figure at A1" expectedDefaultTarget
                , Test.test "change is figure at A1" expectedDefaultChange
                , Test.test "lastUpdated is figure at A1" expectedDefaultLastUpdated
                ]
            , Test.describe "with supplied Config"
                [ Test.test "subtitle is figure at A2" expectedSuppliedSubtitle
                , Test.test "actual is figure at B2" expectedSuppliedActual
                , Test.test "target is figure at B3" expectedSuppliedTarget
                , Test.test "change is figure at C3" expectedSuppliedChange
                , Test.test "lastUpdated is figure at C4" expectedSuppliedLastUpdated
                ]
            ]
