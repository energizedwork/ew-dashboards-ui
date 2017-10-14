module HeatMapTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Views.Widget.Renderers.HeatMap as HeatMap exposing (..)


colourScaleFromTest : Test
colourScaleFromTest =
    test "HeatMap.colourScaleFrom" <|
        \() ->
            let
                amount =
                    "250"

                maxValue =
                    500.0

                expected =
                    0.5
            in
                Expect.equal (HeatMap.colourScaleFrom amount maxValue) expected
