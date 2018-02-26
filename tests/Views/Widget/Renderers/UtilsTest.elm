module Views.Widget.Renderers.UtilsTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Views.Widget.Renderers.Utils as Utils exposing (cssSafe, formatNumberTick)


formatNumberTickTest : Test
formatNumberTickTest =
    let
        zero =
            \_ -> Expect.equal (formatNumberTick 0) "0"

        underOneThousand =
            \_ -> Expect.equal (formatNumberTick 100) "100"

        oneThousand =
            \_ -> Expect.equal (formatNumberTick 1000) "1k"

        negativeOneThousand =
            \_ -> Expect.equal (formatNumberTick -1000) "-1k"

        justOverOneThousand =
            \_ -> Expect.equal (formatNumberTick 1100) "1.1k"

        justUnderOneMillion =
            \_ -> Expect.equal (formatNumberTick 999999) "999.999k"

        oneMillion =
            \_ -> Expect.equal (formatNumberTick 1000000) "1m"

        negativeOneMillion =
            \_ -> Expect.equal (formatNumberTick -1000000) "-1m"

        justOverOneMillion =
            \_ -> Expect.equal (formatNumberTick 1100000) "1.1m"

        tenMillion =
            \_ -> Expect.equal (formatNumberTick 10000000) "10m"

        negativeTenMillion =
            \_ -> Expect.equal (formatNumberTick -10000000) "-10m"

        tenAndAHalfMillion =
            \_ -> Expect.equal (formatNumberTick 10500000) "10.5m"

        untouchedString =
            \_ -> Expect.equal (formatNumberTick "foo") "foo"
    in
        Test.describe "Utils.formatNumberTick"
            [ Test.test "correctly formats a number = 0" zero
            , Test.test "correctly formats a number < 1000" underOneThousand
            , Test.test "correctly formats 1000" oneThousand
            , Test.test "correctly formats -1000" negativeOneThousand
            , Test.test "correctly formats a number > 1000 and < 1000000" justOverOneThousand
            , Test.test "correctly formats a larger number > 1000 and < 1000000" justUnderOneMillion
            , Test.test "correctly formats 1000000" oneMillion
            , Test.test "correctly formats -1000000" negativeOneMillion
            , Test.test "correctly formats a number > 1000000" justOverOneMillion
            , Test.test "correctly formats 10000000" tenMillion
            , Test.test "correctly formats -10000000" negativeTenMillion
            , Test.test "correctly formats 10500000" tenAndAHalfMillion
            , Test.test "when given a string returns the same string" untouchedString
            ]


cssSafeTest : Test
cssSafeTest =
    let
        runCssSafe =
            \_ -> Expect.equal (cssSafe "1_23'\"456 #My Widget & ting% ") "d-1-23456--my-widget--ting"
    in
        Test.describe "Utils.cssSafe"
            [ Test.test "returns a CSS safe class name" runCssSafe
            ]
