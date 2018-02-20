module Views.Widget.Renderers.UtilsTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Views.Widget.Renderers.Utils as Utils exposing (formatNumberTick)


formatNumberTickTest : Test
formatNumberTickTest =
    let
        underOneThousand =
            \_ -> Expect.equal (formatNumberTick 100) "100"

        oneThousand =
            \_ -> Expect.equal (formatNumberTick 1000) "1k"

        justOverOneThousand =
            \_ -> Expect.equal (formatNumberTick 1100) "1.1k"

        justUnderOneMillion =
            \_ -> Expect.equal (formatNumberTick 999999) "999.999k"

        oneMillion =
            \_ -> Expect.equal (formatNumberTick 1000000) "1m"

        justOverOneMillion =
            \_ -> Expect.equal (formatNumberTick 1100000) "1.1m"

        tenMillion =
            \_ -> Expect.equal (formatNumberTick 10000000) "10m"

        tenAndAHalfMillion =
            \_ -> Expect.equal (formatNumberTick 10500000) "10.5m"

        untouchedString =
            \_ -> Expect.equal (formatNumberTick "foo") "foo"
    in
        Test.describe "Utils.formatNumberTick"
            [ Test.test "correctly formats a number < 1000" underOneThousand
            , Test.test "correctly formats 1000" oneThousand
            , Test.test "correctly formats a number > 1000 and < 1000000" justOverOneThousand
            , Test.test "correctly formats a larger number > 1000 and < 1000000" justUnderOneMillion
            , Test.test "correctly formats 1000000" oneMillion
            , Test.test "correctly formats a number > 1000000" justOverOneMillion
            , Test.test "correctly formats 10000000" tenMillion
            , Test.test "correctly formats 10500000" tenAndAHalfMillion
            , Test.test "when given a string returns the same string" untouchedString
            ]
