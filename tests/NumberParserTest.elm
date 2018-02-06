module NumberParserTest exposing (..)

import Data.Widget.Adapters.Adapter as Adapter exposing (Adapter(..))
import Data.Widget.Adapters.CellPosition as CellPosition exposing (..)
import Data.Widget.Adapters.MetricAdapter as MetricAdapter exposing (defaultConfig)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import NumberParser
import Test exposing (..)


numberParserTest : Test
numberParserTest =
    let
        testSimpleParse =
            \_ ->
                let
                    input =
                        "303"
                in
                    Expect.equal (NumberParser.fromString input) 303

        testPercent =
            \_ ->
                let
                    input =
                        "606%"
                in
                    Expect.equal (NumberParser.fromString input) 606

        testCurrencyPounds =
            \_ ->
                let
                    input =
                        "£909"
                in
                    Expect.equal (NumberParser.fromString input) 909

        testCurrencyPoundsPence =
            \_ ->
                let
                    input =
                        "£808.88"
                in
                    Expect.equal (NumberParser.fromString input) 808.88
    in
        Test.describe "NumberParser"
            [ Test.describe "fromString"
                [ Test.test "simple parse" testSimpleParse
                , Test.test "removes percent" testPercent
                , Test.test "removes pound sign" testCurrencyPounds
                , Test.test "honours decimal place" testCurrencyPoundsPence
                ]
            ]
