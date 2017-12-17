module Data.Widget.Adapters.CellPositionTest exposing (..)

import Data.Widget.Adapters.AdapterTestData as TD
import Data.Widget.Adapters.CellPosition as CellPosition exposing (CellPosition(..), encode)
import Data.Widget.Adapters.CellRange exposing (CellRange)
import Data.Widget.Adapters.LineAndBarAdapter as LineAndBarAdapter exposing (adapt)
import Data.Widget.Table as Table exposing (Data)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import List.Extra
import Test exposing (..)


decoderTest : Test
decoderTest =
    let
        input =
            """
            [20, 4]
            """

        runDecode =
            \_ ->
                Decode.decodeString (CellPosition.decoder) input
                    |> Expect.equal
                        (Ok <|
                            CellPosition ( 20, 4 )
                        )
    in
        Test.describe "CellPosition"
            [ Test.test "decoder" runDecode
            ]


encoderTest : Test
encoderTest =
    let
        input =
            CellPosition ( 1, 5 )

        runEncode =
            \_ ->
                (toString <| CellPosition.encode input)
                    |> Expect.equal "{ 0 = 1, 1 = 5 }"
    in
        Test.describe "CellPosition encode"
            [ Test.test "encode" runEncode
            ]
