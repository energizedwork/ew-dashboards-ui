module Data.Widget.Adapters.AdapterDecoderTest exposing (..)

import Expect exposing (Expectation)
import Data.Widget.Adapters.Adapter as Adapter exposing (Adapter(..))
import Data.Widget.Adapters.MetricAdapter as MetricAdapter exposing (Config, defaultConfig)
import Json.Decode as Decode exposing (..)
import Test exposing (..)


adapterDecoderTest : Test
adapterDecoderTest =
    let
        metricDefaultConfig =
            \_ ->
                let
                    input =
                        """
                        {
                            "type_":"METRIC"
                        }
                        """
                in
                    Expect.equal
                        (Decode.decodeString Adapter.decoder input)
                        (Ok
                            (Adapter.METRIC MetricAdapter.defaultConfig)
                        )

        metricWithConfig =
            \_ ->
                let
                    input =
                        """
                        {
                            "type_":"METRIC",
                            "config": {
                                "sourceCell": [1, 1],
                                "targetCell": [1, 3]
                            }

                        }
                        """
                in
                    Expect.equal
                        (Decode.decodeString Adapter.decoder input)
                        (Ok
                            (Adapter.METRIC <| MetricAdapter.Config ( 1, 1 ) ( 1, 3 ))
                        )
    in
        Test.describe "Adapter.decode"
            [ Test.describe "with no Config"
                [ Test.describe "METRIC"
                    [ Test.test "uses default config" metricDefaultConfig
                    ]
                ]
            , Test.describe "with user Config"
                [ Test.describe "METRIC"
                    [ Test.test "uses supplied config" metricWithConfig
                    ]
                ]
            ]
