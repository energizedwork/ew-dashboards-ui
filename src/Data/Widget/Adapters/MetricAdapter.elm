module Data.Widget.Adapters.MetricAdapter exposing (Config, defaultConfig, adapt)

import Data.Widget.Table as Table exposing (Data, Row, Cell)
import List.Extra
import Views.Widget.Renderers.Utils as Utils exposing (..)
import Array


type alias Config =
    { sourceCell : ( Int, Int )
    , targetCell : ( Int, Int )
    }


defaultConfig : Config
defaultConfig =
    Config ( 0, 0 ) ( 1, 0 )


adapt : Config -> Data -> ( String, String )
adapt config data =
    let
        firstRow =
            case List.head data.rows of
                Just row ->
                    row

                Nothing ->
                    []

        source =
            case Array.get (0) (Array.fromList firstRow) of
                Just source ->
                    source

                Nothing ->
                    ""

        target =
            case Array.get (1) (Array.fromList firstRow) of
                Just target ->
                    target

                Nothing ->
                    ""
    in
        ( source, target )
