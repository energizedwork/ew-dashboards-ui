module Data.Widget.Adapters.MetricAdapter exposing (Config, defaultConfig, adapt)

import Data.Widget.Adapters.CellPosition exposing (CellPosition)
import Data.Widget.Table as Table exposing (Data)
import Array


type alias Config =
    { sourceCell : CellPosition
    , targetCell : CellPosition
    }


defaultConfig : Config
defaultConfig =
    Config ( 0, 0 ) ( 0, 1 )


rowForCell : Array.Array (List String) -> CellPosition -> List String
rowForCell rows cellConfig =
    case Array.get (Tuple.first cellConfig) rows of
        Just row ->
            row

        Nothing ->
            []


valueForCell : Array.Array (List String) -> CellPosition -> String
valueForCell rows cell =
    let
        row =
            rowForCell rows cell
    in
        case Array.get (Tuple.second cell) (Array.fromList row) of
            Just value ->
                value

            Nothing ->
                ""


adapt : Config -> Data -> ( String, String )
adapt config data =
    let
        rows =
            Array.fromList data.rows

        sourceValue =
            valueForCell rows config.sourceCell

        targetValue =
            valueForCell rows config.targetCell
    in
        ( sourceValue, targetValue )
