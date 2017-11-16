module Data.Widget.Adapters.MetricAdapter exposing (Config, defaultConfig, adapt)

import Data.Widget.Table as Table exposing (Data, Row, Cell)
import Array


type alias RowNumber =
    Int


type alias ColumnNumber =
    Int


type alias Cell =
    ( RowNumber, ColumnNumber )


type alias Config =
    { sourceCell : Cell
    , targetCell : Cell
    }


defaultConfig : Config
defaultConfig =
    Config ( 0, 0 ) ( 0, 1 )


rowForCell : Array.Array (List String) -> Cell -> List String
rowForCell rows cellConfig =
    case Array.get (Tuple.first cellConfig) rows of
        Just row ->
            row

        Nothing ->
            []


valueForCell : Array.Array (List String) -> Cell -> String
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
