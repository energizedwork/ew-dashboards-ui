module Data.Widget.Adapters.MetricAdapter exposing (defaultConfig, adapt)

import Data.Widget.Adapters.CellPosition as CellPosition exposing (CellPosition, decoder)
import Data.Widget.Table as Table exposing (Data)
import Array
import Dict exposing (Dict)


defaultSourceCellPosition : CellPosition
defaultSourceCellPosition =
    ( 0, 0 )


defaultTargetCellPosition : CellPosition
defaultTargetCellPosition =
    ( 0, 1 )


defaultConfig : Dict String CellPosition
defaultConfig =
    Dict.fromList [ ( "sourceCell", defaultSourceCellPosition ), ( "targetCell", defaultTargetCellPosition ) ]


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


adapt : Dict String CellPosition -> Data -> ( String, String )
adapt config data =
    let
        rows =
            Array.fromList data.rows

        sourceValue =
            valueForCell rows (Maybe.withDefault defaultSourceCellPosition (Dict.get "sourceCell" config))

        targetValue =
            valueForCell rows (Maybe.withDefault defaultTargetCellPosition (Dict.get "targetCell" config))
    in
        ( sourceValue, targetValue )
