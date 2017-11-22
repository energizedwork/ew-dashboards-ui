module Data.Widget.Adapters.CellPosition exposing (CellPosition, cellPositionDecoder)

import Json.Decode as Decode exposing (Decoder, index, int, map2)


type alias RowNumber =
    Int


type alias ColumnNumber =
    Int


type alias CellPosition =
    ( RowNumber, ColumnNumber )


cellPositionDecoder : Decoder CellPosition
cellPositionDecoder =
    -- Why is CellPosition not available here? The following line throws a compiler error `Cannot find variable `CellPosition``
    -- But CellPosition is exposed by MetricAdapter 🤔
    -- map2 CellPosition (index 0 int) (index 1 int)
    map2 (,) (index 0 int) (index 1 int)
