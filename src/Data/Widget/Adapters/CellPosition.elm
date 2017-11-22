module Data.Widget.Adapters.CellPosition exposing (CellPosition, decoder)

import Json.Decode as Decode exposing (Decoder, index, int, map2)


type alias RowNumber =
    Int


type alias ColumnNumber =
    Int


type alias CellPosition =
    ( RowNumber, ColumnNumber )


decoder : Decoder CellPosition
decoder =
    map2 (,) (index 0 int) (index 1 int)
