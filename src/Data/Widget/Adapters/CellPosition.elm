module Data.Widget.Adapters.CellPosition exposing (CellPosition, asJsonValue, decoder)

import Json.Decode as Decode exposing (Decoder, index, int, map2, Value)
import Json.Encode as Encode exposing (int)


type alias RowNumber =
    Int


type alias ColumnNumber =
    Int


type alias CellPosition =
    ( RowNumber, ColumnNumber )


decoder : Decoder CellPosition
decoder =
    map2 (,) (index 0 Decode.int) (index 1 Decode.int)


asJsonValue : CellPosition -> Decode.Value
asJsonValue cellPosition =
    Encode.list
        [ Encode.int <| Tuple.first cellPosition
        , Encode.int <| Tuple.second cellPosition
        ]
