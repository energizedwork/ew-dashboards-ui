module Data.Widget.Adapters.CellPosition exposing (CellPosition(..), x, y, encode, decoder)

import Json.Decode as Decode exposing (Decoder, Value, index, int, map2)
import Json.Encode as Encode exposing (int)
import Tuple exposing (..)


type CellPosition
    = CellPosition ( Int, Int )


decoder : Decoder CellPosition
decoder =
    Decode.map2 (,) (index 0 Decode.int) (index 1 Decode.int)
        |> Decode.andThen
            (\( x, y ) -> Decode.succeed <| CellPosition ( x, y ))


encode : CellPosition -> Decode.Value
encode cellPosition =
    Encode.list
        [ Encode.int <| x cellPosition
        , Encode.int <| y cellPosition
        ]


x : CellPosition -> Int
x (CellPosition position) =
    Tuple.first position


y : CellPosition -> Int
y (CellPosition position) =
    Tuple.second position
