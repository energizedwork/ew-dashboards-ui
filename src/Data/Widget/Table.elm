module Data.Widget.Table exposing (Data, Row, Cell, decoder, rowDecoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (custom, decode, required)


type alias Cell =
    String


type alias Row =
    List Cell


type alias Data =
    { rows : List Row
    }


decoder : Decode.Decoder Data
decoder =
    decode Data
        |> required "data" (Decode.list rowDecoder)


rowDecoder : Decode.Decoder (List String)
rowDecoder =
    Decode.list Decode.string
