module Data.Widget.Adapters.CellRange exposing (CellRange, asJsonValue, decoder, extractRows)

import Array exposing (..)
import Data.Widget.Adapters.CellPosition as CellPosition exposing (CellPosition, asJsonValue, decoder)
import Data.Widget.Table exposing (..)
import Json.Decode as Decode exposing (Decoder, Value, index, int, map2)
import Json.Encode as Encode exposing (int)
import List.Extra exposing (..)


-- Origin is (x1, y1) !!!


type alias CellRange =
    List CellPosition


decoder : Decoder CellRange
decoder =
    Decode.list CellPosition.decoder


asJsonValue : CellRange -> Decode.Value
asJsonValue cellRanges =
    List.map CellPosition.asJsonValue cellRanges
        |> Encode.list


extractRows : Data -> CellRange -> List Row
extractRows data range =
    let
        lowerBound =
            (Array.get 0 (Array.fromList range)
                |> Maybe.withDefault ( 1, 1 )
            )

        upperBound =
            (Array.get 1 (Array.fromList range)
                |> Maybe.withDefault ( 3, 2 )
            )

        cellStart =
            Tuple.first lowerBound

        cellEnd =
            Tuple.first upperBound

        rowStart =
            ((Tuple.second lowerBound))

        rowEnd =
            ((Tuple.second upperBound))

        rowRange =
            (List.range rowStart rowEnd)

        extractedRows =
            List.map
                (\rowIndex ->
                    let
                        row =
                            List.Extra.getAt (rowIndex - 1) data.rows
                                |> Maybe.withDefault []

                        start =
                            case cellStart > 1 of
                                True ->
                                    cellStart - 1

                                False ->
                                    0
                    in
                        List.drop (start) row
                            |> List.take (cellEnd - start)
                )
                rowRange
    in
        extractedRows
