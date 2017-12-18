module Data.Widget.Adapters.CellRange exposing (CellRange, encode, decoder, extractRow, extractRows, firstRowRange, remainingRowsRange)

import Data.Widget.Adapters.CellPosition as CellPosition exposing (CellPosition(..), encode, decoder)
import Data.Widget.Table exposing (..)
import Json.Decode as Decode exposing (Decoder, Value, index, int, map2)
import Json.Encode as Encode exposing (int)
import List.Extra exposing (..)


-- Origin is (x1, y1) !!!


type alias CellRange =
    { start : CellPosition
    , end : CellPosition
    }


decoder : Decoder CellRange
decoder =
    Decode.map2 CellRange
        (Decode.field "start" CellPosition.decoder)
        (Decode.field "end" CellPosition.decoder)


encode : CellRange -> Decode.Value
encode cellRange =
    Encode.object
        [ ( "start", CellPosition.encode cellRange.start )
        , ( "end", CellPosition.encode cellRange.end )
        ]


extractRows : Data -> CellRange -> List Row
extractRows data range =
    let
        lowerBound =
            range.start

        upperBound =
            range.end

        cellStart =
            CellPosition.x lowerBound

        cellEnd =
            CellPosition.x upperBound

        rowStart =
            CellPosition.y lowerBound

        rowEnd =
            CellPosition.y upperBound

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


extractRow : Data -> CellRange -> Row
extractRow data range =
    List.head (extractRows data range)
        |> Maybe.withDefault []


firstRowRange : Data -> CellRange
firstRowRange data =
    let
        firstRow =
            List.head data.rows
                |> Maybe.withDefault []

        numberCells =
            List.length firstRow
    in
        CellRange
            (CellPosition ( 1, 1 ))
            (CellPosition ( numberCells, 1 ))


remainingRowsRange : Data -> CellRange
remainingRowsRange data =
    let
        remainingRows =
            List.tail data.rows
                |> Maybe.withDefault []

        firstRow =
            List.head remainingRows
                |> Maybe.withDefault []

        numberCells =
            List.length firstRow

        numberRows =
            List.length data.rows
    in
        CellRange
            (CellPosition ( 1, 2 ))
            (CellPosition ( numberCells, numberRows ))
