module Data.Widget.Adapters.TableAdapter exposing (adapt)

import Data.Widget.Table as Table exposing (Data, Row, Cell)


adapt : Data -> ( Row, List Row, Float )
adapt data =
    let
        headerRow =
            case List.head data.rows of
                Just header ->
                    header

                Nothing ->
                    []

        bodyRows =
            case List.tail data.rows of
                Just body ->
                    body

                Nothing ->
                    []

        rowToInt row =
            List.map (\n -> String.toFloat n |> Result.withDefault 0) row

        rowMax row =
            List.maximum (rowToInt row) |> Maybe.withDefault 0

        bodyRowMaxes =
            List.map rowMax bodyRows

        maxValue =
            (List.maximum bodyRowMaxes |> Maybe.withDefault 0)
    in
        ( headerRow, bodyRows, maxValue )
