module Data.Widget.Adapters.HeatMapAdapter exposing (adapt)

import Data.Widget.Table as Table exposing (Data, Row, Cell)


adapt : Data -> ( Row, List Row, Float, List String, List String )
adapt data =
    let
        headerRow =
            case List.head data.rows of
                Just header ->
                    header
                        |> List.tail
                        |> Maybe.withDefault []

                Nothing ->
                    []

        xLabels =
            headerRow

        yLabels =
            case List.tail data.rows of
                Just body ->
                    body
                        |> List.map
                            (\row ->
                                List.head row |> Maybe.withDefault ""
                            )

                Nothing ->
                    []

        bodyRows =
            case List.tail data.rows of
                Just body ->
                    body
                        |> List.map
                            (\row ->
                                List.tail row |> Maybe.withDefault []
                            )

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

        firstCellPerRow row =
            List.head row |> Maybe.withDefault ""
    in
        ( headerRow, bodyRows, maxValue, xLabels, yLabels )
