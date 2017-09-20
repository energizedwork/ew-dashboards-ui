module Data.Widget.Adapters.BarChartAdapter exposing (adapt)

import Data.Widget.Table as Table exposing (Data, Row, Cell)


adapt : Data -> ( Row, Row )
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
                    case List.head body of
                        Just firstBodyRow ->
                            firstBodyRow

                        Nothing ->
                            []

                Nothing ->
                    []
    in
        ( headerRow, bodyRows )
