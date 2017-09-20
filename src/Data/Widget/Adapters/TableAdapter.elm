module Data.Widget.Adapters.TableAdapter exposing (adapt)

import Data.Widget.Table as Table exposing (Data, Row, Cell)


adapt : Data -> ( Row, List Row )
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
    in
        ( headerRow, bodyRows )
