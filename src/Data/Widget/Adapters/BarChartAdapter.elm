module Data.Widget.Adapters.BarChartAdapter exposing (adapt)

import Data.Widget.Table as Table exposing (Data, Row, Cell)


adapt : Data -> ( Row, Row, Float )
adapt data =
    let
        headerRow =
            case List.head data.rows of
                Just header ->
                    header

                Nothing ->
                    []

        bodyRow =
            case List.tail data.rows of
                Just body ->
                    case List.head body of
                        Just firstBodyRow ->
                            firstBodyRow

                        Nothing ->
                            []

                Nothing ->
                    []

        -- TODO unhackme! At what point should we assume Floats?
        bodyRowInts =
            case List.tail data.rows of
                Just body ->
                    case List.head body of
                        Just firstBodyRow ->
                            List.map (stringToFloat) firstBodyRow

                        Nothing ->
                            []

                Nothing ->
                    []
    in
        ( headerRow, bodyRow, (List.maximum bodyRowInts |> Maybe.withDefault 0) )


stringToFloat : String -> Float
stringToFloat string =
    String.toFloat string |> Result.withDefault 0
