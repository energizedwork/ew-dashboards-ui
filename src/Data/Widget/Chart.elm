module Data.Widget.Chart exposing (Data)

import Data.Widget.Table exposing (Cell, Row)


type alias Data =
    { rows : List Row
    , data : List (List ( Cell, Cell ))
    , indexedData : List ( Int, List ( Cell, Cell ) )
    , minValue : Float
    , maxValue : Float
    , xLabels : Maybe (List String)
    , yLabels : Maybe (List String)
    , seriesLabels : Maybe (List String)
    , xAxisLabel : Maybe String
    , yAxisLabel : Maybe String
    , forecastPosition : Maybe Int
    }
