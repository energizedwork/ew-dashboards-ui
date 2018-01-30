module Data.Widget.Chart exposing (Data)

import Data.Widget.Table exposing (Row, Cell)


type alias Data =
    { rows : List Row
    , data : List (List ( Cell, Cell ))
    , indexedData : List ( Int, List ( Cell, Cell ) )
    , minValue : Float
    , maxValue : Float
    , xLabels : List String
    , seriesLabels : Maybe (List String)
    }
