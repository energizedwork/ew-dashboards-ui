module Data.Widget.Table exposing (Data, Row, Cell)


type alias Cell =
    String


type alias Row =
    List Cell


type alias Data =
    { rows : List Row
    }
