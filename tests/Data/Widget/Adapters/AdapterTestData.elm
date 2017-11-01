module Data.Widget.Adapters.AdapterTestData exposing (..)


headerRow : List String
headerRow =
    [ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" ]


firstRow : List String
firstRow =
    List.range 101 112 |> List.map toString


secondRow : List String
secondRow =
    List.range 201 212 |> List.map toString


thirdRow : List String
thirdRow =
    List.range 301 312 |> List.map toString


forthRow : List String
forthRow =
    List.range 401 412 |> List.map toString
