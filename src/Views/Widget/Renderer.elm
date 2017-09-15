module Views.Widget.Renderer exposing (run)

import Data.Widget as Widget exposing (Widget, Body)
import Data.Widget.Table as Table exposing (Data, Cell)
import Data.Widget.Adapter exposing (Adapter(..))
import Data.Widget.Renderer exposing (Renderer(..))
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src)
import Html.Events exposing (onClick)


run : Widget Body -> Table.Data -> Html msg
run widget data =
    case widget.renderer of
        TABLE ->
            renderTableFrom widget data

        LINE ->
            renderLineGraphFrom widget data


renderTableFrom : Widget Body -> Table.Data -> Html msg
renderTableFrom widget data =
    case widget.adapter of
        TWO_D ->
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
                table [ class "table" ]
                    [ thead []
                        [ renderHeaderFrom headerRow
                        ]
                    , tbody [] <| renderBodyFrom bodyRows
                    ]

        _ ->
            p [ class "data" ] [ text "Sorry, I can only render tables from a TWO_D adapter right now" ]


renderLineGraphFrom : Widget Body -> Table.Data -> Html msg
renderLineGraphFrom widget data =
    p [ class "data" ] [ text <| "TODO INSERT LINE GRAPH HERE" ++ (toString data.rows) ]


renderHeaderFrom : List String -> Html msg
renderHeaderFrom row =
    tr [] (List.map (\cell -> th [] [ text cell ]) row)


renderBodyFrom : List (List String) -> List (Html msg)
renderBodyFrom rows =
    List.map (\row -> tr [] (renderCells row)) rows


renderCells : List String -> List (Html msg)
renderCells row =
    List.map (\cell -> td [] [ text cell ]) row



--
