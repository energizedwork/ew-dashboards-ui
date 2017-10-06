module Views.Widget.Renderers.Table exposing (render)

import Data.Widget as Widget exposing (Widget, Body)
import Data.Widget.Table as Table exposing (Data, Cell)
import Data.Widget.Adapters.Adapter exposing (Adapter(..))
import Data.Widget.Adapters.TableAdapter as TableAdapter
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src)


render : Widget Body -> Table.Data -> Html msg
render widget data =
    case widget.adapter of
        TABLE ->
            let
                ( headerRow, bodyRows, maxValue ) =
                    TableAdapter.adapt data
            in
                table [ class "table" ]
                    [ thead []
                        [ renderHeaderFrom headerRow
                        ]
                    , tbody [] <| renderBodyFrom bodyRows
                    ]

        _ ->
            p [ class "data" ] [ text "Sorry, I can only render tables from a TABLE adapter right now" ]


renderHeaderFrom : List String -> Html msg
renderHeaderFrom row =
    tr [] (List.map (\cell -> th [] [ text cell ]) row)


renderBodyFrom : List (List String) -> List (Html msg)
renderBodyFrom rows =
    List.map (\row -> tr [] (renderCells row)) rows


renderCells : List String -> List (Html msg)
renderCells row =
    List.map (\cell -> td [] [ text cell ]) row
