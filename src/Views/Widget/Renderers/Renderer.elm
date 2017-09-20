module Views.Widget.Renderers.Renderer exposing (run)

import Data.Widget as Widget exposing (Widget, Body)
import Data.Widget.Table as Table exposing (Data, Cell)
import Data.Widget.Renderer exposing (Renderer(..))
import Views.Widget.Renderers.Table as Table
import Views.Widget.Renderers.BarChart as BarChart
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src)


run : Widget Body -> Table.Data -> Html msg
run widget data =
    case widget.renderer of
        TABLE ->
            Table.render widget data

        LINE ->
            renderLineGraphFrom widget data

        BAR_CHART ->
            BarChart.render widget data


renderLineGraphFrom : Widget Body -> Table.Data -> Html msg
renderLineGraphFrom widget data =
    p [ class "data" ] [ text <| "TODO INSERT LINE GRAPH HERE" ++ (toString data.rows) ]


renderBarChartFrom : Widget Body -> Table.Data -> Html msg
renderBarChartFrom widget data =
    p [ class "data" ] [ text <| "TODO INSERT BAR CHART HERE" ++ (toString data.rows) ]
