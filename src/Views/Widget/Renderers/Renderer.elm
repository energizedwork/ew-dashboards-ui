module Views.Widget.Renderers.Renderer exposing (run)

import Data.Widget as Widget exposing (Widget, Body)
import Data.Widget.Table as Table exposing (Data, Cell)
import Data.Widget.Renderer exposing (Renderer(..))
import Views.Widget.Renderers.Table as Table
import Views.Widget.Renderers.BarChart as BarChart
import Views.Widget.Renderers.LineChart as LineChart
import Views.Spinner
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src)


run : Widget Body -> Table.Data -> Html msg
run widget data =
    case List.isEmpty data.rows of
        True ->
            div [ class "spinner-wrapper" ] [ Views.Spinner.spinnerSVG ]

        False ->
            case widget.renderer of
                TABLE ->
                    Table.render widget data

                LINE_CHART ->
                    LineChart.render widget data

                BAR_CHART ->
                    BarChart.render widget data
