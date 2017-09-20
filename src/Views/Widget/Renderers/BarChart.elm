module Views.Widget.Renderers.BarChart exposing (render)

import Data.Widget as Widget exposing (Widget, Body)
import Data.Widget.Table as Table exposing (Data, Cell)
import Data.Widget.Adapters.Adapter exposing (Adapter(..))
import Data.Widget.Adapters.BarChartAdapter as BarChartAdapter
import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)


render : Widget Body -> Table.Data -> Html msg
render widget data =
    case widget.adapter of
        BAR_CHART ->
            let
                ( headerRow, valuesRow, maxValue ) =
                    BarChartAdapter.adapt data
            in
                view (List.map2 (,) headerRow valuesRow) maxValue

        _ ->
            p [ class "data" ] [ Html.text "Sorry, I can only render bar charts from a BAR_CHART adapter right now" ]


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    50


xScale : List ( String, String ) -> BandScale String
xScale data =
    Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } (List.map Tuple.first data) ( 0, w - 2 * padding )


yScale : Float -> ContinuousScale
yScale maxValue =
    Scale.linear ( 0, maxValue ) ( h - 2 * padding, 0 )


xAxis : List ( String, String ) -> Svg msg
xAxis data =
    Axis.axis { defaultOptions | orientation = Axis.Bottom } (Scale.toRenderable (xScale data))


yAxis : Float -> Svg msg
yAxis maxValue =
    Axis.axis { defaultOptions | orientation = Axis.Left, tickCount = 5 } (yScale maxValue)


column : BandScale String -> Float -> ( String, String ) -> Svg msg
column xScale maxValue ( header, value ) =
    g [ class "column" ]
        [ rect
            [ x <| toString <| Scale.convert xScale header
            , y <| toString <| Scale.convert (yScale maxValue) (String.toFloat value |> Result.withDefault 0)
            , width <| toString <| Scale.bandwidth xScale
            , height <| toString <| h - Scale.convert (yScale maxValue) (String.toFloat value |> Result.withDefault 0) - 2 * padding
            ]
            []
        , text_
            [ x <| toString <| Scale.convert (Scale.toRenderable xScale) header
            , y <| toString <| Scale.convert (yScale maxValue) (String.toFloat value |> Result.withDefault 0) - 5
            , textAnchor "middle"
            ]
            [ Svg.text <| toString value ]
        ]


view : List ( String, String ) -> Float -> Svg msg
view data maxValue =
    svg [ width (toString w ++ "px"), height (toString h ++ "px") ]
        [ Svg.style [] [ Svg.text """
            .column rect { fill: rgba(118, 214, 78, 0.8); }
            .column text { display: none; }
            .column:hover rect { fill: rgb(118, 214, 78); }
            .column:hover text { display: inline; }
          """ ]
        , g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString (h - padding) ++ ")") ]
            [ xAxis data ]
        , g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString padding ++ ")") ]
            [ (yAxis maxValue) ]
        , g [ transform ("translate(" ++ toString padding ++ ", " ++ toString padding ++ ")"), class "series" ] <|
            List.map (column (xScale data) maxValue) data
        ]
