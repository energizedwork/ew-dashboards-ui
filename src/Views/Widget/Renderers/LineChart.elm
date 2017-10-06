module Views.Widget.Renderers.LineChart exposing (render)

import Data.Widget as Widget exposing (Widget, Body)
import Data.Widget.Table as Table exposing (Data, Cell)
import Data.Widget.Adapters.Adapter exposing (Adapter(..))
import Data.Widget.Adapters.TableAdapter as TableAdapter
import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import Visualization.Shape as Shape


render : Widget Body -> Table.Data -> Html msg
render widget data =
    case widget.adapter of
        TABLE ->
            let
                ( headerRow, bodyRows, maxValue ) =
                    TableAdapter.adapt data

                valuesRow =
                    List.head bodyRows |> Maybe.withDefault []

                -- TODO: iterate to create a collection of these tuple rows?
                tupleData =
                    (List.map2 (,) headerRow valuesRow)
            in
                view tupleData maxValue

        _ ->
            p [ class "data" ] [ Html.text "Sorry, I can only render line charts from a TABLE adapter right now" ]


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    50


view : List ( String, String ) -> Float -> Svg msg
view data maxValue =
    let
        xScale : BandScale String
        xScale =
            Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } (List.map Tuple.first data) ( 0, w - 2 * padding )

        yScale : ContinuousScale
        yScale =
            Scale.linear ( 0, maxValue ) ( h - 2 * padding, 0 )

        opts : Axis.Options a
        opts =
            Axis.defaultOptions

        xAxis : Svg msg
        xAxis =
            Axis.axis { opts | orientation = Axis.Bottom, tickCount = List.length data } (Scale.toRenderable xScale)

        yAxis : Svg msg
        yAxis =
            Axis.axis { opts | orientation = Axis.Left, tickCount = 5 } yScale

        areaGenerator : ( String, String ) -> Maybe ( ( Float, Float ), ( Float, Float ) )
        areaGenerator ( x, y ) =
            Just ( ( Scale.convert xScale x, Tuple.first (Scale.rangeExtent yScale) ), ( Scale.convert xScale x, Scale.convert yScale (String.toFloat y |> Result.withDefault 0) ) )

        lineGenerator : ( String, String ) -> Maybe ( Float, Float )
        lineGenerator ( x, y ) =
            Just ( Scale.convert xScale x, Scale.convert yScale (String.toFloat y |> Result.withDefault 0) )

        area : String
        area =
            List.map areaGenerator data
                |> Shape.area Shape.linearCurve

        line : String
        line =
            List.map lineGenerator data
                |> Shape.line Shape.linearCurve
    in
        svg [ width (toString w ++ "px"), height (toString h ++ "px") ]
            [ g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString (h - padding) ++ ")") ]
                [ xAxis ]
            , g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString padding ++ ")") ]
                [ yAxis ]
            , g [ transform ("translate(78" ++ ", " ++ toString padding ++ ")"), class "series" ]
                [ Svg.path [ d line, stroke "red", strokeWidth "3px", fill "none" ] []
                  -- , Svg.path [ d area, stroke "none", strokeWidth "3px", fill "rgba(255, 0, 0, 0.54)" ] []
                ]
            ]
