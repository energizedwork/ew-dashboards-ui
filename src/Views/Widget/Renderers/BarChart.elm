module Views.Widget.Renderers.BarChart
    exposing
        ( render
        , renderColumn
        , renderColumns
        , renderXAxis
        , renderYAxis
        , renderLegend
        , legendLabel
        )

import Array exposing (..)
import Color
import Color.Convert
import Data.Widget as Widget exposing (Body, Widget)
import Data.Widget.Adapters.Adapter exposing (Adapter(..))
import Data.Widget.Adapters.ChartAdapter as ChartAdapter
import Data.Widget.Config as RendererConfig
import Data.Widget.Table as Table exposing (Cell, Data)
import Html exposing (..)
import NumberParser
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Views.Widget.Renderers.ChartLegend as ChartLegend
import Views.Widget.Renderers.Config as ViewConfig exposing (defaultChartPadding)
import Views.Widget.Renderers.Utils as Utils exposing (..)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import Views.Widget.Renderers.ChartLegend as ChartLegend
import Views.Widget.Renderers.ChartAxisLabels as ChartAxisLabels
import Data.Widget.Chart as Chart


render : RendererConfig.Config -> Int -> Int -> Widget -> Table.Data -> Html msg
render optionalRendererConfig width height widget data =
    case widget.adapter of
        CHART optionalConfig ->
            let
                chartData =
                    ChartAdapter.adapt optionalConfig data

                calculatedWidth =
                    ViewConfig.calculateWidth optionalRendererConfig width

                calculatedHeight =
                    ViewConfig.calculateHeight optionalRendererConfig height
            in
                div [ class <| ViewConfig.colSpanClass optionalRendererConfig ++ " widget" ]
                    [ Utils.renderTitleFrom widget
                    , view calculatedWidth calculatedHeight chartData
                    ]

        _ ->
            p [ class "data" ] [ Html.text "Sorry, I can only render bar charts from a CHART adapter right now" ]


barColours : Array Color.Color
barColours =
    Array.fromList Scale.category10


getBarColour : Int -> String
getBarColour index =
    get index barColours
        |> Maybe.withDefault Color.black
        |> Color.Convert.colorToHex


xScale : Int -> List ( String, String ) -> BandScale String
xScale width data =
    Scale.band
        { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 }
        (List.map Tuple.first data)
        ( 0, toFloat width - defaultChartPadding.totalHorizontal )


yScale : Int -> Float -> ContinuousScale
yScale height maxValue =
    Scale.linear ( 0, maxValue ) ( toFloat height - defaultChartPadding.totalVertical, 0 )


xAxis : Int -> List ( String, String ) -> Svg msg
xAxis width data =
    Axis.axis
        { defaultOptions
            | orientation = Axis.Bottom
            , tickFormat = Just Utils.formatStringTick
        }
        (Scale.toRenderable (xScale width data))


yAxis : Int -> Float -> Svg msg
yAxis height maxValue =
    Axis.axis { defaultOptions | orientation = Axis.Left, tickCount = 5 } (yScale height maxValue)


column : Int -> Int -> Int -> String -> BandScale String -> Float -> ( String, String ) -> Svg msg
column height index totalRows colour xScaleBand maxValue ( header, value ) =
    let
        xpos =
            (Scale.convert xScaleBand header) + (colWidth * (toFloat index))

        xposText =
            (Scale.convert (Scale.toRenderable xScaleBand) header) + (colWidth * (toFloat index))

        valueSantized =
            NumberParser.fromString value

        ypos =
            Scale.convert (yScale height maxValue) valueSantized

        yposText =
            Scale.convert (yScale height maxValue) valueSantized - 5

        colWidth =
            Scale.bandwidth xScaleBand / toFloat (totalRows)

        colHeight =
            toFloat height - Scale.convert (yScale height maxValue) valueSantized - defaultChartPadding.totalVertical

        makeTitle =
            value
    in
        g [ class "column" ]
            [ rect
                [ x <| toString xpos
                , y <| toString ypos
                , Svg.Attributes.width <| toString colWidth
                , Svg.Attributes.height <| toString colHeight
                , fill colour
                ]
                [ Svg.title []
                    [ Svg.text makeTitle
                    ]
                ]
            ]


view : Int -> Int -> Chart.Data -> Svg msg
view w h chartData =
    let
        firstDataTuple =
            List.head chartData.data |> Maybe.withDefault []

        indexedData =
            Array.toIndexedList (Array.fromList chartData.data)

        totalRows =
            List.length indexedData

        yTicksCount =
            5

        yAxisScale =
            (yScale h chartData.maxValue)

        yGridTicks =
            Scale.ticks (yScale h chartData.maxValue) yTicksCount
    in
        svg [ Svg.Attributes.width (toString w ++ "px"), Svg.Attributes.height (toString h ++ "px") ]
            (List.concat
                [ [ Svg.style []
                        [ Svg.text """
                            .column text { display: none; }
                            .column:hover rect { opacity: 0.7; cursor: crosshair; }
                            .column:hover text { display: inline; z-index: 9999; }
                          """ ]
                  , renderXAxis w h firstDataTuple
                  , renderYAxis w h chartData.maxValue
                  , Utils.renderYGrid w h defaultChartPadding chartData.maxValue (yScale h chartData.maxValue) yGridTicks
                  ]
                , renderColumns w h chartData.maxValue totalRows indexedData
                , renderLegend w h chartData.seriesLabels
                , [ ChartAxisLabels.renderXAxisLabel w h chartData.xAxisLabel defaultChartPadding ]
                , [ ChartAxisLabels.renderYAxisLabel h chartData.yAxisLabel defaultChartPadding ]
                ]
            )


renderColumns :
    Int
    -> Int
    -> Float
    -> Int
    -> List ( Int, List ( String, String ) )
    -> List (Svg msg)
renderColumns width height maxValue totalRows indexedData =
    List.map
        (\( index, row ) ->
            renderColumn width height index totalRows row maxValue
        )
        indexedData


renderColumn :
    Int
    -> Int
    -> Int
    -> Int
    -> List ( String, String )
    -> Float
    -> Svg msg
renderColumn width height index totalRows row maxValue =
    g [ transform ("translate(" ++ toString defaultChartPadding.left ++ ", " ++ toString defaultChartPadding.top ++ ")"), class "series" ] <|
        List.map (column height index totalRows (getBarColour index) (xScale width row) maxValue) row


renderXAxis : Int -> Int -> List ( Cell, Cell ) -> Svg msg
renderXAxis width height firstDataTuple =
    g [ transform ("translate(" ++ toString (defaultChartPadding.left - 1) ++ ", " ++ toString (toFloat height - defaultChartPadding.bottom) ++ ")") ]
        [ xAxis width firstDataTuple ]


renderYAxis : Int -> Int -> Float -> Svg msg
renderYAxis width height maxValue =
    g [ transform ("translate(" ++ toString (defaultChartPadding.left - 1) ++ ", " ++ toString defaultChartPadding.top ++ ")") ]
        [ (yAxis height maxValue) ]


legendLabel : Int -> String -> Svg msg
legendLabel index labelText =
    ChartLegend.createHorizontalLabel index labelText "■" getBarColour


renderLegend :
    Int
    -> Int
    -> Maybe (List String)
    -> List (Svg msg)
renderLegend width height seriesLabels =
    let
        labels =
            ChartLegend.createLabels seriesLabels legendLabel
    in
        ChartLegend.renderBottomCenterAligned width height labels
