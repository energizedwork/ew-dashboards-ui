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
import Views.Widget.Renderers.Config as ViewConfig exposing (defaultChartPadding, ChartPadding)
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


xScale : Int -> List ( String, String ) -> ChartPadding -> BandScale String
xScale width data chartPadding =
    Scale.band
        { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 }
        (List.map Tuple.first data)
        ( 0, toFloat width - chartPadding.totalHorizontal )


yScale : Int -> Float -> ChartPadding -> ContinuousScale
yScale height maxValue chartPadding =
    Scale.linear ( 0, maxValue ) ( toFloat height - chartPadding.totalVertical, 0 )


xAxis : Int -> List ( String, String ) -> ChartPadding -> Svg msg
xAxis width data chartPadding =
    Axis.axis
        { defaultOptions
            | orientation = Axis.Bottom
            , tickFormat = Just Utils.formatStringTick
        }
        (Scale.toRenderable (xScale width data chartPadding))


yAxis : Int -> Float -> ChartPadding -> Svg msg
yAxis height maxValue chartPadding =
    Axis.axis { defaultOptions | orientation = Axis.Left, tickCount = 5, tickFormat = Just Utils.formatNumberTick } (yScale height maxValue chartPadding)


column : Int -> Int -> Int -> String -> BandScale String -> ChartPadding -> Float -> ( String, String ) -> Svg msg
column height index totalRows colour xScaleBand chartPadding maxValue ( header, value ) =
    let
        xpos =
            (Scale.convert xScaleBand header) + (colWidth * (toFloat index))

        xposText =
            (Scale.convert (Scale.toRenderable xScaleBand) header) + (colWidth * (toFloat index))

        valueSantized =
            NumberParser.fromString value

        ypos =
            Scale.convert (yScale height maxValue chartPadding) valueSantized

        yposText =
            Scale.convert (yScale height maxValue chartPadding) valueSantized - 5

        colWidth =
            Scale.bandwidth xScaleBand / toFloat (totalRows)

        colHeight =
            toFloat height - Scale.convert (yScale height maxValue chartPadding) valueSantized - chartPadding.totalVertical

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
            Scale.ticks (yScale h chartData.maxValue defaultChartPadding) yTicksCount
    in
        svg [ Svg.Attributes.width (toString w ++ "px"), Svg.Attributes.height (toString h ++ "px") ]
            (List.concat
                [ [ Svg.style []
                        [ Svg.text """
                            .column text { display: none; }
                            .column:hover rect { opacity: 0.7; cursor: crosshair; }
                            .column:hover text { display: inline; z-index: 9999; }
                          """ ]
                  , renderXAxis w h firstDataTuple defaultChartPadding
                  , renderYAxis w h chartData.maxValue defaultChartPadding
                  , Utils.renderYGrid w h defaultChartPadding chartData.maxValue (yScale h chartData.maxValue defaultChartPadding) yGridTicks
                  ]
                , renderColumns w h chartData.maxValue totalRows indexedData defaultChartPadding
                , renderLegend w h chartData.seriesLabels
                , [ ChartAxisLabels.renderXAxisLabel w h chartData.xAxisLabel defaultChartPadding ]
                , [ ChartAxisLabels.renderLeftYAxisLabel h chartData.yAxisLabel defaultChartPadding ]
                ]
            )


renderColumns :
    Int
    -> Int
    -> Float
    -> Int
    -> List ( Int, List ( String, String ) )
    -> ChartPadding
    -> List (Svg msg)
renderColumns width height maxValue totalRows indexedData chartPadding =
    List.map
        (\( index, row ) ->
            renderColumn width height index totalRows row maxValue chartPadding
        )
        indexedData


renderColumn :
    Int
    -> Int
    -> Int
    -> Int
    -> List ( String, String )
    -> Float
    -> ChartPadding
    -> Svg msg
renderColumn width height index totalRows row maxValue chartPadding =
    g [ transform ("translate(" ++ toString chartPadding.left ++ ", " ++ toString chartPadding.top ++ ")"), class "series" ] <|
        List.map (column height index totalRows (getBarColour index) (xScale width row chartPadding) chartPadding maxValue) row


renderXAxis : Int -> Int -> List ( Cell, Cell ) -> ChartPadding -> Svg msg
renderXAxis width height firstDataTuple chartPadding =
    g [ transform ("translate(" ++ toString (chartPadding.left - 1) ++ ", " ++ toString (toFloat height - chartPadding.bottom) ++ ")") ]
        [ xAxis width firstDataTuple chartPadding ]


renderYAxis : Int -> Int -> Float -> ChartPadding -> Svg msg
renderYAxis width height maxValue chartPadding =
    g [ transform ("translate(" ++ toString (chartPadding.left - 1) ++ ", " ++ toString chartPadding.top ++ ")") ]
        [ (yAxis height maxValue chartPadding) ]


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
