module Views.Widget.Renderers.BarChartHorizontal
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
import Data.Widget.Adapters.TableAdapter exposing (Orientation(..))
import Data.Widget.Chart as Chart exposing (..)
import Data.Widget.Config as RendererConfig
import Data.Widget.Table as Table exposing (Cell, Data)
import Html exposing (..)
import NumberParser
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Views.Widget.Renderers.ChartAxisLabels as ChartAxisLabels
import Views.Widget.Renderers.ChartLegend as ChartLegend
import Views.Widget.Renderers.Config as ViewConfig exposing (ChartPadding, defaultChartPadding)
import Views.Widget.Renderers.Utils as Utils exposing (..)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)


-- Public ----------------------------------------------------------------------


render : RendererConfig.Config -> Int -> Int -> Widget -> Table.Data -> Html msg
render optionalRendererConfig width height widget data =
    case widget.adapter of
        CHART optionalConfig ->
            let
                chartData =
                    ChartAdapter.adapt optionalConfig data Horizontal

                calculatedWidth =
                    ViewConfig.calculateWidth optionalRendererConfig width

                calculatedHeight =
                    ViewConfig.calculateHeight optionalRendererConfig height

                body =
                    div []
                        [ Utils.renderTitleFrom widget
                        , view calculatedWidth calculatedHeight chartData
                        ]
            in
                div
                    [ class <|
                        ViewConfig.colSpanClass optionalRendererConfig
                            ++ " widget"
                    ]
                <|
                    Utils.renderWidgetBody data body

        _ ->
            p [ class "data" ]
                [ Html.text
                    "Sorry, I can only render bar charts from a CHART adapter right now"
                ]


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
    g
        [ transform
            ("translate("
                ++ toString chartPadding.left
                ++ ", "
                ++ toString chartPadding.top
                ++ ")"
            )
        , class "series"
        ]
    <|
        List.map
            (column width
                index
                totalRows
                (getBarColour index)
                (yScale height row chartPadding)
                chartPadding
                maxValue
            )
            row


renderYAxis : Int -> Int -> List ( Cell, Cell ) -> ChartPadding -> Svg msg
renderYAxis width height firstDataTuple chartPadding =
    g
        [ transform
            ("translate("
                ++ toString (chartPadding.left - 1)
                ++ ", "
                ++ toString chartPadding.top
                ++ ")"
            )
        ]
        [ yAxis height firstDataTuple chartPadding ]


renderXAxis : Int -> Int -> Float -> ChartPadding -> Svg msg
renderXAxis width height maxValue chartPadding =
    g
        [ transform
            ("translate("
                ++ toString (chartPadding.left - 1)
                ++ ", "
                ++ toString (toFloat height - chartPadding.bottom)
                ++ ")"
            )
        ]
        [ (xAxis width maxValue chartPadding) ]


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


legendLabel : Int -> String -> Svg msg
legendLabel index labelText =
    ChartLegend.createHorizontalLabel index labelText "■" getBarColour



-- Private ---------------------------------------------------------------------


view : Int -> Int -> Chart.Data -> Svg msg
view w h chartData =
    let
        firstDataTuple =
            List.head chartData.data |> Maybe.withDefault []

        indexedData =
            Array.toIndexedList (Array.fromList chartData.data)

        totalRows =
            List.length indexedData

        padding =
            defaultChartPadding

        xTicksCount =
            5

        xGridTicks =
            Scale.ticks (xScale w chartData.maxValue padding) xTicksCount
    in
        svg
            [ Svg.Attributes.width (toString w ++ "px")
            , Svg.Attributes.height (toString h ++ "px")
            ]
            (List.concat
                [ [ renderYAxis w h firstDataTuple padding
                  , renderXAxis w h chartData.maxValue padding
                    -- , Utils.renderDebugGrid w h padding
                  , Utils.renderXGrid w
                        h
                        padding
                        chartData.maxValue
                        (xScale w chartData.maxValue padding)
                        xGridTicks
                  ]
                , renderColumns w h chartData.maxValue totalRows indexedData padding
                , renderLegend w h chartData.seriesLabels
                , [ ChartAxisLabels.renderXAxisLabel w h chartData.xAxisLabel padding ]
                , [ ChartAxisLabels.renderLeftYAxisLabel h chartData.yAxisLabel padding ]
                ]
            )


yScale : Int -> List ( String, String ) -> ChartPadding -> BandScale String
yScale width data chartPadding =
    Scale.band
        { defaultBandConfig | align = 0.5 }
        (List.map Tuple.first data)
        ( toFloat width - chartPadding.totalVertical, 0 )


xScale : Int -> Float -> ChartPadding -> ContinuousScale
xScale width maxValue chartPadding =
    Scale.linear ( 0, maxValue ) ( 0, toFloat width - chartPadding.totalHorizontal )


yAxis : Int -> List ( String, String ) -> ChartPadding -> Svg msg
yAxis width data chartPadding =
    Axis.axis
        { defaultOptions
            | orientation = Axis.Left
            , tickFormat = Just Utils.formatStringTick
        }
        (Scale.toRenderable (yScale width data chartPadding))


xAxis : Int -> Float -> ChartPadding -> Svg msg
xAxis width maxValue chartPadding =
    Axis.axis
        { defaultOptions
            | orientation = Axis.Bottom
            , tickFormat = Just Utils.formatNumberTick
        }
        (xScale width maxValue chartPadding)


column :
    Int
    -> Int
    -> Int
    -> String
    -> BandScale String
    -> ChartPadding
    -> Float
    -> ( String, String )
    -> Svg msg
column width index totalRows colour yScaleBand padding maxValue ( header, value ) =
    let
        ypos =
            (Scale.convert yScaleBand header) + (colHeight * (toFloat index))

        valueSantized =
            NumberParser.fromString value

        xpos =
            0

        colWidth =
            Scale.convert (xScale width maxValue padding) valueSantized

        colHeight =
            Scale.bandwidth yScaleBand / toFloat (totalRows)

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


barColours : Array Color.Color
barColours =
    Array.fromList Scale.category10


getBarColour : Int -> String
getBarColour index =
    get index barColours
        |> Maybe.withDefault Color.black
        |> Color.Convert.colorToHex
