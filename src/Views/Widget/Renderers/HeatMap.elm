module Views.Widget.Renderers.HeatMap exposing (render, colourScaleFrom)

import Array exposing (..)
import Color.Convert
import Data.Widget as Widget exposing (Widget, Body)
import Data.Widget.Table as Table exposing (Data, Cell)
import Data.Widget.Adapters.Adapter exposing (Adapter(..))
import Data.Widget.Adapters.HeatMapAdapter as HeatMapAdapter
import Html exposing (..)
import Html.Attributes exposing (title)
import Html.Events exposing (onSubmit, onInput)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.Attributes exposing (attribute, placeholder, defaultValue)
import Views.Widget.Renderers.RendererMessage as RendererMessage exposing (Msg(..))
import Views.Widget.Renderers.Utils as Utils exposing (..)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)


type alias Model =
    { screenWidth : Float
    , screenHeight : Float
    , data : List (List ( Cell, Cell ))
    , xLabels : List String
    , yLabels : List String
    , maxValue : Float
    }


render : Int -> Int -> Widget Body -> Table.Data -> Bool -> Html RendererMessage.Msg
render width height widget data updatable =
    case widget.adapter of
        HEAT_MAP ->
            let
                ( headerRow, bodyRows, maxValue, xLabels, yLabels ) =
                    HeatMapAdapter.adapt data

                dataAsHeaderValueTuples =
                    List.map (List.map2 (,) headerRow) bodyRows

                initWidth =
                    case width of
                        0 ->
                            Utils.largeWidth

                        _ ->
                            (width |> toFloat) - (padding * 2)

                initHeight =
                    Utils.largeHeight

                primaryDataSource =
                    (Widget.primaryDataSource widget)

                renderOptionalInputField =
                    case updatable of
                        True ->
                            div []
                                [ Html.form
                                    [ Html.Attributes.class "hm-form"
                                    , onSubmit <| UpdateDataSource (Widget.slugToString widget.uuid) primaryDataSource.uuid primaryDataSource.name
                                    ]
                                    [ label [] [ Html.text "Current data source id: " ]
                                    , input
                                        [ (defaultValue) primaryDataSource.uuid
                                        , placeholder "Enter a data source uuid"
                                        , onInput (\x -> SetDataSourceUUID (x))
                                        ]
                                        []
                                    , button []
                                        [ Html.text "Update"
                                        ]
                                    ]
                                ]

                        False ->
                            div [] []

                initModel =
                    Model initWidth initHeight dataAsHeaderValueTuples xLabels yLabels maxValue
            in
                div [ class "col-md-12 widget" ]
                    [ div [ class "row" ]
                        [ div [ class "col-md-5" ]
                            [ h3
                                [ Html.Attributes.title widget.description
                                , Html.Attributes.class "heading"
                                , Html.Attributes.style [ ( "marginLeft", ((toString padding) ++ "px") ) ]
                                ]
                                [ Html.text widget.name ]
                            ]
                        , div [ class "col-md-5 col-md-offset-2" ]
                            [ renderOptionalInputField
                            ]
                        ]
                    , draw initModel
                    , Utils.renderDataSourceInfoFrom widget
                    ]

        _ ->
            p [ class "data" ] [ Html.text "Sorry, I can only render line charts from a HEAT_MAP / UPDATABLE_HEAT_MAP adapter right now" ]


w : Float
w =
    Utils.largeWidth


h : Float
h =
    Utils.largeHeight


padding : Float
padding =
    Utils.largePadding


getCellColour : Float -> String
getCellColour index =
    Scale.infernoInterpolator index
        |> Color.Convert.colorToHex


colourScale : Float -> ContinuousScale
colourScale maxValue =
    Scale.linear ( 0, maxValue ) ( 0, 1 )


colourScaleFrom : String -> Float -> Float
colourScaleFrom amount maxValue =
    Scale.convert
        (colourScale maxValue)
        (String.toFloat amount |> Result.withDefault 0)


draw : Model -> Svg msg
draw model =
    let
        maxValue =
            model.maxValue

        firstDataTuple =
            List.head model.data |> Maybe.withDefault []

        indexedData =
            Array.toIndexedList (Array.fromList model.data)

        totalRows =
            List.length indexedData

        totalCols =
            List.length firstDataTuple

        viewPortWidth : Float
        viewPortWidth =
            model.screenWidth - (padding)

        viewPortHeight : Float
        viewPortHeight =
            model.screenHeight - (padding)

        heatMapCellWidth =
            (viewPortWidth / toFloat totalCols)

        heatMapCellHeight =
            (viewPortHeight / toFloat totalRows)

        legendCellWidth =
            (viewPortWidth / maxValue)

        legendCellHeight =
            (viewPortHeight / toFloat 50)

        valueRange =
            (List.range 0 <| (String.toInt (toString maxValue) |> Result.withDefault 0))

        xScale : ContinuousScale
        xScale =
            Scale.linear ( 0, List.maximum (Utils.rowToFloats model.xLabels) |> Maybe.withDefault 0 ) ( 0, viewPortWidth )

        legendXScale : ContinuousScale
        legendXScale =
            Scale.linear ( 0, maxValue ) ( 0, viewPortWidth )

        yScale : BandScale String
        yScale =
            (Scale.band
                { defaultBandConfig | paddingInner = 0, paddingOuter = 0 }
                (model.yLabels)
                ( 0, viewPortHeight )
            )

        opts : Axis.Options a
        opts =
            Axis.defaultOptions

        xAxis : Svg msg
        xAxis =
            Axis.axis { opts | orientation = Axis.Bottom, tickCount = 20 } (xScale)

        legendXAxis : Svg msg
        legendXAxis =
            Axis.axis { opts | orientation = Axis.Bottom, tickCount = 5 } (legendXScale)

        yAxis : Svg msg
        yAxis =
            Axis.axis { opts | orientation = Axis.Left, tickCount = 10, tickFormat = Just Utils.formatStringTick } (Scale.toRenderable yScale)

        fillAt colIndex cell =
            case colIndex of
                -- 0 ->
                --     "#fff"
                _ ->
                    (getCellColour (colourScaleFrom cell maxValue))

        renderSquare rowIndex colIndex cell cellWidth cellHeight renderLabels =
            let
                renderShape =
                    rect
                        [ x <| toString (padding + toFloat colIndex * cellWidth)
                        , y <| toString (toFloat rowIndex * cellHeight)
                        , width <| toString cellWidth
                        , height <| toString cellHeight
                        , fill <| fillAt colIndex cell
                        , stroke <| fillAt colIndex cell
                        , attribute "data-amount" cell
                        ]
                        []

                renderText =
                    case renderLabels of
                        True ->
                            text_
                                [ x <| toString <| (padding + toFloat colIndex * cellWidth) + (cellWidth / 2)
                                , y <| toString <| (toFloat rowIndex * cellHeight) + (cellHeight / 2)
                                , textAnchor "middle"
                                , alignmentBaseline "central"
                                , stroke "none"
                                , fill "#fff"
                                , fontSize "8px"
                                ]
                                [ Svg.text <| cell ]

                        False ->
                            text_ [] []
            in
                g [ class "square" ]
                    [ renderShape
                    , renderText
                    ]

        renderSquares =
            let
                renderLabels =
                    True
            in
                List.map
                    (\( rowIndex, row ) ->
                        g [ class "squares" ]
                            (List.map
                                (\( colIndex, ( header, cell ) ) ->
                                    renderSquare rowIndex colIndex cell heatMapCellWidth heatMapCellHeight renderLabels
                                )
                                (Array.toIndexedList (Array.fromList row))
                            )
                    )
                    indexedData

        renderLegend =
            let
                renderLabels =
                    False

                rowIndex =
                    0
            in
                List.map
                    (\index ->
                        renderSquare rowIndex index (toString index) legendCellWidth legendCellHeight renderLabels
                    )
                    valueRange
    in
        div []
            [ svg [ width (toString model.screenWidth ++ "px"), height (toString model.screenHeight ++ "px") ]
                (List.concat
                    [ [ Svg.style []
                            [ Svg.text """
                            .square text { display: none; }
                            .square:hover rect { opacity: 0.7; }
                            .square:hover text { display: inline; }
                          """ ]
                      , g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString (h - padding) ++ ")") ]
                            [ xAxis ]
                      , g [ transform ("translate(" ++ toString (padding - 1) ++ ",0)") ]
                            [ yAxis ]
                      ]
                    , renderSquares
                    ]
                )
            , h5
                [ Html.Attributes.style [ ( "marginLeft", ((toString padding) ++ "px") ) ]
                ]
                [ Html.text "Scale" ]
            , svg [ width (toString model.screenWidth ++ "px"), height (toString (legendCellHeight + 50) ++ "px") ]
                (List.concat
                    [ [ Svg.style [] [] ]
                    , [ g [ class "legend" ] renderLegend ]
                    , [ g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString (legendCellHeight) ++ ")") ] [ legendXAxis ] ]
                    ]
                )
            ]
