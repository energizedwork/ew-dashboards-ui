module Views.Widget.Renderers.Table exposing (render)

import Data.Widget as Widget exposing (Body, Widget)
import Data.Widget.Adapters.Adapter exposing (Adapter(..))
import Data.Widget.Adapters.TableAdapter as TableAdapter exposing (Orientation(..))
import Data.Widget.Config as RendererConfig
import Data.Widget.Table as Table exposing (Cell, Data)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src, style, title)
import Views.Spinner
import Views.Widget.Renderers.Config as ViewConfig
import Views.Widget.Renderers.Utils as Utils


render : RendererConfig.Config -> Int -> Int -> Widget -> Table.Data -> Html msg
render optionalRendererConfig width height widget data =
    case widget.adapter of
        TABLE optionalAdapterConfig ->
            let
                vTable =
                    TableAdapter.adapt optionalAdapterConfig data Vertical

                calculatedHeight =
                    ViewConfig.calculateHeight optionalRendererConfig height

                body =
                    div []
                        [ Utils.renderTitleFrom widget
                        , div
                            [ style
                                [ ( "maxWidth", ((toString <| width - floor padding * 2) ++ "px") )
                                , ( "minHeight", ((toString <| calculatedHeight) ++ "px") )
                                , ( "overflow", "auto" )
                                ]
                            ]
                            [ table
                                [ class "table table-striped" ]
                                [ thead []
                                    [ renderHeaderFrom <| Maybe.withDefault [] vTable.xLabels
                                    ]
                                , tbody [] <| renderBodyFrom vTable.rows
                                ]
                            ]
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
            p [ class "data" ] [ text "Sorry, I can only render tables from a TABLE adapter right now" ]


padding : Float
padding =
    ViewConfig.mediumPadding


renderHeaderFrom : List String -> Html msg
renderHeaderFrom row =
    tr [] (List.map (\cell -> th [] [ text cell ]) row)


renderBodyFrom : List (List String) -> List (Html msg)
renderBodyFrom rows =
    List.map (\row -> tr [] (renderCells row)) rows


renderCells : List String -> List (Html msg)
renderCells row =
    List.map (\cell -> td [] [ text cell ]) row
