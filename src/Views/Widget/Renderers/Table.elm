module Views.Widget.Renderers.Table exposing (render)

import Data.Widget as Widget exposing (Body, Widget)
import Data.Widget.Adapters.Adapter exposing (Adapter(..))
import Data.Widget.Adapters.TableAdapter as TableAdapter
import Data.Widget.Config as RendererConfig
import Data.Widget.Table as Table exposing (Cell, Data)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src, style, title)
import Views.Widget.Renderers.Config as ViewConfig
import Views.Widget.Renderers.Utils as Utils


render : RendererConfig.Config -> Int -> Int -> Widget -> Table.Data -> Html msg
render optionalRendererConfig width height widget data =
    case widget.adapter of
        TABLE optionalAdapterConfig ->
            let
                ( headerRow, bodyRows, minValue, maxValue, xLabels ) =
                    TableAdapter.adapt optionalAdapterConfig data

                calculatedHeight =
                    ViewConfig.calculateHeight optionalRendererConfig height
            in
                div
                    [ class <|
                        ViewConfig.colSpanClass optionalRendererConfig
                            ++ " widget"
                    ]
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
                                [ renderHeaderFrom headerRow
                                ]
                            , tbody [] <| renderBodyFrom bodyRows
                            ]
                        ]
                    ]

        _ ->
            p [ class "data" ] [ text "Sorry, I can only render tables from a TABLE adapter right now" ]


padding : Float
padding =
    Utils.largePadding


renderHeaderFrom : List String -> Html msg
renderHeaderFrom row =
    tr [] (List.map (\cell -> th [] [ text cell ]) row)


renderBodyFrom : List (List String) -> List (Html msg)
renderBodyFrom rows =
    List.map (\row -> tr [] (renderCells row)) rows


renderCells : List String -> List (Html msg)
renderCells row =
    List.map (\cell -> td [] [ text cell ]) row
