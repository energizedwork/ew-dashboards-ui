module Views.Widget.Renderers.Table exposing (render)

import Data.Widget as Widget exposing (Widget, Body)
import Data.Widget.Table as Table exposing (Data, Cell)
import Data.Widget.Adapters.Adapter exposing (Adapter(..))
import Data.Widget.Adapters.TableAdapter as TableAdapter
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src, title, style)
import Views.Widget.Renderers.Utils as Utils


render : Int -> Int -> Widget Body -> Table.Data -> Html msg
render width height widget data =
    case widget.adapter of
        TABLE optionalConfig ->
            let
                ( headerRow, bodyRows, minValue, maxValue, xLabels ) =
                    TableAdapter.adapt optionalConfig data
            in
                div
                    [ class <| "col-md-12 widget" ]
                    [ h3 [ title widget.description, class "heading" ] [ Html.text widget.name ]
                    , div
                        [ style
                            [ ( "maxWidth", ((toString <| width - floor padding * 2) ++ "px") )
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
                        , Utils.renderDataSourceInfoFrom widget
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
