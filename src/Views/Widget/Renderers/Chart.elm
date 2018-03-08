module Views.Widget.Renderers.Chart exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Views.Widget.Renderers.Config exposing (ChartPadding)


type alias Dimensions =
    { w : Int
    , h : Int
    }


calculateDimensions : Int -> Int -> Maybe ChartPadding -> Dimensions
calculateDimensions w h chartPadding =
    let
        ( totalHorizontal, totalVertical ) =
            case chartPadding of
                Just padding ->
                    ( padding.totalHorizontal, padding.totalVertical )

                Nothing ->
                    ( 0, 0 )
    in
        Dimensions
            (w - floor totalHorizontal)
            (h - floor totalVertical)


renderClipPaths : String -> Dimensions -> Dimensions -> Svg msg
renderClipPaths namespace actualsDimensions forecastsDimensions =
    let
        leftRegionRender =
            Svg.clipPath [ id <| namespace ++ "-left-region" ]
                [ Svg.rect
                    [ width <| toString <| actualsDimensions.w
                    , height <| toString <| actualsDimensions.h
                    ]
                    []
                ]

        rightRegionRender =
            Svg.clipPath [ id <| namespace ++ "-right-region" ]
                [ Svg.rect
                    [ width <| toString <| forecastsDimensions.w
                    , height <| toString <| forecastsDimensions.h
                    , x <| toString <| actualsDimensions.w
                    ]
                    []
                ]
    in
        defs []
            [ leftRegionRender
            , rightRegionRender
            ]
