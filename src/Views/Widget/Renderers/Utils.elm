module Views.Widget.Renderers.Utils
    exposing
        ( cssSafe
        , rowToFloats
        , renderTitleFrom
        , renderDebugGrid
        , renderYGrid
        , formatStringTick
        , toStr
        , formatNumberTick
        )

import Data.DataSource as DataSource
import Data.Widget as Widget exposing (Body, Widget)
import Html exposing (..)
import Html.Attributes exposing (class)
import Regex exposing (..)
import String exposing (..)
import String.Extra exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Views.Widget.Renderers.Config as ViewConfig exposing (ChartPadding)
import Visualization.Scale as Scale exposing (ContinuousScale)


renderDataSourceInfoFrom : Widget -> Html msg
renderDataSourceInfoFrom widget =
    p [ Html.Attributes.class "small data-source-info" ] [ Html.text <| DataSource.toChannel <| Widget.primaryDataSource widget ]


yGridLine : Int -> Int -> Float -> ContinuousScale -> Int -> Float -> Svg msg
yGridLine w h padding scale index tick =
    let
        yPos =
            toString <| Scale.convert scale tick
    in
        line
            [ x1 "0"
            , y1 yPos
            , x2 <| toString (Basics.toFloat w - padding)
            , y2 yPos
            , stroke "#ccc"
            , strokeWidth "1"
              -- , strokeWidth (toString (Basics.max (toFloat (index % 2)) 0.5))
            ]
            []


renderYGrid : Int -> Int -> ChartPadding -> Float -> ContinuousScale -> List Float -> Svg msg
renderYGrid w h chartPadding maxValue scale ticks =
    g [ transform ("translate(" ++ toString (chartPadding.left - 1) ++ ", " ++ toString (chartPadding.top) ++ ")") ] <|
        List.indexedMap (yGridLine w h chartPadding.totalHorizontal scale) ticks


renderDebugGrid : Int -> Int -> ChartPadding -> Svg msg
renderDebugGrid w h padding =
    let
        gridSize =
            10

        wScale =
            Scale.linear
                ( 0, Basics.toFloat w )
                ( 0, Basics.toFloat w )

        hScale =
            Scale.linear
                ( 0, Basics.toFloat h )
                ( 0, Basics.toFloat h )

        wTicks =
            Scale.ticks wScale (w // gridSize)

        hTicks =
            Scale.ticks hScale (h // gridSize)

        drawWline index tick =
            line
                [ x1 <| toString <| tick
                , y1 "0"
                , x2 <| toString <| tick
                , y2 <| toString <| h
                , stroke "#ccc"
                , calcuateStrokeWidthFrom index
                ]
                []

        drawHline index tick =
            line
                [ x1 "0"
                , y1 <| toString <| tick
                , x2 <| toString <| w
                , y2 <| toString <| tick
                , stroke "#ccc"
                , calcuateStrokeWidthFrom index
                ]
                []

        calcuateStrokeWidthFrom index =
            strokeWidth (toString (Basics.max (Basics.toFloat (index % 2)) 0.8))
    in
        g []
            [ g [] <| List.indexedMap drawWline wTicks
            , g [] <| List.indexedMap drawHline hTicks
            ]


createDataSourceHoverTitleFrom : Widget -> String
createDataSourceHoverTitleFrom widget =
    widget.description ++ ": \n\n" ++ (DataSource.toChannel <| Widget.primaryDataSource widget)


renderTitleFrom : Widget -> Html msg
renderTitleFrom widget =
    h3
        [ Html.Attributes.title <| createDataSourceHoverTitleFrom widget
        , Html.Attributes.class "heading"
        ]
        [ span [ Html.Attributes.class "ion-information-circled data-source-info" ] []
        , span [] [ Html.text widget.name ]
        ]


rowToFloats : List String -> List Float
rowToFloats row =
    List.map (\n -> String.toFloat n |> Result.withDefault 0) row


formatStringTick : a -> String
formatStringTick tick =
    toStr tick


formatNumberTick : a -> String
formatNumberTick tick =
    case (String.toFloat (toStr tick)) of
        Ok floatTick ->
            if floatTick >= 1000000 || floatTick <= -1000000 then
                toStr (floatTick / 1000000) ++ "m"
            else if floatTick >= 1000 || floatTick <= -1000 then
                toStr (floatTick / 1000) ++ "k"
            else
                toStr floatTick

        Err err ->
            toStr tick



--https://github.com/elm-lang/core/issues/657


toStr : a -> String
toStr v =
    let
        str =
            toString v
    in
        if left 1 str == "\"" then
            dropRight 1 (dropLeft 1 str)
        else
            str


cssSafe : String -> String
cssSafe input =
    input
        |> String.Extra.dasherize
        |> String.Extra.clean
        |> Regex.replace All (regex "[&#%.',\"\\s]+") (always "")
        |> (++) "d-"
