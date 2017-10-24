module Views.Widget.Renderers.Utils
    exposing
        ( mediumWidth
        , mediumHeight
        , mediumPadding
        , largeWidth
        , largeHeight
        , largePadding
        , renderDataSourceInfoFrom
        , rowToFloats
        , formatStringTick
        )

import Data.DataSource as DataSource
import Data.Widget as Widget exposing (Widget, Body)
import Html exposing (..)
import Html.Attributes exposing (class)


mediumWidth : Float
mediumWidth =
    740


mediumHeight : Float
mediumHeight =
    370


mediumPadding : Float
mediumPadding =
    50


largeWidth : Float
largeWidth =
    1320


largeHeight : Float
largeHeight =
    880


largePadding : Float
largePadding =
    50


renderDataSourceInfoFrom : Widget a -> Html msg
renderDataSourceInfoFrom widget =
    p [ class "small data-source-info" ] [ text <| DataSource.toChannel <| Widget.primaryDataSource widget ]


rowToFloats : List String -> List Float
rowToFloats row =
    List.map (\n -> String.toFloat n |> Result.withDefault 0) row


formatStringTick : String -> String
formatStringTick tick =
    tick
