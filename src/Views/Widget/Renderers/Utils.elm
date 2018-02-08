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
        , toStr
        )

import Data.DataSource as DataSource
import Data.Widget as Widget exposing (Body, Widget)
import Html exposing (..)
import Html.Attributes exposing (class)
import String exposing (..)


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


renderDataSourceInfoFrom : Widget -> Html msg
renderDataSourceInfoFrom widget =
    p [ class "small data-source-info" ] [ text <| DataSource.toChannel <| Widget.primaryDataSource widget ]


rowToFloats : List String -> List Float
rowToFloats row =
    List.map (\n -> String.toFloat n |> Result.withDefault 0) row


formatStringTick : a -> String
formatStringTick tick =
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
