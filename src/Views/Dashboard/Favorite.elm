module Views.Dashboard.Favorite exposing (button)

{-| TODO: DRY me with Widget.Favourite
-}

import Data.Dashboard as Dashboard exposing (Dashboard)
import Html exposing (Attribute, Html, i, text)
import Html.Attributes exposing (class)
import Util exposing ((=>), onClickStopPropagation)


{-| This is a "build your own element" API.

You pass it some configuration, followed by a `List (Attribute msg)` and a
`List (Html msg)`, just like any standard Html element.

-}
button :
    (Dashboard -> msg)
    -> Dashboard
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
button toggleFavorite dashboard extraAttributes extraChildren =
    let
        favoriteButtonClass =
            if dashboard.favorited then
                "btn-primary"
            else
                "btn-outline-primary"

        attributes =
            [ class ("btn btn-sm " ++ favoriteButtonClass)
            , onClickStopPropagation (toggleFavorite dashboard)
            ]
                ++ extraAttributes

        children =
            [ i [ class "ion-heart" ] [] ]
                ++ extraChildren
    in
        Html.button attributes children
