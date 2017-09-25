module Views.Widget exposing (view, viewTimestamp)

{-| Viewing a preview of an individual widget, excluding its body.
-}

import Data.Widget as Widget exposing (Widget)
import Data.UserPhoto as UserPhoto exposing (UserPhoto)
import Date.Format
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src)
import Route exposing (Route)
import Views.DataSource as DataSource exposing (view)
import Views.Widget.Favorite as Favorite


-- VIEWS --


{-| Some pages want to view just the timestamp, not the whole widget.
-}
viewTimestamp : Widget a -> Html msg
viewTimestamp widget =
    span [ class "date" ] [ text (formattedTimestamp widget) ]


view : (Widget a -> msg) -> Widget a -> Html msg
view toggleFavorite widget =
    let
        author =
            widget.author
    in
        div [ class "article-preview" ]
            [ div [ class "article-meta" ]
                [ a [ Route.href (Route.Profile author.username) ]
                    [ img [ UserPhoto.src author.image ] [] ]
                , div [ class "info" ]
                    [ span [ class "date" ] [ text (formattedTimestamp widget) ]
                    ]
                , Favorite.button
                    toggleFavorite
                    widget
                    [ class "pull-xs-right" ]
                    [ text (" " ++ toString widget.favoritesCount) ]
                ]
            , a [ class "preview-link", Route.href (Route.Widget widget.uuid) ]
                [ h1 [] [ text widget.name ]
                , p [] [ text widget.description ]
                , ul [] <| List.map DataSource.view widget.dataSources
                , span [] [ text "Click to run widget.." ]
                ]
            ]



-- INTERNAL --


formattedTimestamp : Widget a -> String
formattedTimestamp widget =
    Date.Format.format "%B %e, %Y" widget.createdAt
