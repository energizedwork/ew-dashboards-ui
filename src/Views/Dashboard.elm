module Views.Dashboard exposing (view, viewTimestamp)

import Data.Dashboard as Dashboard exposing (Dashboard)
import Data.UserPhoto as UserPhoto exposing (UserPhoto)
import Date.Format
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src)
import Route exposing (Route)
import Views.DataSource as DataSource exposing (view)
import Views.Dashboard.Favorite as Favorite


-- VIEWS --


viewTimestamp : Dashboard -> Html msg
viewTimestamp dashboard =
    span [ class "date" ] [ text (formattedTimestamp dashboard) ]


view : (Dashboard -> msg) -> Dashboard -> Html msg
view toggleFavorite dashboard =
    let
        author =
            dashboard.author
    in
        div [ class "article-preview" ]
            [ div [ class "article-meta" ]
                [ a [ Route.href (Route.Profile author.username) ]
                    [ img [ UserPhoto.src author.image ] [] ]
                , div [ class "info" ]
                    [ span [ class "date" ] [ text (formattedTimestamp dashboard) ]
                    ]
                , Favorite.button
                    toggleFavorite
                    dashboard
                    [ class "pull-xs-right" ]
                    [ text (" " ++ toString dashboard.favoritesCount) ]
                ]
            , a [ class "preview-link", Route.href (Route.Dashboard dashboard.uuid) ]
                [ h1 [] [ text dashboard.name ]
                , p [] [ text dashboard.description ]
                ]
            ]



-- INTERNAL --


formattedTimestamp : Dashboard -> String
formattedTimestamp dashboard =
    Date.Format.format "%B %e, %Y" dashboard.createdAt
