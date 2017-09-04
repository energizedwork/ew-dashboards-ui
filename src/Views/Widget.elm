module Views.Widget exposing (view, viewTimestamp)

{-| Viewing a preview of an individual article, excluding its body.
-}

import Data.Widget as Widget exposing (Widget)
import Data.UserPhoto as UserPhoto exposing (UserPhoto)
import Date.Format
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src)
import Route exposing (Route)
import Views.Widget.Favorite as Favorite
import Views.Author


-- VIEWS --


{-| Some pages want to view just the timestamp, not the whole article.
-}
viewTimestamp : Widget a -> Html msg
viewTimestamp article =
    span [ class "date" ] [ text (formattedTimestamp article) ]


view : (Widget a -> msg) -> Widget a -> Html msg
view toggleFavorite article =
    let
        author =
            article.author
    in
    div [ class "article-preview" ]
        [ div [ class "article-meta" ]
            [ a [ Route.href (Route.Profile author.username) ]
                [ img [ UserPhoto.src author.image ] [] ]
            , div [ class "info" ]
                [ Views.Author.view author.username
                , span [ class "date" ] [ text (formattedTimestamp article) ]
                ]
            , Favorite.button
                toggleFavorite
                article
                [ class "pull-xs-right" ]
                [ text (" " ++ toString article.favoritesCount) ]
            ]
        , a [ class "preview-link", Route.href (Route.Widget article.slug) ]
            [ h1 [] [ text article.title ]
            , p [] [ text article.description ]
            , span [] [ text "Read more..." ]
            ]
        ]



-- INTERNAL --


formattedTimestamp : Widget a -> String
formattedTimestamp article =
    Date.Format.format "%B %e, %Y" article.createdAt
