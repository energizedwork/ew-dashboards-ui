module Request.Dashboard
    exposing
        ( FeedConfig
        , ListConfig
        , defaultFeedConfig
        , defaultListConfig
        , feed
        , get
        , list
        , tags
        , toggleFavorite
        )

import Data.AuthToken as AuthToken exposing (AuthToken, withAuthorization)
import Data.Dashboard as Dashboard exposing (..)
import Data.UUID as UUID
import Data.User as User exposing (Username)
import Data.Widget as Widget exposing (Body, Tag, Widget)
import Data.Widget.Feed as Feed exposing (Feed)
import Http
import HttpBuilder exposing (RequestBuilder, withBody, withExpect, withQueryParams)
import Json.Decode as Decode
import Request.Helpers exposing (mockApiUrl)
import Util exposing ((=>))


-- SINGLE --


get : Maybe AuthToken -> UUID.UUID -> Http.Request Dashboard
get maybeToken slug =
    let
        expect =
            Dashboard.decoder
                |> Decode.field "dashboard"
                |> Http.expectJson
    in
        mockApiUrl ("/dashboards/" ++ UUID.slugToString slug)
            |> HttpBuilder.get
            |> HttpBuilder.withExpect expect
            |> withAuthorization maybeToken
            |> HttpBuilder.toRequest



-- LIST --


type alias ListConfig =
    { tag : Maybe Tag
    , author : Maybe Username
    , favorited : Maybe Username
    , limit : Int
    , offset : Int
    }


defaultListConfig : ListConfig
defaultListConfig =
    { tag = Nothing
    , author = Nothing
    , favorited = Nothing
    , limit = 20
    , offset = 0
    }


list : ListConfig -> Maybe AuthToken -> Http.Request Feed
list config maybeToken =
    [ "tag" => Maybe.map Widget.tagToString config.tag
    , "author" => Maybe.map User.usernameToString config.author
    , "favorited" => Maybe.map User.usernameToString config.favorited
    , "limit" => Just (toString config.limit)
    , "offset" => Just (toString config.offset)
    ]
        |> List.filterMap maybeVal
        |> buildFromQueryParams "/dashboards"
        |> withAuthorization maybeToken
        |> HttpBuilder.toRequest



-- FEED --


type alias FeedConfig =
    { limit : Int
    , offset : Int
    }


defaultFeedConfig : FeedConfig
defaultFeedConfig =
    { limit = 10
    , offset = 0
    }


feed : FeedConfig -> AuthToken -> Http.Request Feed
feed config token =
    [ "limit" => Just (toString config.limit)
    , "offset" => Just (toString config.offset)
    ]
        |> List.filterMap maybeVal
        |> buildFromQueryParams "/widgets/feed"
        |> withAuthorization (Just token)
        |> HttpBuilder.toRequest



-- TAGS --


tags : Http.Request (List Tag)
tags =
    Decode.field "tags" (Decode.list Widget.tagDecoder)
        |> Http.get (mockApiUrl "/tags")



-- FAVORITE --


toggleFavorite : Dashboard -> AuthToken -> Http.Request Dashboard
toggleFavorite dashboard authToken =
    if dashboard.favorited then
        unfavorite dashboard.uuid authToken
    else
        favorite dashboard.uuid authToken


favorite : UUID.UUID -> AuthToken -> Http.Request Dashboard
favorite =
    buildFavorite HttpBuilder.post


unfavorite : UUID.UUID -> AuthToken -> Http.Request Dashboard
unfavorite =
    buildFavorite HttpBuilder.delete


buildFavorite :
    (String -> RequestBuilder a)
    -> UUID.UUID
    -> AuthToken
    -> Http.Request Dashboard
buildFavorite builderFromUrl slug token =
    let
        expect =
            Dashboard.decoder
                |> Decode.field "dashboard"
                |> Http.expectJson
    in
        [ mockApiUrl "/dashboards", UUID.slugToString slug, "favorite" ]
            |> String.join "/"
            |> builderFromUrl
            |> withAuthorization (Just token)
            |> withExpect expect
            |> HttpBuilder.toRequest



-- HELPERS --


maybeVal : ( a, Maybe b ) -> Maybe ( a, b )
maybeVal ( key, value ) =
    case value of
        Nothing ->
            Nothing

        Just val ->
            Just (key => val)


buildFromQueryParams : String -> List ( String, String ) -> RequestBuilder Feed
buildFromQueryParams url queryParams =
    url
        |> mockApiUrl
        |> HttpBuilder.get
        |> withExpect (Http.expectJson Feed.decoder)
        |> withQueryParams queryParams
