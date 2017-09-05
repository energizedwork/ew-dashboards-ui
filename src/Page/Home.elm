module Page.Home exposing (Model, Msg, init, update, view)

{-| The homepage. You can get here via either the / or /#/ routes.
-}

import Data.Widget as Widget exposing (Tag)
import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder)
import Html.Events exposing (onClick)
import Http
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Request.Widget
import SelectList exposing (SelectList)
import Task exposing (Task)
import Util exposing ((=>), onClickStopPropagation)
import Views.Widget.Feed as Feed exposing (FeedSource, globalFeed, tagFeed, yourFeed)
import Views.Page as Page


-- MODEL --


type alias Model =
    { tags : List Tag
    , feed : Feed.Model
    }


init : Session -> Task PageLoadError Model
init session =
    let
        feedSources =
            if session.user == Nothing then
                SelectList.singleton globalFeed
            else
                SelectList.fromLists [] yourFeed [ globalFeed ]

        loadTags =
            Request.Widget.tags
                |> Http.toTask

        loadSources =
            Feed.init session feedSources

        handleLoadError _ =
            pageLoadError Page.Home "Homepage is currently unavailable."
    in
        Task.map2 Model loadTags loadSources
            |> Task.mapError handleLoadError



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    div [ class "home-page" ]
        [ viewBanner
        , div [ class "container page" ]
            [ div [ class "row" ]
                [ div [ class "col-md-12" ] (viewFeed model.feed)
                , div [ class "col-md-3" ] []
                  -- [ div [ class "sidebar" ]
                  --     [ p [] [ text "Popular Tags" ]
                  --     , viewTags model.tags
                  --     ]
                  -- ]
                ]
            ]
        ]


viewBanner : Html msg
viewBanner =
    div [ class "banner" ]
        [ div [ class "container" ]
            [ h1 [ class "logo-font" ] [ text "Widgets" ]
            ]
        ]


viewFeed : Feed.Model -> List (Html Msg)
viewFeed feed =
    div [ class "feed-toggle" ]
        [ Feed.viewFeedSources feed |> Html.map FeedMsg ]
        :: (Feed.viewWidgets feed |> List.map (Html.map FeedMsg))


viewTags : List Tag -> Html Msg
viewTags tags =
    div [ class "tag-list" ] (List.map viewTag tags)


viewTag : Tag -> Html Msg
viewTag tagName =
    a
        [ class "tag-pill tag-default"
        , href "javascript:void(0)"
        , onClick (SelectTag tagName)
        ]
        [ text (Widget.tagToString tagName) ]



-- UPDATE --


type Msg
    = FeedMsg Feed.Msg
    | SelectTag Tag


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        FeedMsg subMsg ->
            let
                ( newFeed, subCmd ) =
                    Feed.update session subMsg model.feed
            in
                { model | feed = newFeed } => Cmd.map FeedMsg subCmd

        SelectTag tagName ->
            let
                subCmd =
                    Feed.selectTag (Maybe.map .token session.user) tagName
            in
                model => Cmd.map FeedMsg subCmd
