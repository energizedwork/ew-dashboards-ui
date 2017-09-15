module Page.Widget exposing (Model, Msg, init, update, view, subscriptions)

{-| Viewing an individual article.
-}

import Data.Widget as Widget exposing (Widget, Body)
import Data.Widget.Author as Author exposing (Author)
import Data.Widget.Comment as Comment exposing (Comment, CommentId)
import Data.Session as Session exposing (Session)
import Data.User as User exposing (User)
import Data.UserPhoto as UserPhoto
import Data.DataSourceMessage as DataSourceMessage exposing (DataSourceMessage, decoder)
import Date exposing (Date)
import Date.Format
import Html exposing (..)
import Html.Attributes exposing (attribute, class, disabled, href, id, placeholder, value, type_)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Request.Widget
import Request.Widget.Comments
import Request.Profile
import Route
import Task exposing (Task)
import Util exposing ((=>), pair, viewIf)
import Views.Widget
import Views.Widget.Favorite as Favorite
import Views.Author
import Views.Errors
import Views.Page as Page
import Views.User.Follow as Follow
import Data.Widget.Table as Table exposing (Data, Cell, decoder)
import Views.Widget.Renderer as Renderer
import Json.Encode as JE
import Json.Decode as JD exposing (field)
import Phoenix.Socket
import Phoenix.Channel
import Phoenix.Push
import Dict


-- CONSTANTS


socketServer : String
socketServer =
    "ws://localhost:4000/socket/websocket"



-- "wss://phoenixchat.herokuapp.com/ws"


channelName : String
channelName =
    "dataSource:sheets"



-- MODEL --


type alias Model =
    { errors : List String
    , newMessage : String
    , messages : List String
    , phxSocket : Phoenix.Socket.Socket Msg
    , article : Widget Body
    , data : Data
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Phoenix.Socket.listen model.phxSocket PhoenixMsg


user : String
user =
    "msp"


userParams : JE.Value
userParams =
    JE.object [ ( "user_id", JE.string user ) ]


init : Session -> Widget.UUID -> Task PageLoadError Model
init session slug =
    let
        maybeAuthToken =
            Maybe.map .token session.user

        loadWidget =
            Request.Widget.get maybeAuthToken slug
                |> Http.toTask

        loadData =
            Request.Widget.loadData maybeAuthToken slug
                |> Http.toTask

        handleLoadError err =
            pageLoadError Page.Other ("Widget is currently unavailable. " ++ (toString err))
    in
        Task.map2 (Model [] "" [] initPhxSocket) loadWidget loadData
            |> Task.mapError handleLoadError


initPhxSocket : Phoenix.Socket.Socket Msg
initPhxSocket =
    Phoenix.Socket.init socketServer
        |> Phoenix.Socket.withDebug
        |> Phoenix.Socket.on "new:msg" channelName ReceiveChatMessage



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    let
        widget =
            model.article

        author =
            widget.author

        buttons =
            viewButtons widget author session.user
    in
        div [ class "article-page" ]
            [ viewBanner model.errors widget author session.user
            , div [ class "container page" ]
                [ div [ class "row article-content" ]
                    [ div [ class "col-md-12" ]
                        [ h3 [] [ text widget.name ]
                        , p [] [ text ((toString widget.adapter) ++ " | " ++ (toString widget.renderer)) ]
                        , Renderer.run widget model.data
                        ]
                    ]
                , hr [] []
                , viewDataSource model
                , div [ class "article-actions" ]
                    [ div [ class "article-meta" ] <|
                        [ a [ Route.href (Route.Profile author.username) ]
                            [ img [ UserPhoto.src author.image ] [] ]
                        , div [ class "info" ]
                            [ Views.Author.view author.username
                            , Views.Widget.viewTimestamp widget
                            ]
                        ]
                            ++ buttons
                    ]
                , div [ class "row" ]
                    [-- div [ class "col-xs-12 col-md-8 offset-md-2" ] <|
                     --     viewAddComment False session.user
                     --         :: List.map (viewComment session.user) model.comments
                    ]
                ]
            ]


viewBanner : List String -> Widget a -> Author -> Maybe User -> Html Msg
viewBanner errors article author maybeUser =
    let
        buttons =
            viewButtons article author maybeUser
    in
        div [ class "banner" ]
            [ div [ class "container" ]
                [ h1 [] [ text article.name ]
                , div [ class "article-meta" ] <|
                    [ a [ Route.href (Route.Profile author.username) ]
                        [ img [ UserPhoto.src author.image ] [] ]
                    , div [ class "info" ]
                        [ Views.Author.view author.username
                        , Views.Widget.viewTimestamp article
                        ]
                    ]
                        ++ buttons
                , Views.Errors.view DismissErrors errors
                ]
            ]


viewAddComment : Bool -> Maybe User -> Html Msg
viewAddComment postingDisabled maybeUser =
    case maybeUser of
        Nothing ->
            p []
                [ a [ Route.href Route.Login ] [ text "Sign in" ]
                , text " or "
                , a [ Route.href Route.Register ] [ text "sign up" ]
                , text " to add comments on this article."
                ]

        Just user ->
            Html.form [ class "card comment-form" ]
                [ div [ class "card-block" ]
                    [ textarea
                        [ class "form-control"
                        , placeholder "Write a comment..."
                        , attribute "rows" "3"
                        ]
                        []
                    ]
                , div [ class "card-footer" ]
                    [ img [ class "comment-author-img", UserPhoto.src user.image ] []
                    , button
                        [ class "btn btn-sm btn-primary"
                        , disabled postingDisabled
                        ]
                        [ text "Post Comment" ]
                    ]
                ]


viewButtons : Widget a -> Author -> Maybe User -> List (Html Msg)
viewButtons article author maybeUser =
    let
        isMyWidget =
            Maybe.map .username maybeUser == Just author.username
    in
        if isMyWidget then
            [ editButton article
            , text " "
            , deleteButton article
            ]
        else
            [ followButton author
            , text " "
            , favoriteButton article
            ]


viewComment : Maybe User -> Comment -> Html Msg
viewComment user comment =
    let
        author =
            comment.author

        isAuthor =
            Maybe.map .username user == Just comment.author.username
    in
        div [ class "card" ]
            [ div [ class "card-block" ]
                [ p [ class "card-text" ] [ text comment.body ] ]
            , div [ class "card-footer" ]
                [ a [ class "comment-author", href "" ]
                    [ img [ class "comment-author-img", UserPhoto.src author.image ] []
                    , text " "
                    ]
                , text " "
                , a [ class "comment-author", Route.href (Route.Profile author.username) ]
                    [ text (User.usernameToString comment.author.username) ]
                , span [ class "date-posted" ] [ text (formatCommentTimestamp comment.createdAt) ]
                , viewIf isAuthor <|
                    span
                        [ class "mod-options"
                        ]
                        [ i [ class "ion-trash-a" ] [] ]
                ]
            ]


formatCommentTimestamp : Date -> String
formatCommentTimestamp =
    Date.Format.format "%B %e, %Y"


viewDataSource : Model -> Html Msg
viewDataSource model =
    div []
        [ h3 [] [ text "Channels:" ]
        , div
            []
            [ button [ onClick JoinChannel ] [ text "Subscribe" ]
            , button [ onClick LeaveChannel ] [ text "Leave" ]
            ]
        , channelsTable (Dict.values model.phxSocket.channels)
        , br [] []
        , h3 [] [ text "Messages:" ]
        , newMessageForm model
        , ul [] ((List.reverse << List.map renderMessage) model.messages)
        ]


channelsTable : List (Phoenix.Channel.Channel Msg) -> Html Msg
channelsTable channels =
    table []
        [ tbody [] (List.map channelRow channels)
        ]


channelRow : Phoenix.Channel.Channel Msg -> Html Msg
channelRow channel =
    tr []
        [ td [] [ text channel.name ]
        , td [] [ (text << toString) channel.payload ]
        , td [] [ (text << toString) channel.state ]
        ]


newMessageForm : Model -> Html Msg
newMessageForm model =
    form [ onSubmit SendMessage ]
        [ input [ type_ "text", value model.newMessage, onInput SetNewMessage ] []
        ]


renderMessage : String -> Html Msg
renderMessage str =
    li [] [ text str ]



-- UPDATE --


type Msg
    = DismissErrors
    | ToggleFavorite
    | FavoriteCompleted (Result Http.Error (Widget Body))
    | ToggleFollow
    | FollowCompleted (Result Http.Error Author)
    | DeleteWidget
    | WidgetDeleted (Result Http.Error ())
    | SendMessage
    | SetNewMessage String
    | PhoenixMsg (Phoenix.Socket.Msg Msg)
    | ReceiveChatMessage JE.Value
    | JoinChannel
    | LeaveChannel
    | ShowJoinedMessage String
    | ShowLeftMessage String
    | NoOp


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    let
        socket =
            model.phxSocket

        article =
            model.article

        author =
            article.author
    in
        case msg of
            DismissErrors ->
                { model | errors = [] } => Cmd.none

            ToggleFavorite ->
                let
                    cmdFromAuth authToken =
                        Request.Widget.toggleFavorite model.article authToken
                            |> Http.toTask
                            |> Task.map (\newWidget -> { newWidget | body = article.body })
                            |> Task.attempt FavoriteCompleted
                in
                    session
                        |> Session.attempt "favorite" cmdFromAuth
                        |> Tuple.mapFirst (Util.appendErrors model)

            FavoriteCompleted (Ok newWidget) ->
                { model | article = newWidget } => Cmd.none

            FavoriteCompleted (Err error) ->
                -- In a serious production application, we would log the error to
                -- a logging service so we could investigate later.
                [ "There was a server error trying to record your Favorite. Sorry!" ]
                    |> Util.appendErrors model
                    => Cmd.none

            ToggleFollow ->
                let
                    cmdFromAuth authToken =
                        authToken
                            |> Request.Profile.toggleFollow author.username author.following
                            |> Http.send FollowCompleted
                in
                    session
                        |> Session.attempt "follow" cmdFromAuth
                        |> Tuple.mapFirst (Util.appendErrors model)

            FollowCompleted (Ok { following }) ->
                let
                    newWidget =
                        { article | author = { author | following = following } }
                in
                    { model | article = newWidget } => Cmd.none

            FollowCompleted (Err error) ->
                { model | errors = "Unable to follow user." :: model.errors }
                    => Cmd.none

            DeleteWidget ->
                let
                    cmdFromAuth authToken =
                        authToken
                            |> Request.Widget.delete model.article.uuid
                            |> Http.send WidgetDeleted
                in
                    session
                        |> Session.attempt "delete articles" cmdFromAuth
                        |> Tuple.mapFirst (Util.appendErrors model)

            WidgetDeleted (Ok ()) ->
                model => Route.modifyUrl Route.Home

            WidgetDeleted (Err error) ->
                { model | errors = model.errors ++ [ "Server error while trying to delete article." ] }
                    => Cmd.none

            PhoenixMsg msg ->
                let
                    ( phxSocket, phxCmd ) =
                        Phoenix.Socket.update msg socket
                in
                    ( { model | phxSocket = phxSocket }
                    , Cmd.map PhoenixMsg phxCmd
                    )

            SendMessage ->
                let
                    payload =
                        (JE.object [ ( "user", JE.string user ), ( "body", JE.string model.newMessage ) ])

                    push_ =
                        Phoenix.Push.init "new:msg" channelName
                            |> Phoenix.Push.withPayload payload

                    ( phxSocket, phxCmd ) =
                        Phoenix.Socket.push push_ socket
                in
                    ( { model
                        | newMessage = ""
                        , phxSocket = phxSocket
                      }
                    , Cmd.map PhoenixMsg phxCmd
                    )

            SetNewMessage str ->
                ( { model | newMessage = str }
                , Cmd.none
                )

            ReceiveChatMessage raw ->
                case JD.decodeValue DataSourceMessage.decoder raw of
                    Ok chatMessage ->
                        ( { model
                            | messages =
                                ((chatMessage.user ++ ": " ++ (toString chatMessage.body)) :: model.messages)
                            , data = chatMessage.body
                          }
                        , Cmd.none
                        )

                    Err error ->
                        Debug.log ("ERROR decoding  " ++ (toString error) ++ "---> ")
                            ( model, Cmd.none )

            JoinChannel ->
                let
                    channel =
                        Phoenix.Channel.init channelName
                            |> Phoenix.Channel.withPayload userParams
                            |> Phoenix.Channel.onJoin (always (ShowJoinedMessage channelName))
                            |> Phoenix.Channel.onClose (always (ShowLeftMessage channelName))

                    ( phxSocket, phxCmd ) =
                        Phoenix.Socket.join channel socket
                in
                    ( { model | phxSocket = phxSocket }
                    , Cmd.map PhoenixMsg phxCmd
                    )

            LeaveChannel ->
                let
                    ( phxSocket, phxCmd ) =
                        Phoenix.Socket.leave channelName socket
                in
                    ( { model | phxSocket = phxSocket }
                    , Cmd.map PhoenixMsg phxCmd
                    )

            ShowJoinedMessage channelName ->
                ( { model | messages = ("Joined channel " ++ channelName) :: model.messages }
                , Cmd.none
                )

            ShowLeftMessage channelName ->
                ( { model | messages = ("Left channel " ++ channelName) :: model.messages }
                , Cmd.none
                )

            NoOp ->
                ( model, Cmd.none )



-- INTERNAL --


withoutComment : CommentId -> List Comment -> List Comment
withoutComment id =
    List.filter (\comment -> comment.id /= id)


favoriteButton : Widget a -> Html Msg
favoriteButton article =
    let
        favoriteText =
            " Favorite Widget (" ++ toString article.favoritesCount ++ ")"
    in
        Favorite.button (\_ -> ToggleFavorite) article [] [ text favoriteText ]


deleteButton : Widget a -> Html Msg
deleteButton article =
    button [ class "btn btn-outline-danger btn-sm", onClick DeleteWidget ]
        [ i [ class "ion-trash-a" ] [], text " Delete Widget" ]


editButton : Widget a -> Html Msg
editButton article =
    a [ class "btn btn-outline-secondary btn-sm", Route.href (Route.EditWidget article.uuid) ]
        [ i [ class "ion-edit" ] [], text " Edit Widget" ]


followButton : Follow.State record -> Html Msg
followButton =
    Follow.button (\_ -> ToggleFollow)
