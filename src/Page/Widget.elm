module Page.Widget exposing (Model, Msg(..), init, update, view, subscriptions)

{-| Viewing an individual widget.
-}

import Data.Widget as Widget exposing (Widget, Body)
import Data.Widget.Author as Author exposing (Author)
import Data.Session as Session exposing (Session)
import Data.User as User exposing (User)
import Data.UserPhoto as UserPhoto
import Data.DataSource as DataSource exposing (..)
import Data.DataSourceMessage as DataSourceMessage exposing (DataSourceMessage, decoder)
import Date exposing (Date)
import Date.Format
import Html exposing (..)
import Html.Attributes exposing (attribute, class, disabled, href, id, placeholder, value, type_)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Request.Widget
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


-- import Views.Widget.Renderers.Renderer as Renderer

import Json.Encode as JE
import Json.Decode as JD exposing (field)
import Phoenix.Socket
import Phoenix.Channel
import Phoenix.Push
import Dict


-- CONSTANTS
-- TODO Pass in via a Flag


socketServer : String
socketServer =
    "wss://ew-dashboards-staging.herokuapp.com/socket/websocket"



-- "ws://localhost:4000/socket/websocket"


channelName : DataSource -> String
channelName dataSource =
    dataSource
        |> DataSource.toChannel



-- MODEL --


type alias Model =
    { errors : List String
    , newMessage : String
    , messages : List String
    , width : Int
    , height : Int
    , phxSocket : Phoenix.Socket.Socket Msg
    , data : Data
    , widget : Widget Body
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

        handleLoadError err =
            pageLoadError Page.Other ("Widget is currently unavailable. " ++ (toString err))

        initModel =
            (Model [] "" [] 0 0 initPhxSocket (Data []))
    in
        Task.map initModel loadWidget
            |> Task.mapError handleLoadError


initPhxSocket : Phoenix.Socket.Socket Msg
initPhxSocket =
    Phoenix.Socket.init socketServer
        |> Phoenix.Socket.withDebug



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    let
        widget =
            model.widget

        author =
            widget.author

        buttons =
            viewButtons widget author session.user

        -- below is for dev only! saves having to wait for a websocket if you're working on UI..
        devData =
            Data
                [ List.range 0 12 |> List.map toString
                , List.range 100 111 |> List.map toString |> (::) "00:00"
                , List.range 200 211 |> List.map toString |> (::) "00:30"
                , List.range 300 311 |> List.map toString |> (::) "01:00"
                , List.range 400 411 |> List.map toString |> (::) "01:30"
                , List.range 500 511 |> List.map toString |> (::) "02:00"
                , List.range 600 611 |> List.map toString |> (::) "02:30"
                , List.range 700 711 |> List.map toString |> (::) "03:00"
                , List.range 800 811 |> List.map toString |> (::) "03:30"
                , List.range 900 911 |> List.map toString |> (::) "04:00"
                , List.range 1000 1011 |> List.map toString |> (::) "04:30"
                , List.range 1100 1111 |> List.map toString |> (::) "05:00"
                , List.range 1200 1211 |> List.map toString |> (::) "05:30"
                ]
    in
        div [ class "article-page" ]
            [ viewBanner model.errors widget author session.user
            , div [ class "container page" ]
                [ div [ class "row article-content" ]
                    [ div [ class "col-md-12" ]
                        [ h3 [] [ text <| (widget.name) ]
                          -- TODO, copy from Dashboard
                          -- , Renderer.run model.width model.height widget model.data
                          --   Renderer.run model.width model.height widget devData
                        , br [] []
                        , br [] []
                        , p [ class "small" ] [ text <| channelName <| Widget.primaryDataSource widget ]
                        ]
                    ]
                , hr [] []
                  -- , viewAndDebugDataSource model
                  -- , div [ class "article-actions" ]
                  --     [ div [ class "article-meta" ] <|
                  --         [ a [ Route.href (Route.Profile author.username) ]
                  --             [ img [ UserPhoto.src author.image ] [] ]
                  --           , div [ class "info" ]
                  --               [ Views.Author.view author.username
                  --               , Views.Widget.viewTimestamp widget
                  --               ]
                  --         ]
                  --             ++ buttons
                  --     ]
                ]
            ]


viewBanner : List String -> Widget a -> Author -> Maybe User -> Html Msg
viewBanner errors widget author maybeUser =
    let
        buttons =
            viewButtons widget author maybeUser
    in
        div [ class "banner" ]
            [ div [ class "container" ]
                [ h1 [] [ text widget.name ]
                , div [ class "article-meta" ] <|
                    [ a [ Route.href (Route.Profile author.username) ]
                        [ img [ UserPhoto.src author.image ] [] ]
                    , div [ class "info" ]
                        [ Views.Author.view author.username
                        , Views.Widget.viewTimestamp widget
                        ]
                    ]
                        ++ buttons
                , Views.Errors.view DismissErrors errors
                ]
            ]


viewButtons : Widget a -> Author -> Maybe User -> List (Html Msg)
viewButtons widget author maybeUser =
    let
        isMyWidget =
            Maybe.map .username maybeUser == Just author.username
    in
        if isMyWidget then
            [ editButton widget
            , text " "
            , deleteButton widget
            ]
        else
            [ followButton author
            , text " "
            , favoriteButton widget
            ]


formatTimestamp : Date -> String
formatTimestamp =
    Date.Format.format "%B %e, %Y"


viewAndDebugDataSource : Model -> Html Msg
viewAndDebugDataSource model =
    div []
        [ h3 [] [ text "Data updates:" ]
        , div
            []
            [ button [ onClick JoinChannel ] [ text "Subscribe" ]
            , button [ onClick LeaveChannel ] [ text "Leave" ]
            ]
        , channelsTable (Dict.values model.phxSocket.channels)
        , br [] []
        , h3 [] [ text "Broadcast:" ]
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

        widget =
            model.widget

        author =
            widget.author
    in
        case msg of
            -- case Debug.log "------------------- > Widget.update: " msg of
            DismissErrors ->
                { model | errors = [] } => Cmd.none

            ToggleFavorite ->
                let
                    cmdFromAuth authToken =
                        Request.Widget.toggleFavorite model.widget authToken
                            |> Http.toTask
                            |> Task.map (\newWidget -> { newWidget | body = widget.body })
                            |> Task.attempt FavoriteCompleted
                in
                    session
                        |> Session.attempt "favorite" cmdFromAuth
                        |> Tuple.mapFirst (Util.appendErrors model)

            FavoriteCompleted (Ok newWidget) ->
                { model | widget = newWidget } => Cmd.none

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
                        { widget | author = { author | following = following } }
                in
                    { model | widget = newWidget } => Cmd.none

            FollowCompleted (Err error) ->
                { model | errors = "Unable to follow user." :: model.errors }
                    => Cmd.none

            DeleteWidget ->
                let
                    cmdFromAuth authToken =
                        authToken
                            |> Request.Widget.delete model.widget.uuid
                            |> Http.send WidgetDeleted
                in
                    session
                        |> Session.attempt "delete widgets" cmdFromAuth
                        |> Tuple.mapFirst (Util.appendErrors model)

            WidgetDeleted (Ok ()) ->
                model => Route.modifyUrl Route.Home

            WidgetDeleted (Err error) ->
                { model | errors = model.errors ++ [ "Server error while trying to delete widget." ] }
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
                        Phoenix.Push.init "new:msg" (channelName <| Widget.primaryDataSource model.widget)
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
                    fullChannelName =
                        channelName <| Widget.primaryDataSource model.widget

                    channel =
                        Phoenix.Channel.init fullChannelName
                            |> Phoenix.Channel.withPayload userParams
                            |> Phoenix.Channel.onJoin (always (ShowJoinedMessage fullChannelName))
                            |> Phoenix.Channel.onClose (always (ShowLeftMessage fullChannelName))

                    ( phxSocket, phxCmd ) =
                        Phoenix.Socket.join channel socket

                    listeningSocket =
                        phxSocket |> Phoenix.Socket.on "new:msg" fullChannelName ReceiveChatMessage
                in
                    ( { model | phxSocket = listeningSocket }
                    , Cmd.map PhoenixMsg phxCmd
                    )

            LeaveChannel ->
                let
                    ( phxSocket, phxCmd ) =
                        Phoenix.Socket.leave (channelName <| Widget.primaryDataSource model.widget) socket
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


favoriteButton : Widget a -> Html Msg
favoriteButton widget =
    let
        favoriteText =
            " Favorite Widget (" ++ toString widget.favoritesCount ++ ")"
    in
        Favorite.button (\_ -> ToggleFavorite) widget [] [ text favoriteText ]


deleteButton : Widget a -> Html Msg
deleteButton widget =
    button [ class "btn btn-outline-danger btn-sm", onClick DeleteWidget ]
        [ i [ class "ion-trash-a" ] [], text " Delete Widget" ]


editButton : Widget a -> Html Msg
editButton widget =
    a [ class "btn btn-outline-secondary btn-sm", Route.href (Route.EditWidget widget.uuid) ]
        [ i [ class "ion-edit" ] [], text " Edit Widget" ]


followButton : Follow.State record -> Html Msg
followButton =
    Follow.button (\_ -> ToggleFollow)
