module Page.Dashboard exposing (Model, Msg(..), init, update, view, subscriptions)

import Data.Dashboard as Dashboard exposing (Dashboard)
import Data.Widget as Widget exposing (..)
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
import Request.Dashboard
import Request.Profile
import Route
import Task exposing (Task)
import Util exposing ((=>), pair, viewIf)
import Views.Widget
import Views.Dashboard.Favorite as Favorite
import Views.Author
import Views.Dashboard
import Views.Errors
import Views.Page as Page
import Views.User.Follow as Follow
import Data.Widget.Table as Table exposing (Data, Cell, decoder)
import Views.Widget.Renderers.Renderer as Renderer
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
    , phxSocket : Phoenix.Socket.Socket Msg
    , dashboard : Dashboard
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
            Request.Dashboard.get maybeAuthToken slug
                |> Http.toTask

        handleLoadError err =
            pageLoadError Page.Other ("Dashboard is currently unavailable. " ++ (toString err))

        initModel =
            Model [] "" [] initPhxSocket
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
        dashboard =
            model.dashboard

        author =
            dashboard.author

        buttons =
            viewButtons dashboard author session.user
    in
        div [ class "article-page" ]
            [ viewBanner model.errors dashboard author session.user
            , div [ class "container-fluid page" ]
                [ div [ class "row article-content" ] <|
                    List.map
                        (\widget ->
                            Renderer.run widget widget.data
                        )
                        model.dashboard.widgets
                , hr [] []
                ]
            ]


viewBanner : List String -> Dashboard -> Author -> Maybe User -> Html Msg
viewBanner errors dashboard author maybeUser =
    let
        buttons =
            viewButtons dashboard author maybeUser
    in
        div [ class "banner" ]
            [ div [ class "container-fluid" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-4" ] [ h2 [] [ text dashboard.name ] ]
                    , div [ class "col-md-5 col-md-offset-3" ]
                        [ div [ class "article-meta article-meta-dashboard" ] <|
                            [ a [ Route.href (Route.Profile author.username) ]
                                [ img [ UserPhoto.src author.image ] [] ]
                            , div [ class "info" ]
                                [ Views.Author.view author.username
                                , Views.Dashboard.viewTimestamp dashboard
                                ]
                            ]
                                ++ buttons
                        , Views.Errors.view DismissErrors errors
                        ]
                    ]
                ]
            ]


viewButtons : Dashboard -> Author -> Maybe User -> List (Html Msg)
viewButtons dashboard author maybeUser =
    let
        isMyDashboard =
            Maybe.map .username maybeUser == Just author.username
    in
        if isMyDashboard then
            [ text " "
            ]
        else
            [ followButton author
            , text " "
            , favoriteButton dashboard
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
    | FavoriteCompleted (Result Http.Error Dashboard)
    | ToggleFollow
    | FollowCompleted (Result Http.Error Author)
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

        dashboard =
            model.dashboard

        author =
            dashboard.author
    in
        case Debug.log "-------------- > Dashboard.update: " msg of
            -- case msg of
            DismissErrors ->
                { model | errors = [] } => Cmd.none

            ToggleFavorite ->
                let
                    cmdFromAuth authToken =
                        Request.Dashboard.toggleFavorite model.dashboard authToken
                            |> Http.toTask
                            -- |> Task.map (\newDashboard -> { newDashboard | body = widget.body })
                            |>
                                Task.map (\newDashboard -> newDashboard)
                            |> Task.attempt FavoriteCompleted
                in
                    session
                        |> Session.attempt "favorite" cmdFromAuth
                        |> Tuple.mapFirst (Util.appendErrors model)

            FavoriteCompleted (Ok newDashboard) ->
                { model | dashboard = newDashboard } => Cmd.none

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
                    newDashboard =
                        { dashboard | author = { author | following = following } }
                in
                    { model | dashboard = newDashboard } => Cmd.none

            FollowCompleted (Err error) ->
                { model | errors = "Unable to follow user." :: model.errors }
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
                        Phoenix.Push.init "new:msg" "foo:bar"
                            -- Phoenix.Push.init "new:msg" (channelName <| Widget.primaryDataSource model.widget)
                            |>
                                Phoenix.Push.withPayload payload

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
                        let
                            originalDashboard =
                                model.dashboard

                            updateWidget newData widget =
                                { widget | data = newData }

                            updatedDashboard =
                                { originalDashboard
                                    | widgets =
                                        List.map
                                            (\widget ->
                                                case matchingDataSource widget chatMessage.uuid of
                                                    True ->
                                                        updateWidget chatMessage.body widget

                                                    False ->
                                                        widget
                                            )
                                            model.dashboard.widgets
                                }

                            matchingDataSource widget uuid =
                                let
                                    primaryDataSource =
                                        Widget.primaryDataSource widget
                                in
                                    Debug.log ("MATCH? primaryDataSource.uuid: " ++ primaryDataSource.uuid ++ " chatMessage.uuid: " ++ chatMessage.uuid) (primaryDataSource.uuid == chatMessage.uuid)
                        in
                            ( { model
                                | messages =
                                    ((chatMessage.user ++ ": " ++ (toString chatMessage.body)) :: model.messages)
                                , dashboard = updatedDashboard
                              }
                            , Cmd.none
                            )

                    Err error ->
                        Debug.log ("ERROR decoding  " ++ (toString error) ++ "---> ")
                            ( model, Cmd.none )

            JoinChannel ->
                let
                    initChannel widget =
                        let
                            fullChannelName =
                                channelName <| Widget.primaryDataSource widget

                            channel =
                                Phoenix.Channel.init fullChannelName
                                    |> Phoenix.Channel.withPayload userParams
                                    |> Phoenix.Channel.onJoin (always (ShowJoinedMessage fullChannelName))
                                    |> Phoenix.Channel.onClose (always (ShowLeftMessage fullChannelName))
                        in
                            ( fullChannelName, channel )

                    channels =
                        List.map initChannel model.dashboard.widgets

                    initCmds =
                        []

                    joinChannel phxSocket channels commands =
                        case Debug.log "joinChannel" <| List.head channels of
                            Just ( fullChannelName, channel ) ->
                                let
                                    ( updatedPhxSocket, phxCmd ) =
                                        Phoenix.Socket.join channel phxSocket

                                    remainingChannels =
                                        case List.tail channels of
                                            Just someChannels ->
                                                someChannels

                                            Nothing ->
                                                []

                                    listeningSocket =
                                        updatedPhxSocket |> Phoenix.Socket.on "new:msg" fullChannelName ReceiveChatMessage
                                in
                                    joinChannel listeningSocket remainingChannels (phxCmd :: commands)

                            Nothing ->
                                ( phxSocket, commands )

                    ( updatedSocket, phxCmds ) =
                        joinChannel socket channels initCmds
                in
                    ( { model | phxSocket = updatedSocket }
                      -- , Debug.log "Cmds: " <|
                    , Cmd.batch <|
                        List.map
                            (\cmd ->
                                Cmd.map PhoenixMsg cmd
                            )
                            phxCmds
                    )

            LeaveChannel ->
                let
                    ( phxSocket, phxCmd ) =
                        Phoenix.Socket.leave "foo:bar" socket

                    -- Phoenix.Socket.leave (channelName <| Widget.primaryDataSource model.widget) socket
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


favoriteButton : Dashboard -> Html Msg
favoriteButton dashboard =
    let
        favoriteText =
            " Favorite Dashboard (" ++ toString dashboard.favoritesCount ++ ")"
    in
        Favorite.button (\_ -> ToggleFavorite) dashboard [] [ text favoriteText ]


followButton : Follow.State record -> Html Msg
followButton =
    Follow.button (\_ -> ToggleFollow)
