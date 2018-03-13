module Page.Dashboard
    exposing
        ( Model
        , Msg(..)
        , init
        , update
        , view
        , subscriptions
        )

import Data.Dashboard as Dashboard exposing (Dashboard)
import Data.DataSource as DataSource exposing (..)
import Data.DataSourceMessage as DataSourceMessage exposing (DataSourceMessage, decoder)
import Data.Session as Session exposing (Session)
import Data.UUID as UUID
import Data.User as User exposing (User)
import Data.UserPhoto as UserPhoto
import Data.Widget as Widget exposing (..)
import Data.Widget.Author as Author exposing (Author)
import Data.Widget.Table as Table exposing (Cell, Data, decoder)
import Date exposing (Date)
import Date.Format
import Dict
import Html exposing (..)
import Html.Attributes exposing (attribute, class, disabled, href, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as JD exposing (field)
import Json.Encode as JE
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Phoenix.Channel
import Phoenix.Push
import Phoenix.Socket
import Request.Dashboard
import Request.Profile
import Route
import Task exposing (Task)
import Util exposing ((=>), pair, viewIf)
import Views.Author
import Views.Dashboard
import Views.Dashboard.Favorite as Favorite
import Views.Errors
import Views.Page as Page
import Views.User.Follow as Follow
import Views.Widget.Renderers.Renderer as Renderer
import Views.Widget.Renderers.RendererMessage as RendererMessage exposing (..)
import Window exposing (..)


-- Constants -------------------------------------------------------------------


socketServer : String
socketServer =
    -- TODO Pass in via a Flag
    -- "ws://localhost:4000/socket/websocket"
    "wss://ew-dashboards-staging.herokuapp.com/socket/websocket"


channelName : DataSource -> String
channelName dataSource =
    dataSource
        |> DataSource.toChannel


user : String
user =
    "msp"


userParams : JE.Value
userParams =
    JE.object [ ( "user_id", JE.string user ) ]



-- Model -----------------------------------------------------------------------


type alias Model =
    { errors : List String
    , newMessage : String
    , messages : List String
    , userDefinedDataSourceUUID : Maybe String
    , phxSocket : Phoenix.Socket.Socket Msg
    , dashboard : Dashboard
    , windowSize : Window.Size
    }


init : Session -> UUID.UUID -> Task PageLoadError Model
init session slug =
    let
        initPhxSocket =
            Phoenix.Socket.init socketServer

        initPhxSocketWithDebug =
            Phoenix.Socket.init socketServer
                |> Phoenix.Socket.withDebug

        maybeAuthToken =
            Maybe.map .token session.user

        loadWidget =
            Request.Dashboard.get maybeAuthToken slug
                |> Http.toTask

        handleLoadError err =
            pageLoadError Page.Other
                ("Dashboard is currently unavailable. "
                    ++ (toString err)
                )

        initModel =
            Model []
                ""
                []
                Nothing
                initPhxSocket
    in
        Task.map2 initModel loadWidget Window.size
            |> Task.mapError handleLoadError



-- View ------------------------------------------------------------------------


view : Session -> Model -> Html Msg
view session model =
    let
        dashboard =
            model.dashboard

        author =
            dashboard.author

        buttons =
            viewButtons dashboard author session.user

        -- below is for dev only! saves having to wait for a websocket if you're working on UI..
        -- use in conjunction with dev-data-test.json for Widget config
        devData =
            createDevData
    in
        div [ class "article-page" ]
            [ viewBanner model.errors dashboard author session.user
            , div [ class "container-fluid page" ]
                [ div [ class "row article-content" ] <|
                    List.map
                        (\widget ->
                            -- Html.map (\uuid -> RendererMsg uuid) (Renderer.run model.windowSize.width model.windowSize.height widget devData)
                            Html.map (\uuid -> RendererMsg uuid) (Renderer.run model.windowSize.width model.windowSize.height widget widget.data)
                        )
                        model.dashboard.widgets
                  -- , viewAndDebugDataSource model
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



-- Update ----------------------------------------------------------------------


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
    | ResizeWindow Int Int
    | ShowJoinedMessage String
    | ShowLeftMessage String
    | RendererMsg RendererMessage.Msg
    | NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes (\{ height, width } -> ResizeWindow width height)
        , Phoenix.Socket.listen model.phxSocket PhoenixMsg
        ]


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
        -- case Debug.log "-------------- > Dashboard.update: " msg of
        case msg of
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
                                    (primaryDataSource.uuid == chatMessage.uuid)
                        in
                            ( { model
                                | messages =
                                    ((chatMessage.user
                                        ++ ": "
                                        ++ (toString chatMessage.body)
                                     )
                                        :: model.messages
                                    )
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
                                    |> Phoenix.Channel.onJoin
                                        (always (ShowJoinedMessage fullChannelName))
                                    |> Phoenix.Channel.onClose
                                        (always (ShowLeftMessage fullChannelName))
                        in
                            ( fullChannelName, channel )

                    channels =
                        List.map initChannel model.dashboard.widgets

                    initCmds =
                        []

                    joinChannel phxSocket channels commands =
                        -- case Debug.log "joinChannel" <| List.head channels of
                        case List.head channels of
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
                                        updatedPhxSocket
                                            |> Phoenix.Socket.on "new:msg"
                                                fullChannelName
                                                ReceiveChatMessage
                                in
                                    joinChannel
                                        listeningSocket
                                        remainingChannels
                                        (phxCmd :: commands)

                            Nothing ->
                                ( phxSocket, commands )

                    ( updatedSocket, phxCmds ) =
                        joinChannel socket channels initCmds
                in
                    ( { model | phxSocket = updatedSocket }
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
                in
                    ( { model | phxSocket = phxSocket }
                    , Cmd.map PhoenixMsg phxCmd
                    )

            ShowJoinedMessage channelName ->
                ( { model
                    | messages =
                        ("Joined channel "
                            ++ channelName
                        )
                            :: model.messages
                  }
                , Cmd.none
                )

            ShowLeftMessage channelName ->
                ( { model
                    | messages =
                        ("Left channel "
                            ++ channelName
                        )
                            :: model.messages
                  }
                , Cmd.none
                )

            ResizeWindow w h ->
                ( { model | windowSize = Window.Size w h }, Cmd.none )

            RendererMsg subMsg ->
                let
                    originalDashboard =
                        model.dashboard

                    ( uuid, updatedSocket, updatedDashboard, cmd ) =
                        case subMsg of
                            SetDataSourceUUID uuid ->
                                ( Just uuid, model.phxSocket, model.dashboard, Cmd.none )

                            UpdateDataSource widgetUUID dataSourceUUID dataSourceName ->
                                let
                                    updatedDataSource =
                                        DataSource (Maybe.withDefault "--" model.userDefinedDataSourceUUID) dataSourceName

                                    updatedDashboard =
                                        { originalDashboard | widgets = updatedWidgets }

                                    updatedWidgets =
                                        List.map
                                            (\widget ->
                                                case UUID.slugToString widget.uuid == widgetUUID of
                                                    True ->
                                                        let
                                                            ( deleteMe, keepMe ) =
                                                                List.partition (\ds -> ds.uuid == dataSourceUUID) widget.dataSources
                                                        in
                                                            { widget
                                                                | dataSources =
                                                                    [ updatedDataSource ] ++ keepMe
                                                                , data = Data []
                                                            }

                                                    False ->
                                                        widget
                                            )
                                            model.dashboard.widgets

                                    fullChannelName =
                                        updatedDataSource
                                            |> DataSource.toChannel

                                    channel =
                                        Phoenix.Channel.init fullChannelName
                                            |> Phoenix.Channel.withPayload userParams
                                            |> Phoenix.Channel.onJoin (always (ShowJoinedMessage fullChannelName))
                                            |> Phoenix.Channel.onClose (always (ShowLeftMessage fullChannelName))

                                    -- TODO leave existing channel?
                                    ( updatedPhxSocket, phxCmd ) =
                                        Phoenix.Socket.join channel model.phxSocket

                                    listeningSocket =
                                        updatedPhxSocket |> Phoenix.Socket.on "new:msg" fullChannelName ReceiveChatMessage
                                in
                                    ( model.userDefinedDataSourceUUID, listeningSocket, updatedDashboard, phxCmd )
                in
                    ( { model
                        | userDefinedDataSourceUUID = uuid
                        , phxSocket = updatedSocket
                        , dashboard = updatedDashboard
                      }
                    , Cmd.map PhoenixMsg cmd
                    )

            NoOp ->
                ( model, Cmd.none )



-- Internal --------------------------------------------------------------------


createDevData : Data
createDevData =
    Data
        [ [ "Client A", "2,482,780", "1,294,181" ]
        , [ "Client B", "39,500", "(11,400)" ]
        , [ "Client C", "394,000", "187,147" ]
        ]



--     Data
--         [ [ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" ]
--         , List.range 100 111 |> List.map toString
--         , List.range 200 211 |> List.map toString
--         , List.range 300 311 |> List.map toString
--         , List.range 400 411 |> List.map toString
--         , List.range 500 511 |> List.map toString
--         , List.range 600 611 |> List.map toString
--         , List.range 700 711 |> List.map toString
--         , List.range 800 811 |> List.map toString
--         , List.range 900 911 |> List.map toString
--         , List.range 1000 1011 |> List.map toString
--         , List.range 1100 1111 |> List.map toString
--         , List.range 1200 1211 |> List.map toString
--         ]
