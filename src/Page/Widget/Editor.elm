module Page.Widget.Editor exposing (Model, Msg, initEdit, initNew, update, view)

import Data.Widget as Widget exposing (Widget, Body)
import Data.Session as Session exposing (Session)
import Data.User as User exposing (User)
import Data.UUID as UUID
import Html exposing (..)
import Html.Attributes exposing (attribute, class, defaultValue, disabled, href, id, placeholder, type_)
import Html.Events exposing (onInput, onSubmit)
import Http
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Request.Widget
import Route
import Task exposing (Task)
import Util exposing ((=>), pair, viewIf)
import Validate exposing (ifBlank)
import Views.Form as Form
import Views.Page as Page


-- MODEL --


type alias Model =
    { errors : List Error
    , editingWidget : Maybe UUID.UUID
    , name : String
    , description : String
    , tags : List String
    }


initNew : Model
initNew =
    { errors = []
    , editingWidget = Nothing
    , name = ""
    , description = ""
    , tags = []
    }


initEdit : Session -> UUID.UUID -> Task PageLoadError Model
initEdit session slug =
    let
        maybeAuthToken =
            session.user
                |> Maybe.map .token
    in
        Request.Widget.get maybeAuthToken slug
            |> Http.toTask
            |> Task.mapError (\_ -> pageLoadError Page.Other "Widget is currently unavailable.")
            |> Task.map
                (\widget ->
                    { errors = []
                    , editingWidget = Just slug
                    , name = widget.name
                    , description = widget.description
                    , tags = widget.tags
                    }
                )



-- VIEW --


view : Model -> Html Msg
view model =
    div [ class "editor-page" ]
        [ div [ class "container page" ]
            [ div [ class "row" ]
                [ div [ class "col-md-10 offset-md-1 col-xs-12" ]
                    [ Form.viewErrors model.errors
                    , viewForm model
                    ]
                ]
            ]
        ]


viewForm : Model -> Html Msg
viewForm model =
    let
        isEditing =
            model.editingWidget /= Nothing

        saveButtonText =
            if isEditing then
                "Update Widget"
            else
                "Publish Widget"
    in
        Html.form [ onSubmit Save ]
            [ fieldset []
                [ Form.input
                    [ class "form-control-lg"
                    , placeholder "Widget Name"
                    , onInput SetName
                    , defaultValue model.name
                    ]
                    []
                , Form.input
                    [ placeholder "What's this widget about?"
                    , onInput SetDescription
                    , defaultValue model.description
                    ]
                    []
                , Form.textarea
                    [ placeholder "Write your widget (in markdown)"
                    , attribute "rows" "8"
                    ]
                    []
                , Form.input
                    [ placeholder "Enter tags"
                    , onInput SetTags
                    , defaultValue (String.join " " model.tags)
                    ]
                    []
                , button [ class "btn btn-lg pull-xs-right btn-primary" ]
                    [ text saveButtonText ]
                ]
            ]



-- UPDATE --


type Msg
    = Save
    | SetName String
    | SetDescription String
    | SetTags String
    | CreateCompleted (Result Http.Error Widget)
    | EditCompleted (Result Http.Error Widget)


update : User -> Msg -> Model -> ( Model, Cmd Msg )
update user msg model =
    case msg of
        Save ->
            case validate model of
                [] ->
                    case model.editingWidget of
                        Nothing ->
                            user.token
                                |> Request.Widget.create model
                                |> Http.send CreateCompleted
                                |> pair { model | errors = [] }

                        Just slug ->
                            user.token
                                |> Request.Widget.update slug model
                                |> Http.send EditCompleted
                                |> pair { model | errors = [] }

                errors ->
                    { model | errors = errors } => Cmd.none

        SetName name ->
            { model | name = name } => Cmd.none

        SetDescription description ->
            { model | description = description } => Cmd.none

        SetTags tags ->
            { model | tags = tagsFromString tags } => Cmd.none

        CreateCompleted (Ok widget) ->
            Route.Widget widget.uuid
                |> Route.modifyUrl
                |> pair model

        CreateCompleted (Err error) ->
            { model | errors = model.errors ++ [ Form => "Server error while attempting to publish widget" ] }
                => Cmd.none

        EditCompleted (Ok widget) ->
            Route.Widget widget.uuid
                |> Route.modifyUrl
                |> pair model

        EditCompleted (Err error) ->
            { model | errors = model.errors ++ [ Form => "Server error while attempting to save widget" ] }
                => Cmd.none



-- VALIDATION --


type Field
    = Form
    | Name


type alias Error =
    ( Field, String )


validate : Model -> List Error
validate =
    Validate.all
        [ .name >> ifBlank (Name => "name can't be blank.")
        ]



-- INTERNAL --


tagsFromString : String -> List String
tagsFromString str =
    str
        |> String.split " "
        |> List.map String.trim
        |> List.filter (not << String.isEmpty)


redirectToWidget : UUID.UUID -> Cmd msg
redirectToWidget =
    Route.modifyUrl << Route.Widget
