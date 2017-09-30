module WidgetDecoder exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Json.Encode as Encode exposing (object)
import Json.Decode as Decode exposing (..)
import JsonApi.Types exposing (Document, Resource)
import String
import Data.DataSource as DataSource exposing (..)
import Data.Widget.Author as Author exposing (..)
import Data.Widget as Widget exposing (Body(..), decoder, defaultAttributes)
import Data.Widget.Adapters.Adapter as Adapter exposing (Adapter(..))
import Data.Widget.Renderer as Renderer exposing (Renderer(..))
import Data.User as User exposing (Username(..))
import Data.UserPhoto as UserPhoto exposing (UserPhoto(..))
import Data.UUID as UUID exposing (UUID)
import Date
import JsonApi.Decode
import JsonApi.Documents
import JsonApi.Resources
import List.Extra


widgetDecoderTest : Test
widgetDecoderTest =
    let
        input =
            """
                    {
                        "jsonapi": {
                            "version": "1.0"
                        },
                        "included": [
                            {
                                "type": "author",
                                "id": "da4ec009-7dd9-4036-9e82-2073fad03fd8",
                                "attributes": {
                                    "username": "msp",
                                    "image-src": "https://cenatus.org/images/msp.jpg"
                                }
                            },
                            {
                                "type": "data-source",
                                "meta": {
                                    "url": "http://google.com/my-spread-sheet"
                                },
                                "id": "de2c5277-1a63-4196-a8e5-c2ed62bf89da",
                                "attributes": {
                                    "name": "google spreadsheet Q2",
                                    "meta": {
                                        "url": "http://google.com/my-spread-sheet"
                                    }
                                }
                            }
                        ],
                        "data": [
                            {
                                "type": "widget",
                                "relationships": {
                                    "data-sources": {
                                        "data": [
                                            {
                                                "type": "data-source",
                                                "id": "de2c5277-1a63-4196-a8e5-c2ed62bf89da"
                                            }
                                        ]
                                    },
                                    "author": {
                                        "data": {
                                            "type": "author",
                                            "id": "da4ec009-7dd9-4036-9e82-2073fad03fd8"
                                        }
                                    }
                                },
                                "id": "948f0330-235c-462f-a8a5-192b924e445b",
                                "attributes": {
                                    "renderer": "BAR_CHART",
                                    "name": "12 months Table",
                                    "meta": null,
                                    "description": "12 months of important data",
                                    "adapter": "TABLE",
                                    "inserted-at": "2017-09-26T11:07:13.485940Z",
                                    "updated-at": "2017-09-26T11:07:13.485940Z"
                                }
                            }
                        ]
                    }
                    """

        decodedResource =
            case JsonApi.Documents.primaryResourceCollection (getDocumentFrom input) of
                Err string ->
                    Debug.crash string

                Ok resourceList ->
                    case List.head resourceList of
                        Nothing ->
                            Debug.crash "Expected non-empty collection"

                        Just resource ->
                            resource

        -- TODO: Hmm, Id rather the decoder blew up than us have to pass a default type ?
        decodedWidgetAttributes =
            JsonApi.Resources.attributes Widget.decoder decodedResource
                |> Result.toMaybe
                |> Maybe.withDefault Widget.defaultAttributes

        decodedDataSourceRelation =
            case JsonApi.Resources.relatedResourceCollection "data-sources" decodedResource of
                Err string ->
                    Debug.crash string

                Ok dataSourceResources ->
                    case (List.Extra.find (\resource -> (JsonApi.Resources.id resource) == "de2c5277-1a63-4196-a8e5-c2ed62bf89da") dataSourceResources) of
                        Nothing ->
                            Debug.crash "Expected to find related data source with id: de2c5277-1a63-4196-a8e5-c2ed62bf89da"

                        Just resource ->
                            resource

        decodedAuthorRelation =
            case JsonApi.Resources.relatedResource "author" decodedResource of
                Err string ->
                    Debug.crash string

                Ok author ->
                    author

        decodedDataSourceAttributes =
            JsonApi.Resources.attributes DataSource.decoder decodedDataSourceRelation
                |> Result.toMaybe
                |> Maybe.withDefault DataSource.defaultAttributes

        decodedAuthorAttributes =
            JsonApi.Resources.attributes Author.decoder decodedAuthorRelation
                |> Result.toMaybe
                |> Maybe.withDefault Author.defaultAttributes

        widgetIdIsDecoded =
            \_ -> Expect.equal (JsonApi.Resources.id decodedResource) (UUID.slugToString expectedWidgetUUID)

        widgetAttributesAreDecoded =
            \_ -> Expect.equal decodedWidgetAttributes expectedWidgetAttributes

        dataSourceAttributesAreDecoded =
            \_ -> Expect.equal decodedDataSourceAttributes expectedDataSourceAttributes

        authorAttributesAreDecoded =
            \_ -> Expect.equal decodedAuthorAttributes expectedAuthorAttributes

        widgetConstructed =
            let
                widgetUUID =
                    UUID.UUID (JsonApi.Resources.id decodedResource)

                dataSourceUUID =
                    UUID.UUID (JsonApi.Resources.id decodedDataSourceRelation)

                dataSources =
                    DataSource.factory dataSourceUUID decodedDataSourceAttributes

                author =
                    Author.factory decodedAuthorAttributes

                tags =
                    []

                widget =
                    Widget.factory widgetUUID decodedWidgetAttributes [ dataSources ] tags author
            in
                \_ -> Expect.equal widget expectedWidget
    in
        Test.describe "decoding and relationships"
            [ Test.test "it extracts the widget id" widgetIdIsDecoded
            , Test.test "it extracts the widget attributes" widgetAttributesAreDecoded
            , Test.test "it extracts the data source attributes" dataSourceAttributesAreDecoded
            , Test.test "it extracts the author attributes" authorAttributesAreDecoded
            , Test.test "we construct the expected Widget" widgetConstructed
            ]


getDocumentFrom : String -> Document
getDocumentFrom input =
    case decodeString JsonApi.Decode.document input of
        Ok doc ->
            doc

        Err string ->
            Debug.crash string


expectedWidgetUUID =
    UUID.UUID "948f0330-235c-462f-a8a5-192b924e445b"


expectedWidgetAttributes : Widget.WidgetAttributes
expectedWidgetAttributes =
    Widget.WidgetAttributes
        "12 months Table"
        "12 months of important data"
        Adapter.TABLE
        Renderer.BAR_CHART
        expectedDate
        expectedDate
        False
        0


expectedDataSourceUUID =
    UUID.UUID "de2c5277-1a63-4196-a8e5-c2ed62bf89da"


expectedDataSourceAttributes : DataSource.DataSourceAttributes
expectedDataSourceAttributes =
    DataSource.DataSourceAttributes
        "google spreadsheet Q2"


expectedDate =
    Date.fromString "2017-09-26T11:07:13.485940Z" |> Result.withDefault (Date.fromTime 0)


expectedAuthorAttributes =
    Author.AuthorAttributes
        (User.Username "msp")
        (UserPhoto.UserPhoto <| Just "https://cenatus.org/images/msp.jpg")


expectedWidget =
    let
        widgetUUID =
            expectedWidgetUUID

        dataSourceUUID =
            expectedDataSourceUUID

        dataSources =
            DataSource.factory dataSourceUUID expectedDataSourceAttributes

        author =
            Author.factory expectedAuthorAttributes

        tags =
            []
    in
        Widget.factory widgetUUID expectedWidgetAttributes [ dataSources ] tags author
