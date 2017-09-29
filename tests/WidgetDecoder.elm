module WidgetDecoder exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Json.Encode as Encode exposing (object)
import Json.Decode as Decode exposing (..)
import JsonApi.Types exposing (Document, Resource)
import String
import Data.DataSource as DataSource exposing (DataSource)
import Data.Widget.Author as Author
import Data.Widget as Widget exposing (UUID(..), Body(..), decoder)
import Data.Widget.Adapters.Adapter as Adapter exposing (Adapter(..))
import Data.Widget.Renderer as Renderer exposing (Renderer(..))
import Data.User as User exposing (Username(..))
import Data.UserPhoto as UserPhoto exposing (UserPhoto(..))
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
                                    "image-src": null
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
                                    "renderer": "TABLE",
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

        -- Hmm, Id rather the decoder blew up than us have to pass a default type ?
        decodedWidget =
            JsonApi.Resources.attributes Widget.decoder decodedResource
                |> Result.toMaybe
                |> Maybe.withDefault defaultWidget

        decodedDataSource =
            case JsonApi.Resources.relatedResourceCollection "data-sources" decodedResource of
                Err string ->
                    Debug.crash string

                Ok dataSourceResources ->
                    case (List.Extra.find (\resource -> (JsonApi.Resources.id resource) == "de2c5277-1a63-4196-a8e5-c2ed62bf89da") dataSourceResources) of
                        Nothing ->
                            Debug.crash "Expected to find related data source with id: de2c5277-1a63-4196-a8e5-c2ed62bf89da"

                        Just resource ->
                            resource

        -- TODO MSP - are we missing the links attributes in the JSON ?
        decodedDataSourceResourceAttributes =
            JsonApi.Resources.attributes DataSource.decoder decodedDataSource
                |> Result.toMaybe
                |> Maybe.withDefault defaultDataSource

        primaryIdIsDecoded =
            \_ -> Expect.equal (JsonApi.Resources.id decodedResource) "948f0330-235c-462f-a8a5-192b924e445b"

        primaryAttributesAreDecoded =
            \_ -> Expect.equal decodedWidget expectedWidget

        relationshipAttributesAreDecoded =
            \_ -> Expect.equal decodedDataSourceResourceAttributes expectedDataSource
    in
        Test.describe "decoding and relationships"
            [ Test.test "it extracts the primary resource id" primaryIdIsDecoded
            , Test.test "it extracts the primary resource attributes" primaryAttributesAreDecoded
            , Test.test "it extracts the relationship attributes" relationshipAttributesAreDecoded
            ]


getDocumentFrom : String -> Document
getDocumentFrom input =
    case decodeString JsonApi.Decode.document input of
        Ok doc ->
            doc

        Err string ->
            Debug.crash string


defaultWidget : Widget.Widget
defaultWidget =
    Widget.Widget
        (Widget.UUID "a08ad2d0-7743-4eaf-906c-bdf11352cfcd")
        "defaultWidget"
        "defaultWidget"
        []
        Adapter.TABLE
        Renderer.TABLE
        []
        expectedDate
        expectedDate
        False
        0
        expectedAuthor


defaultDataSource : DataSource.DataSource
defaultDataSource =
    DataSource.DataSource
        "1234567890"
        "defaultDataSource"


expectedWidget : Widget.Widget
expectedWidget =
    Widget.Widget
        (Widget.UUID "006f0092-5a11-468d-b822-ea57753f45c4")
        "12 months Table"
        "12 months of important data"
        []
        Adapter.TABLE
        Renderer.TABLE
        []
        expectedDate
        expectedDate
        False
        0
        expectedAuthor


expectedDataSource : DataSource.DataSource
expectedDataSource =
    DataSource.DataSource
        "de2c5277-1a63-4196-a8e5-c2ed62bf89da"
        "google spreadsheet Q2"


expectedDatasources =
    [ expectedDataSource ]


expectedDate =
    Date.fromString "2017-09-26T11:07:13.485940Z" |> Result.withDefault (Date.fromTime 0)


expectedAuthor =
    Author.Author
        (User.Username "msp")
        (Just "beautifully flawed creation ..")
        (UserPhoto.UserPhoto <| Just "https://static.productionready.io/images/smiley-cyrus.jpg")
        False
