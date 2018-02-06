module Data.WidgetDecoderTest exposing (..)

import Data.DataSource exposing (DataSource)
import Data.UUID as UUID
import Data.User as User exposing (Username(..))
import Data.UserPhoto as UserPhoto exposing (UserPhoto(..))
import Data.Widget as Widget exposing (Body(..), decoder)
import Data.Widget.Adapters.Adapter as Adapter exposing (Adapter(..))
import Data.Widget.Adapters.CellPosition as CellPosition exposing (CellPosition(..), decoder, encode)
import Data.Widget.Adapters.CellRange as CellRange exposing (..)
import Data.Widget.Adapters.TableAdapter as TableAdapter
import Data.Widget.Author as Author
import Data.Widget.Config as Config
import Data.Widget.Renderer as Renderer exposing (Renderer(..))
import Date
import Dict
import Expect exposing (Expectation)
import Json.Decode as Decode exposing (..)
import Json.Encode exposing (encode)
import Test exposing (..)


widgetDecoderTest : Test
widgetDecoderTest =
    test "Widget.decoder" <|
        \() ->
            let
                input =
                    """
                    {
                        "widget": {
                            "uuid": "006f0092-5a11-468d-b822-ea57753f45c4",
                            "name": "12 months Table",
                            "description": "12 months of important data",
                            "dataSources": [{
                                "uuid": "datasource-1234",
                                "name": "12 month financials"
                            }],
                            "adapter": {
                                "type_":"TABLE",
                                "config": {
                                  "xLabels": {
                                    "start": [4, 176],
                                    "end": [15, 176]
                                  },
                                  "bodyRows": {
                                    "start": [4, 177],
                                    "end": [15, 178]
                                  }
                                }
                            },
                            "renderer": {
                                "type_":"TABLE",
                                "config": {
                                  "colSpan": 12
                                }
                            },
                            "createdAt": "2017-09-04T16:03:55.948Z",
                            "updatedAt": "2017-09-04T16:03:55.948Z",
                            "tagList": [],
                            "author": {
                                "username": "msp",
                                "bio": "beautifully flawed creation ..",
                                "image": "https://static.productionready.io/images/smiley-cyrus.jpg",
                                "following": false
                            },
                            "favorited": false,
                            "favoritesCount": 0,
                            "data": {
                              "data": []
                            }
                        }
                    }
                    """

                expectedDate =
                    Date.fromString "Mon Sep 04 2017 17:03:55 GMT+0100 (BST)" |> Result.withDefault (Date.fromTime 0)

                expectedAuthor =
                    Author.Author
                        (User.Username "msp")
                        (Just "beautifully flawed creation ..")
                        (UserPhoto.UserPhoto <| Just "https://static.productionready.io/images/smiley-cyrus.jpg")
                        False

                expectedDatasources =
                    [ DataSource "datasource-1234" "12 month financials" ]

                xLabelsRange =
                    (CellRange
                        (CellPosition ( 4, 176 ))
                        (CellPosition ( 15, 176 ))
                    )

                bodyRowsRange =
                    (CellRange
                        (CellPosition ( 4, 177 ))
                        (CellPosition ( 15, 178 ))
                    )

                expectedAdapterConfig =
                    Dict.fromList
                        [ ( "xLabels", CellRange.encode xLabelsRange )
                        , ( "bodyRows", CellRange.encode bodyRowsRange )
                        ]

                expectedRendererConfig =
                    Dict.fromList
                        [ ( "colSpan", (Json.Encode.int 12) )
                        ]

                decodedOutput =
                    Decode.decodeString (Widget.decoder |> Decode.field "widget") input
            in
                -- TODO - hmm, why do I need the toString :/
                Expect.equal (toString decodedOutput)
                    (toString
                        (Ok
                            { uuid = UUID.UUID "006f0092-5a11-468d-b822-ea57753f45c4"
                            , name = "12 months Table"
                            , description = "12 months of important data"
                            , dataSources = expectedDatasources
                            , adapter = Adapter.TABLE expectedAdapterConfig
                            , renderer = Renderer.TABLE expectedRendererConfig
                            , tags = []
                            , createdAt = expectedDate
                            , updatedAt = expectedDate
                            , favorited = False
                            , favoritesCount = 0
                            , author = expectedAuthor
                            , data = { rows = [] }
                            }
                        )
                    )
