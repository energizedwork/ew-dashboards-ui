module Data.Widget.Adapters.MetricAdapter exposing (defaultConfig, adapt)

import Data.Widget.Adapters.CellPosition as CellPosition exposing (CellPosition(..), encode, decoder, defaultPosition)
import Data.Widget.Config as AdapterConfig
import Data.Widget.Table as Table exposing (Data)
import Array
import Dict exposing (Dict)
import Json.Decode as Json exposing (Value)


defaultConfig : Dict String Json.Value
defaultConfig =
    Dict.fromList
        [ ( "subtitleCell", CellPosition.encode defaultPosition )
        , ( "actualCell", CellPosition.encode defaultPosition )
        , ( "targetCell", CellPosition.encode defaultPosition )
        , ( "changeCell", CellPosition.encode defaultPosition )
        , ( "lastUpdatedCell", CellPosition.encode defaultPosition )
        ]


rowForCell : Array.Array (List String) -> CellPosition -> List String
rowForCell rows cellPosition =
    case Array.get ((CellPosition.y cellPosition) - 1) rows of
        Just row ->
            row

        Nothing ->
            []


valueForCell : Array.Array (List String) -> AdapterConfig.Config -> String -> String
valueForCell rows config key =
    let
        cellPosition =
            Dict.get key config
                |> Maybe.withDefault (CellPosition.encode defaultPosition)
                |> Json.decodeValue CellPosition.decoder
                |> Result.withDefault defaultPosition

        row =
            rowForCell rows cellPosition
    in
        case Array.get ((CellPosition.x cellPosition) - 1) (Array.fromList row) of
            Just value ->
                value

            Nothing ->
                ""


adapt : AdapterConfig.Config -> Data -> ( String, String, String, String, String )
adapt optionalConfig data =
    let
        rows =
            Array.fromList data.rows

        subtitleValue =
            valueForCell rows optionalConfig "subtitleCell"

        actualValue =
            valueForCell rows optionalConfig "actualCell"

        targetValue =
            valueForCell rows optionalConfig "targetCell"

        changeValue =
            valueForCell rows optionalConfig "changeCell"

        lastUpdatedValue =
            valueForCell rows optionalConfig "lastUpdatedCell"
    in
        ( subtitleValue, actualValue, targetValue, changeValue, lastUpdatedValue )
