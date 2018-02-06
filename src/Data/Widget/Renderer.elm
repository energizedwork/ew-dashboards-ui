module Data.Widget.Renderer exposing (Renderer(..), decoder)

import Data.Widget.Config as RendererConfig
import Data.Widget.Definition as Definition
import Json.Decode as Decode exposing (Decoder, index, int, map2, maybe)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)


type Renderer
    = TABLE RendererConfig.Config
    | LINE_CHART
    | BAR_CHART
    | LINE_AND_BAR_CHART
    | HEAT_MAP
    | UPDATABLE_HEAT_MAP
    | METRIC


decoder : Decoder Renderer
decoder =
    Definition.decoder
        |> Decode.andThen
            (\definition ->
                case definition.type_ of
                    "TABLE" ->
                        Decode.succeed <|
                            TABLE
                                (definition.config |> Maybe.withDefault RendererConfig.default)

                    "LINE_CHART" ->
                        Decode.succeed LINE_CHART

                    "BAR_CHART" ->
                        Decode.succeed BAR_CHART

                    "LINE_AND_BAR_CHART" ->
                        Decode.succeed LINE_AND_BAR_CHART

                    "HEAT_MAP" ->
                        Decode.succeed HEAT_MAP

                    "UPDATABLE_HEAT_MAP" ->
                        Decode.succeed UPDATABLE_HEAT_MAP

                    "METRIC" ->
                        Decode.succeed METRIC

                    somethingElse ->
                        Decode.fail <| "Unknown renderer: " ++ somethingElse
            )
