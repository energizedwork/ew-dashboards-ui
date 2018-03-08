module Data.Widget.Renderer exposing (Renderer(..), decoder)

import Data.Widget.Config as RendererConfig
import Data.Widget.Definition as Definition
import Json.Decode as Decode exposing (Decoder, index, int, map2, maybe)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)


type Renderer
    = TABLE RendererConfig.Config
    | LINE_CHART RendererConfig.Config
    | BAR_CHART RendererConfig.Config
    | BAR_CHART_HORIZONTAL RendererConfig.Config
    | LINE_AND_BAR_CHART RendererConfig.Config
    | HEAT_MAP
    | UPDATABLE_HEAT_MAP
    | METRIC RendererConfig.Config
    | PIE_CHART RendererConfig.Config


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
                        Decode.succeed <|
                            LINE_CHART
                                (definition.config |> Maybe.withDefault RendererConfig.default)

                    "BAR_CHART" ->
                        Decode.succeed <|
                            BAR_CHART
                                (definition.config |> Maybe.withDefault RendererConfig.default)

                    "BAR_CHART_HORIZONTAL" ->
                        Decode.succeed <|
                            BAR_CHART_HORIZONTAL
                                (definition.config |> Maybe.withDefault RendererConfig.default)

                    "LINE_AND_BAR_CHART" ->
                        Decode.succeed <|
                            LINE_AND_BAR_CHART
                                (definition.config |> Maybe.withDefault RendererConfig.default)

                    "HEAT_MAP" ->
                        Decode.succeed HEAT_MAP

                    "UPDATABLE_HEAT_MAP" ->
                        Decode.succeed UPDATABLE_HEAT_MAP

                    "METRIC" ->
                        Decode.succeed <|
                            METRIC
                                (definition.config |> Maybe.withDefault RendererConfig.default)

                    "PIE_CHART" ->
                        Decode.succeed <|
                            PIE_CHART
                                (definition.config |> Maybe.withDefault RendererConfig.default)

                    somethingElse ->
                        Decode.fail <| "Unknown renderer: " ++ somethingElse
            )
