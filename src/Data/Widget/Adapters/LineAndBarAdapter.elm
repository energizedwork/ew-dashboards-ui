module Data.Widget.Adapters.LineAndBarAdapter exposing (adapt, defaultConfig)

import Data.Widget.Adapters.ChartAdapter as ChartAdapter exposing (..)
import Data.Widget.Adapters.TableAdapter exposing (Orientation(..))
import Data.Widget.Chart as Chart exposing (Data)
import Data.Widget.Config as AdapterConfig
import Data.Widget.Table as Table exposing (Cell, Data, Row)
import Dict exposing (Dict)
import Json.Decode as Json exposing (..)


-- Public ----------------------------------------------------------------------


adapt : AdapterConfig.Config -> Table.Data -> ( Chart.Data, Chart.Data )
adapt combinedConfig data =
    let
        -- TODO
        -- Certain config should be able to override specific bar or chart config
        -- at top level in JSON e.g. xLabels ?
        lineChartData =
            ChartAdapter.adapt
                (AdapterConfig.at "line" combinedConfig)
                data
                Vertical

        barChartData =
            ChartAdapter.adapt
                (AdapterConfig.at "bar" combinedConfig)
                data
                Vertical
    in
        ( lineChartData, barChartData )


defaultConfig : Dict String Json.Value
defaultConfig =
    Dict.empty



-- Private ---------------------------------------------------------------------
