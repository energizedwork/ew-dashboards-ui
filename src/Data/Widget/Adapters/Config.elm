module Data.Widget.Adapters.Config exposing (Config)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Value)


type alias Config =
    Dict String Value
