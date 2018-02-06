module Data.Widget.Config exposing (Config, default)

import Dict exposing (Dict)
import Json.Decode as Json exposing (Value)


type alias Config =
    Dict String Value


default : Dict String Json.Value
default =
    Dict.empty
