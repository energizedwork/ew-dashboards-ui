module Data.Widget.Config exposing (at, Config, default, decoder)

import Dict exposing (Dict)
import Json.Decode as Json exposing (..)
import Json.Encode as Encode exposing (object)


-- Public ----------------------------------------------------------------------


type alias Config =
    Dict String Value


default : Dict String Json.Value
default =
    Dict.empty


decoder : Decoder Config
decoder =
    Json.dict Json.value


at : String -> Config -> Config
at key combinedConfig =
    let
        nestedJSON =
            (Dict.get key combinedConfig)
                |> Maybe.withDefault (Encode.string "{}")
    in
        Json.decodeValue decoder nestedJSON
            |> Result.withDefault default



-- Private ---------------------------------------------------------------------
