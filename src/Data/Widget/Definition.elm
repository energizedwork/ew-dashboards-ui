module Data.Widget.Definition exposing (decoder)

import Data.Widget.Config as DefinitionConfig
import Json.Decode as Decode exposing (Decoder, Value, dict, maybe, string)
import Json.Decode.Extra
import Json.Decode.Pipeline as Pipeline exposing (custom, decode, hardcoded, optional, required)


type alias Definition =
    { type_ : String
    , config : Maybe DefinitionConfig.Config
    }


decoder : Decoder Definition
decoder =
    decode Definition
        |> required "type_" Decode.string
        |> optional "config"
            (maybe
                (Decode.dict Decode.value)
            )
            Nothing
