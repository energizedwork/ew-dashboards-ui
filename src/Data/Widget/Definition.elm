module Data.Widget.Definition exposing (decoder, Definition)

import Data.Widget.Config as AdapterConfig
import Json.Decode as Decode exposing (Decoder, Value, dict, maybe, string)
import Json.Decode.Pipeline as Pipeline exposing (custom, decode, hardcoded, optional, required)


-- Public ----------------------------------------------------------------------


type alias Definition =
    { type_ : String
    , config : Maybe AdapterConfig.Config
    }


decoder : Decoder Definition
decoder =
    decode Definition
        |> required "type_" Decode.string
        |> optional "config"
            (maybe
                AdapterConfig.decoder
            )
            Nothing



-- Private ---------------------------------------------------------------------
