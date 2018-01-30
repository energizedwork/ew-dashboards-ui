module Data.UUID
    exposing
        ( UUID(..)
        , slugParser
        , slugToString
        )

import UrlParser


type UUID
    = UUID String


slugParser : UrlParser.Parser (UUID -> a) a
slugParser =
    UrlParser.custom "SLUG" (Ok << UUID)


slugToString : UUID -> String
slugToString (UUID slug) =
    slug
