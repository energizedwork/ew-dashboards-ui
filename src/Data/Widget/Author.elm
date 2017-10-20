module Data.Widget.Author exposing (Author, AuthorAttributes, decoder, defaultAttributes, factory)

import Data.User as User exposing (Username)
import Data.UserPhoto as UserPhoto exposing (UserPhoto)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (custom, decode, required, hardcoded)


type alias Author =
    { username : Username
    , bio : Maybe String
    , image : UserPhoto
    , following : Bool
    }


type alias AuthorAttributes =
    { username : Username
    , image : UserPhoto
    }


factory : AuthorAttributes -> Author
factory atts =
    Author
        atts.username
        Nothing
        atts.image
        False


decoder : Decoder AuthorAttributes
decoder =
    decode AuthorAttributes
        |> required "username" User.usernameDecoder
        |> required "image-src" UserPhoto.decoder


defaultAttributes =
    AuthorAttributes
        (User.Username "default-author")
        (UserPhoto.UserPhoto <| Just "https://static.productionready.io/images/smiley-cyrus.jpg")
