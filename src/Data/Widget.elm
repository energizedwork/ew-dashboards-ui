module Data.Widget
    exposing
        ( Widget
        , Body
        , UUID
        , Tag
        , bodyToHtml
        , bodyToMarkdownString
        , decoder
        , decoderWithBody
        , slugParser
        , slugToString
        , tagDecoder
        , tagToString
        )

import Data.Widget.Author as Author exposing (Author)
import Data.Widget.Adapter exposing (Adapter(..))
import Data.Widget.Renderer exposing (Renderer(..))
import Data.DataSource as DataSource exposing (DataSource)
import Date exposing (Date)
import Html exposing (Attribute, Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra
import Json.Decode.Pipeline as Pipeline exposing (custom, decode, hardcoded, required)
import Markdown
import UrlParser


{-| An article, optionally with an article body.

To see the difference between { body : body } and { body : Maybe Body },
consider the difference between the "view individual article" page (which
renders one article, including its body) and the "article feed" -
which displays multiple articles, but without bodies.

This definition for `Widget` means we can write:

viewWidget : Widget Body -> Html msg
viewFeed : List (Widget ()) -> Html msg

This indicates that `viewWidget` requires an article _with a `body` present_,
wereas `viewFeed` accepts articles with no bodies. (We could also have written
it as `List (Widget a)` to specify that feeds can accept either articles that
have `body` present or not. Either work, given that feeds do not attempt to
read the `body` field from articles.)

This is an important distinction, because in Request.Widget, the `feed`
function produces `List (Widget ())` because the API does not return bodies.
Those articles are useful to the feed, but not to the individual article view.

-}
type alias Widget a =
    { uuid : UUID
    , name : String
    , description : String
    , dataSources : List DataSource
    , adapter : Adapter
    , renderer : Renderer
    , tags : List String
    , createdAt : Date
    , updatedAt : Date
    , favorited : Bool
    , favoritesCount : Int
    , author : Author
    , body : a
    }



-- SERIALIZATION --


decoder : Decoder (Widget ())
decoder =
    baseWidgetDecoder
        |> hardcoded ()


decoderWithBody : Decoder (Widget Body)
decoderWithBody =
    baseWidgetDecoder
        |> required "body" bodyDecoder


baseWidgetDecoder : Decoder (a -> Widget a)
baseWidgetDecoder =
    decode Widget
        |> required "uuid" (Decode.map UUID Decode.string)
        |> required "name" Decode.string
        |> required "description" (Decode.map (Maybe.withDefault "") (Decode.nullable Decode.string))
        |> required "dataSources" (Decode.list DataSource.decoder)
        |> required "adapter" adapterDecoder
        |> required "renderer" rendererDecoder
        |> required "tagList" (Decode.list Decode.string)
        |> required "createdAt" Json.Decode.Extra.date
        |> required "updatedAt" Json.Decode.Extra.date
        |> required "favorited" Decode.bool
        |> required "favoritesCount" Decode.int
        |> required "author" Author.decoder


adapterDecoder : Decoder Adapter
adapterDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "2D" ->
                        Decode.succeed TWO_D

                    "XY" ->
                        Decode.succeed XY

                    somethingElse ->
                        Decode.fail <| "Unknown adapter: " ++ somethingElse
            )


rendererDecoder : Decoder Renderer
rendererDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "TABLE" ->
                        Decode.succeed TABLE

                    "LINE" ->
                        Decode.succeed LINE

                    somethingElse ->
                        Decode.fail <| "Unknown renderer: " ++ somethingElse
            )



-- IDENTIFIERS --


type UUID
    = UUID String


slugParser : UrlParser.Parser (UUID -> a) a
slugParser =
    UrlParser.custom "SLUG" (Ok << UUID)


slugToString : UUID -> String
slugToString (UUID slug) =
    slug



-- TAGS --


type Tag
    = Tag String


tagToString : Tag -> String
tagToString (Tag slug) =
    slug


tagDecoder : Decoder Tag
tagDecoder =
    Decode.map Tag Decode.string



-- BODY --


type Body
    = Body Markdown


type alias Markdown =
    String


bodyToHtml : Body -> List (Attribute msg) -> Html msg
bodyToHtml (Body markdown) attributes =
    Markdown.toHtml attributes markdown


bodyToMarkdownString : Body -> String
bodyToMarkdownString (Body markdown) =
    markdown


bodyDecoder : Decoder Body
bodyDecoder =
    Decode.map Body Decode.string
