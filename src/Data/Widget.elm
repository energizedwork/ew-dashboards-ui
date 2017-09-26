module Data.Widget
    exposing
        ( Widget
        , Body(..)
        , UUID(..)
        , Tag
        , bodyToHtml
        , bodyToMarkdownString
        , decoder
        , primaryDataSource
        , slugParser
        , slugToString
        , tagDecoder
        , tagToString
        )

import Data.Widget.Author as Author exposing (Author)
import Data.Widget.Adapters.Adapter as Adapter exposing (Adapter(..))
import Data.Widget.Renderer as Renderer exposing (Renderer(..))
import Data.DataSource as DataSource exposing (DataSource)
import Data.User as User exposing (Username(..))
import Data.UserPhoto as UserPhoto exposing (UserPhoto(..))
import Date exposing (Date)
import Html exposing (Attribute, Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra
import Json.Decode.Pipeline as Pipeline exposing (custom, decode, hardcoded, required, optional)
import Markdown
import UrlParser


type alias Widget =
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
    }



-- SERIALIZATION --


decoder : Decoder Widget
decoder =
    baseWidgetDecoder


baseWidgetDecoder : Decoder Widget
baseWidgetDecoder =
    decode Widget
        |> hardcoded (UUID "006f0092-5a11-468d-b822-ea57753f45c4")
        -- TODO!
        |>
            required "name" Decode.string
        |> required "description" (Decode.map (Maybe.withDefault "") (Decode.nullable Decode.string))
        |> hardcoded []
        |> required "adapter" adapterDecoder
        |> required "renderer" rendererDecoder
        |> hardcoded []
        |> required "inserted-at" Json.Decode.Extra.date
        |> required "updated-at" Json.Decode.Extra.date
        |> hardcoded False
        |> hardcoded 0
        |> hardcoded defaultAuthor


defaultDate =
    Date.fromString "Mon Jan 01 1976 17:03:55 GMT+0100 (BST)" |> Result.withDefault (Date.fromTime 0)


defaultAuthor =
    Author.Author
        (User.Username "msp")
        (Just "beautifully flawed creation ..")
        (UserPhoto.UserPhoto <| Just "https://static.productionready.io/images/smiley-cyrus.jpg")
        False


adapterDecoder : Decoder Adapter
adapterDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "TABLE" ->
                        Decode.succeed Adapter.TABLE

                    "BAR_CHART" ->
                        Decode.succeed Adapter.BAR_CHART

                    "HEAT_MAP" ->
                        Decode.succeed Adapter.HEAT_MAP

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
                        Decode.succeed Renderer.TABLE

                    "LINE_CHART" ->
                        Decode.succeed Renderer.LINE_CHART

                    "BAR_CHART" ->
                        Decode.succeed Renderer.BAR_CHART

                    "HEAT_MAP" ->
                        Decode.succeed Renderer.HEAT_MAP

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


primaryDataSource : Widget a -> DataSource
primaryDataSource widget =
    List.head widget.dataSources |> Maybe.withDefault DataSource.init
