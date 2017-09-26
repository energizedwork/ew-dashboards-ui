module Data.Widget
    exposing
        ( Widget
        , Body(..)
        , UUID(..)
        , Tag
        , bodyToHtml
        , bodyToMarkdownString
        , decoder
        , init
        , primaryDataSource
        , slugParser
        , slugToString
        , tagDecoder
        , tagToString
        )

import Data.DataSource as DataSource exposing (DataSource)
import Data.Widget.Adapters.Adapter as Adapter exposing (Adapter(..))
import Data.Widget.Adapters.TableAdapter as TableAdapter
import Data.Widget.Author as Author exposing (Author)
import Data.Widget.Renderer as Renderer exposing (Renderer(..))
import Data.Widget.Table as Table exposing (Data)
import Date exposing (Date)
import Html exposing (Attribute, Html)
import Json.Decode as Decode exposing (Decoder, index, int, map2, maybe)
import Json.Decode.Extra
import Json.Decode.Pipeline as Pipeline exposing (custom, decode, hardcoded, optional, required)
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
    , data : Data
    }


init : Widget
init =
    let
        uuid =
            UUID "not-so-unique"

        name =
            "Init Widget"

        description =
            "Init Widget"

        dataSources =
            [ DataSource.init ]

        adapter =
            Adapter.TABLE TableAdapter.defaultConfig

        renderer =
            Renderer.TABLE

        tags =
            []

        createdAt =
            Date.fromString "2017-01-01T11:07:13.485940Z" |> Result.withDefault (Date.fromTime 0)

        updatedAt =
            Date.fromString "2017-01-01T11:07:13.485940Z" |> Result.withDefault (Date.fromTime 0)

        favorited =
            False

        favoritesCount =
            0

        author =
            Author.init

        data =
            Data []
    in
        Widget uuid name description dataSources adapter renderer tags createdAt updatedAt favorited favoritesCount author data



-- SERIALIZATION --


decoder : Decoder Widget
decoder =
    decode Widget
        |> required "uuid" (Decode.map UUID Decode.string)
        |> required "name" Decode.string
        |> required "description" (Decode.map (Maybe.withDefault "") (Decode.nullable Decode.string))
        |> required "dataSources" (Decode.list DataSource.decoder)
        |> required "adapter" Adapter.decoder
        |> required "renderer" rendererDecoder
        |> required "tagList" (Decode.list Decode.string)
        |> required "createdAt" Json.Decode.Extra.date
        |> required "updatedAt" Json.Decode.Extra.date
        |> required "favorited" Decode.bool
        |> required "favoritesCount" Decode.int
        |> required "author" Author.decoder
        |> required "data" Table.decoder


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

                    "LINE_AND_BAR_CHART" ->
                        Decode.succeed Renderer.LINE_AND_BAR_CHART

                    "HEAT_MAP" ->
                        Decode.succeed Renderer.HEAT_MAP

                    "UPDATABLE_HEAT_MAP" ->
                        Decode.succeed Renderer.UPDATABLE_HEAT_MAP

                    "METRIC" ->
                        Decode.succeed Renderer.METRIC

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


primaryDataSource : Widget -> DataSource
primaryDataSource widget =
    List.head widget.dataSources |> Maybe.withDefault DataSource.init
