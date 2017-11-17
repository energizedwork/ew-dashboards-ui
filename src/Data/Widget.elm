module Data.Widget
    exposing
        ( Widget
        , Body(..)
        , UUID(..)
        , Tag
        , bodyToHtml
        , bodyToMarkdownString
        , decoder
        , decoderWithBody
        , init
        , primaryDataSource
        , slugParser
        , slugToString
        , tagDecoder
        , tagToString
        )

import Data.Widget.Author as Author exposing (Author)
import Data.Widget.Adapters.Adapter as Adapter exposing (Adapter(..))
import Data.Widget.Adapters.MetricAdapter as MetricAdapter exposing (CellConfig, Config, defaultConfig)
import Data.Widget.Renderer as Renderer exposing (Renderer(..))
import Data.Widget.Table as Table exposing (Data)
import Data.DataSource as DataSource exposing (DataSource)
import Date exposing (Date)
import Html exposing (Attribute, Html)
import Json.Decode as Decode exposing (Decoder, index, int, map2)
import Json.Decode.Extra
import Json.Decode.Pipeline as Pipeline exposing (custom, decode, hardcoded, required, optional)
import Markdown
import UrlParser


{-| An widget, optionally with an widget body.

To see the difference between { body : body } and { body : Maybe Body },
consider the difference between the "view individual widget" page (which
renders one widget, including its body) and the "widget feed" -
which displays multiple articles, but without bodies.

This definition for `Widget` means we can write:

viewWidget : Widget Body -> Html msg
viewFeed : List (Widget ()) -> Html msg

This indicates that `viewWidget` requires an widget *with a `body` present*,
wereas `viewFeed` accepts articles with no bodies. (We could also have written
it as `List (Widget a)` to specify that feeds can accept either articles that
have `body` present or not. Either work, given that feeds do not attempt to
read the `body` field from articles.)

This is an important distinction, because in Request.Widget, the `feed`
function produces `List (Widget ())` because the API does not return bodies.
Those articles are useful to the feed, but not to the individual widget view.

-}
type alias Widget a =
    { uuid : UUID
    , name : String
    , description : String
    , dataSources : List DataSource
    , adapter : AdapterDefinition
    , renderer : Renderer
    , tags : List String
    , createdAt : Date
    , updatedAt : Date
    , favorited : Bool
    , favoritesCount : Int
    , author : Author
    , data : Data
    , body : a
    }


type alias AdapterDefinition =
    { type_ : Adapter, config : Config }


init : Widget Body
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
            AdapterDefinition Adapter.TABLE defaultConfig

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

        body =
            Body "Init Widget"
    in
        Widget uuid name description dataSources adapter renderer tags createdAt updatedAt favorited favoritesCount author data body



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
        |> required "data" Table.decoder


adapterDecoder : Decoder AdapterDefinition
adapterDecoder =
    decode AdapterDefinition
        |> required "type_" adapterTypeDecoder
        |> optional "config" adapterConfigDecoder defaultConfig


adapterTypeDecoder : Decoder Adapter
adapterTypeDecoder =
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

                    "METRIC" ->
                        Decode.succeed Adapter.METRIC

                    somethingElse ->
                        Decode.fail <| "Unknown adapter: " ++ somethingElse
            )


adapterConfigDecoder : Decoder Config
adapterConfigDecoder =
    decode Config
        |> required "sourceCell" cellConfigDecoder
        |> required "targetCell" cellConfigDecoder


cellConfigDecoder : Decoder CellConfig
cellConfigDecoder =
    -- Decoding an array to a tuple https://stackoverflow.com/a/47041770
    -- Why is CellConfig not available here? The following line throws a compiler error `Cannot find variable `CellConfig``
    -- But CellConfig is exposed by MetricAdapter 🤔
    -- map2 CellConfig (index 0 int) (index 1 int)
    map2 (,) (index 0 int) (index 1 int)


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


primaryDataSource : Widget a -> DataSource
primaryDataSource widget =
    List.head widget.dataSources |> Maybe.withDefault DataSource.init
