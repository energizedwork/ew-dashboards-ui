module Data.Widget
    exposing
        ( Widget
        , WidgetAttributes
        , Body(..)
        , Tag
        , bodyToHtml
        , bodyToMarkdownString
        , decoder
        , primaryDataSource
        , tagDecoder
        , tagToString
        , defaultAttributes
        , factory
        )

import Data.Widget.Author as Author exposing (Author)
import Data.Widget.Adapters.Adapter as Adapter exposing (Adapter(..))
import Data.Widget.Renderer as Renderer exposing (Renderer(..))
import Data.DataSource as DataSource exposing (DataSource)
import Data.User as User exposing (Username(..))
import Data.UserPhoto as UserPhoto exposing (UserPhoto(..))
import Data.UUID as UUID exposing (UUID)
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


type alias WidgetAttributes =
    { name : String
    , description : String
    , adapter : Adapter
    , renderer : Renderer
    , createdAt : Date
    , updatedAt : Date
    , favorited : Bool
    , favoritesCount : Int
    }


factory : UUID -> WidgetAttributes -> List DataSource -> List String -> Author -> Widget
factory uuid atts dataSources tags author =
    Widget
        uuid
        atts.name
        atts.description
        dataSources
        atts.adapter
        atts.renderer
        tags
        atts.createdAt
        atts.updatedAt
        atts.favorited
        atts.favoritesCount
        author


decoder : Decoder WidgetAttributes
decoder =
    decode WidgetAttributes
        |> required "name" Decode.string
        |> required "description" (Decode.map (Maybe.withDefault "") (Decode.nullable Decode.string))
        |> required "adapter" adapterDecoder
        |> required "renderer" rendererDecoder
        |> required "inserted-at" Json.Decode.Extra.date
        |> required "updated-at" Json.Decode.Extra.date
        |> hardcoded False
        |> hardcoded 0


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


defaultAttributes =
    WidgetAttributes
        "initialised widget attributes"
        "initialised widget attributes"
        Adapter.TABLE
        Renderer.TABLE
        defaultDate
        defaultDate
        False
        0


defaultDate =
    Date.fromString "Mon Jan 01 1976 17:03:55 GMT+0100 (BST)" |> Result.withDefault (Date.fromTime 0)



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
