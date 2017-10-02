module Route exposing (Route(..), fromLocation, href, modifyUrl)

import Data.User as User exposing (Username)
import Html exposing (Attribute)
import Html.Attributes as Attr
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, oneOf, parseHash, s, string)
import Data.UUID as UUID


-- ROUTING --


type Route
    = Home
    | Login
    | Logout
    | Register
    | Settings
    | Widget UUID.UUID
    | Profile Username
    | NewWidget
    | EditWidget UUID.UUID


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Home (s "")
        , Url.map Login (s "login")
        , Url.map Logout (s "logout")
        , Url.map Settings (s "settings")
        , Url.map Profile (s "profile" </> User.usernameParser)
        , Url.map Register (s "register")
        , Url.map Widget (s "widget" </> UUID.slugParser)
        , Url.map NewWidget (s "editor")
        , Url.map EditWidget (s "editor" </> UUID.slugParser)
        ]



-- INTERNAL --


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []

                Login ->
                    [ "login" ]

                Logout ->
                    [ "logout" ]

                Register ->
                    [ "register" ]

                Settings ->
                    [ "settings" ]

                Widget slug ->
                    [ "widget", UUID.slugToString slug ]

                Profile username ->
                    [ "profile", User.usernameToString username ]

                NewWidget ->
                    [ "editor" ]

                EditWidget slug ->
                    [ "editor", UUID.slugToString slug ]
    in
        "#/" ++ String.join "/" pieces



-- PUBLIC HELPERS --


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.modifyUrl


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just Home
    else
        parseHash route location
