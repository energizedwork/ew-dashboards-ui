module Route exposing (Route(..), fromLocation, href, modifyUrl)

import Data.Widget as Widget
import Data.User as User exposing (Username)
import Html exposing (Attribute)
import Html.Attributes as Attr
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, oneOf, parseHash, s, string)


-- ROUTING --


type Route
    = Home
    | Login
    | Logout
    | Register
    | Settings
    | Widget Widget.Slug
    | Profile Username
    | NewWidget
    | EditWidget Widget.Slug


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Home (s "")
        , Url.map Login (s "login")
        , Url.map Logout (s "logout")
        , Url.map Settings (s "settings")
        , Url.map Profile (s "profile" </> User.usernameParser)
        , Url.map Register (s "register")
        , Url.map Widget (s "article" </> Widget.slugParser)
        , Url.map NewWidget (s "editor")
        , Url.map EditWidget (s "editor" </> Widget.slugParser)
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
                    [ "article", Widget.slugToString slug ]

                Profile username ->
                    [ "profile", User.usernameToString username ]

                NewWidget ->
                    [ "editor" ]

                EditWidget slug ->
                    [ "editor", Widget.slugToString slug ]
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
