module Views.DataSource exposing (view)

import Data.DataSource as DataSource exposing (DataSource)
import Html exposing (Html, li, text)
import Html.Attributes exposing (class, id)


view : DataSource -> Html msg
view dataSource =
    li [ class "data-source" ]
        [ text <| "[" ++ dataSource.uuid ++ "] -> " ++ dataSource.name ]
