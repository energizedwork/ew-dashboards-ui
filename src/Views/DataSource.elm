module Views.DataSource exposing (view)

import Data.DataSource as DataSource exposing (DataSource)
import Html exposing (Html, li, text)
import Html.Attributes exposing (class, id)
import Data.UUID as UUID


view : DataSource -> Html msg
view dataSource =
    li [ class "data-source" ]
        [ text <| DataSource.toChannel (dataSource) ]
