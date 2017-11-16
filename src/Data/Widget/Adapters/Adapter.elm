module Data.Widget.Adapters.Adapter exposing (Adapter(..))

-- TODO these are hardcoded for now but will likely have to be a set of
-- defaults plus optional user defined adapters
-- e.g. "use row 3 for headings & row 4-6 for data"


type Adapter
    = TABLE
    | BAR_CHART
    | HEAT_MAP
    | METRIC
