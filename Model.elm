module Model exposing (Model, init)
import Csv exposing (Csv)
import Msg exposing (Msg)

init : (Model, Cmd Msg)
init = (Model 1 30 10 1 10 3 (Csv [] [ ]) Nothing, Cmd.none) 

type alias Model =
    { minRows : Int
    , maxRows : Int
    , curRows : Int

    , minCols : Int
    , maxCols : Int
    , curCols : Int

    , csvData : Csv
    , errors : Maybe (List String) 
    }
