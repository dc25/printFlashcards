module Msg exposing (..)
import Ports exposing (FileLoadedData)

type Msg
    = FileSelected
    | FileLoaded FileLoadedData
    | SetRows String
    | SetCols String


