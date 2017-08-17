import Csv exposing (..)
import Html exposing (..)
import Ports exposing (fileContentRead, fileSelected)
import Msg exposing (..)
import Model exposing (Model, init)
import View exposing (view)

-- MAIN

main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    fileContentRead FileLoaded

stringToCount minVal curVal maxVal s =
    case String.toInt s of
        Err _ -> curVal
        Ok v ->
            ( case (v < minVal, v > maxVal) of
                  (True, False) -> minVal
                  (False, True) -> maxVal
                  _ -> v
            ) 

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        SetRows cs -> 
             ({model | curRows = stringToCount model.minRows model.curRows model.maxRows cs }, Cmd.none)

        SetCols cs -> 
             ({model | curCols = stringToCount model.minCols model.curCols model.maxCols cs }, Cmd.none)

        FileSelected ->
            ( model, fileSelected "CSVInput" )

        FileLoaded fileContents ->
            case Csv.parse fileContents of
                Ok results ->
                    ( { model | csvData = results, errors = Nothing }, Cmd.none )

                Err errors ->
                    ( { model | errors = Just errors }, Cmd.none )

