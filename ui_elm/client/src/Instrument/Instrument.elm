module Instrument.Instrument exposing (..)

import Array exposing (Array)


type alias Data =
    { runningData : Array (Array (Array Float))
    , record_length : Int
    }


type alias Model =
    { data : Data }


asDataIn : Model -> Data -> Model
asDataIn model data =
    { model | data = data }


retrieveData : Model -> Model
retrieveData model =
    model


handleRunningData : Model -> Model
handleRunningData model =
    model


{-| Takes a list of data and appends it to a larger time based list. The size defines
how large the time based list will be.
-}
addDataToList : Int -> List (List Float) -> Model -> Model
addDataToList size newData model =
    let
        -- This is the number of dimensions
        s =
            Array.length model.runningData
    in
        if List.length model.runningData >= size then
            List.append [ newData ] (List.take (size - 1) model.runningData)
        else if List.isEmpty model.runningData then
            [ newData ]
        else
            List.append [ newData ] model.runningData
