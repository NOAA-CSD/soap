module Devices.Ppt exposing (..)

import Devices.Device exposing (..)
import Dict
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


type alias Model =
    { cvt : DictCvt
    , data : DictData
    }


type alias Data =
    { pressure : Float
    , temperature : Float
    }


type alias DictData =
    Dict.Dict String Data


type alias DictCvt =
    Dict.Dict String Device


defaultCvt : DictCvt
defaultCvt =
    Dict.singleton "NullDevice" defaultDevice


defaultData : DictData
defaultData =
    Dict.singleton "NullData" (Data 0 0)


init : Model
init =
    Model defaultCvt defaultData


decodeData : Decoder Data
decodeData =
    decode Data
        |> required "p" float
        |> required "t" float


insertPptDev : DeviceList -> Model -> Model
insertPptDev devs model =
    let
        pdev =
            List.map
                (\dev ->
                    let
                        device =
                            Tuple.second dev

                        d =
                            if device.type_ == "ppt" then
                                dev
                            else
                                ( "", defaultDevice )
                    in
                    d
                )
                (Dict.toList devs)

        dev_ =
            List.filter checkEmptyDevice pdev
    in
    { model | cvt = Dict.fromList dev_ }


getPptData : String -> Model -> Model
getPptData data model =
    let
        d =
            List.map
                (\d ->
                    let
                        id =
                            Tuple.first d

                        d_ =
                            Result.withDefault (Data 0 0)
                                (decodeString (field id decodeData) data)
                    in
                    ( id, d_ )
                )
                (Dict.toList
                    model.cvt
                )
    in
    { model | data = Dict.fromList d }
