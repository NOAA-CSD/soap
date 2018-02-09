module Devices.Vaisala exposing (..)

import Devices.Device exposing (..)
import Dict
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


type alias Model =
    { cvt : DictCvt
    , data : DictData
    }


type alias Data =
    { temperature : Float
    , relative_humidity : Float
    , dewpoint : Float
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
    Dict.singleton "NullData" (Data 0 0 0)


init : Model
init =
    Model defaultCvt defaultData


decodeData : Decoder Data
decodeData =
    decode Data
        |> required "t" float
        |> required "rh" float
        |> required "td" float


insertVaisalaDev : DeviceList -> Model -> Model
insertVaisalaDev devs model =
    let
        vdev =
            List.map
                (\dev ->
                    let
                        device =
                            Tuple.second dev

                        d =
                            if device.type_ == "vaisala" then
                                dev
                            else
                                ( "", defaultDevice )
                    in
                    d
                )
                (Dict.toList devs)

        v_dev =
            List.filter checkEmptyDevice vdev
    in
    { model | cvt = Dict.fromList v_dev }


getVaisalaData : Model -> String -> Model
getVaisalaData model data =
    let
        test =
            List.map
                (\d ->
                    let
                        id =
                            Tuple.first d

                        d_ =
                            Result.withDefault (Data 0 0 0)
                                (decodeString (field id decodeData) data)
                    in
                    ( id, d_ )
                )
                (Dict.toList
                    model.cvt
                )
    in
    { model | data = Dict.fromList test }
