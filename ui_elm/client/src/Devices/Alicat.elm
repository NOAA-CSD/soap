module Devices.Alicat exposing (..)

{-|


# The Model

@docs Data, Device

-}

import Devices.Device exposing (..)
import Dict
import Json.Decode exposing (..)


{-| `Data` defines the structure of the data that may be returned. As
Alicat devices are intended to provide some measure of flow, they will
always have the field `output`. This value has multiple meanings depending
on the

  - for a pressure meter/controller, this is the pressure
  - for a volumetric/mass flow meter/controller, this is volumetric flow

The structure will define all possible values that can be returned, with the
maximum number of values found in the mass flow controller. The values are
defined as follows:

  - `pressure` - this is the measured pressure in mb
  - `temperature` - measured temperature in degrees Celsius
  - `setpoint` - device setpoint - units are dependent on device type (if it
    is a controller)
  - `mass_flow` - measured mass flow rate in SLPM at T = 273.15 K and P = 1013.25 mb

-}
type alias Data =
    { pressure : Maybe Float
    , temperature : Maybe Float
    , setpoint : Maybe Float
    , mass_flow : Maybe Float
    , output : Float
    }


defaultDictData : Dict.Dict String Data
defaultDictData =
    Dict.singleton "Default_Alicat" (Data Nothing Nothing Nothing Nothing 0)


defaultData : Data
defaultData =
    Data Nothing Nothing Nothing Nothing 0


{-| Type of Alicat device. This will be defined by a query of the device by the server
and returned as part of the CVT.
-}
type Dtype
    = MC
    | M
    | VC
    | V
    | PC
    | P


{-| The `Model` contains two fields:

  - `cvt` - this is the current value table that should be synced with the server.
  - `data` - current list of data corresponding to each connected device

-}



-- TODO: possibly move cvt definition to Device


type alias Model =
    { cvt : Dict.Dict String Device
    , data : Dict.Dict String Data
    }


asCvtIn : Model -> Dict.Dict String Device -> Model
asCvtIn model dict =
    { model | cvt = dict }


insertAlicatDev : DeviceList -> Model -> Model
insertAlicatDev devs model =
    let
        adev =
            List.map
                (\dev ->
                    let
                        device =
                            Tuple.second dev

                        d =
                            if device.type_ == "alicat" then
                                dev
                            else
                                ( "", defaultDevice )
                    in
                    d
                )
                (Dict.toList devs)

        a_dev =
            List.filter checkEmptyDevice adev
    in
    { model | cvt = Dict.fromList a_dev }


init : Model
init =
    { data = defaultDictData, cvt = defaultDeviceDict }


{-| `Msg` defines a single command for updating the setpoint of controllers. The `Msg` `UpdateSetpoint`
accepts two strings - the first defines the id of the device with which we are communicating (the key in
the `Dict`) while the second is a numeric that defines the new setpoint.
-}
type Msg
    = UpdateSetpoint String String
    | SendNewSetpoint String


update : Msg -> Model -> Model
update msg model =
    case msg of
        -- TODO: test to see if device is_controller
        -- TODO: remove is-controller from json as the type will tell you what it is
        UpdateSetpoint id sp ->
            case Dict.get id model.cvt of
                Just device ->
                    let
                        n_device =
                            setSpIn sp device

                        n_dict =
                            Dict.insert id n_device model.cvt
                    in
                    { model | cvt = n_dict }

                Nothing ->
                    model

        SendNewSetpoint id ->
            model



-- BEGIN DECODING JSON


{-| Decode the `Dtype` from a json string. This will provide the device type of the
Alicat.
-}
dtype : Decoder Dtype
dtype =
    let
        helper : String -> Decoder Dtype
        helper s =
            if s == "MC" then
                succeed MC
            else if s == "VC" then
                succeed VC
            else if s == "V" then
                succeed V
            else if s == "M" then
                succeed M
            else if s == "P" then
                succeed P
            else if s == "PC" then
                succeed PC
            else
                fail "Failed to decode dtype"
    in
    string |> andThen helper


decodeData : Decoder Data
decodeData =
    map5 Data
        (maybe (field "P" float))
        (maybe (field "T" float))
        (maybe (field "Qsp" float))
        (maybe (field "Q0" float))
        (field "Q" float)


getAlicatData : Model -> String -> Model
getAlicatData model data =
    let
        test =
            List.map
                (\d ->
                    let
                        id =
                            Tuple.first d

                        d_ =
                            Result.withDefault defaultData
                                (decodeString (field id decodeData) data)
                    in
                    ( id, d_ )
                )
                (Dict.toList
                    model.cvt
                )
    in
    { model | data = Dict.fromList test }
