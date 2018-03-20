module Devices.Device exposing (..)

{-| This is a simple module which contains the basis for communicating with all
serial based devices.


# Definition

@docs Model

-}

import Dict
import Json.Decode exposing (..)


type alias DeviceList =
    Dict.Dict String Device


{-| `Device` contains information regarding the operation of the device itself. This
information should be populated once and then used to populate UI elements and interpret
data retrieved from the server. The data is defined as follows:

  - `address` - single character (A-Z) representing the address of the device for communication
    purposes. Defined here strictly for informational purposes.
  - `dtype` - defines the type of Alicat device we are communicating with. Will define
    how data returned is interpreted.
  - `serial_number` - serial number of device queried by the server. For info purposes only.
  - `label` - provided by server for visualizing data
  - `setpoint` - numeric setpoint of device if the device is a controller
  - `is_controller` - bool representing whether the device accepts a setpoint

-}
type alias Device =
    { type_ : String
    , label : String
    , sn : String
    , controller : Bool
    , address : String
    , model : String
    , active : Bool
    , sp : Maybe String
    }


setSpIn : String -> Device -> Device
setSpIn sp dev =
    { dev | sp = Just sp }


defaultDevice : Device
defaultDevice =
    { type_ = "n\x07"
    , label = "label"
    , sn = "-1"
    , controller = False
    , address = "-1"
    , model = "not_present"
    , active = False
    , sp = Nothing
    }


decodeDevice : Decoder Device
decodeDevice =
    map8 Device
        (field "type" string)
        (field "label" string)
        (field "sn" string)
        (field "controller" bool)
        (field "address" string)
        (field "model" string)
        (field "active" bool)
        (maybe (field "sp" string))


defaultDeviceDict : Dict.Dict String Device
defaultDeviceDict =
    Dict.singleton "null_device"
        (Device "n\x07" "label" "-1" False "-1" "not_present" False Nothing)


decodeDeviceCvt : Decoder DeviceList
decodeDeviceCvt =
    field "device" (dict decodeDevice)


checkEmptyDevice : ( String, Device ) -> Bool
checkEmptyDevice devt =
    Tuple.first devt /= ""



-- TODO: Add a generic decoder here for divying up dict data...
