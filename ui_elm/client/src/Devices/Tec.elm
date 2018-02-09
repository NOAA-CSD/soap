module Devices.Tec exposing (..)

{-|This module supports the client side representation of the thermoelectric coolers
used in the gCRD-PAS.  These TECs are run using PWM and run off of a software based
PID control loop.  The user will request a setpoint and the signal will be modulated
based upon the measured temperature that is returned (found in `model`).  There are
multiple TECs and this package handles all via a `Dict` for the data and the CVT data..-}

import Json.Decode as Decoder exposing (Decoder, field)

import Dict

type alias Cvt =
    {
        label: String,
        setpoint:String,
        p: String,
        i: String,
        d: String,
        power: Bool
    }

type alias Model =
    {
        data: Dict.Dict String Float,
        cvt: Dict.Dict String Cvt
    }

type Msg = UpdateP String String |
            UpdateI String String |
            UpdateD String String |
            UpdateSp String String |
            SendCurrentValues String |
            TogglePower String

defaultCvt: Cvt
defaultCvt = Cvt "Default Tec" "18" "1" "0.1" "0" False

init: Model
init = Model (Dict.singleton "Default_Tec" 0) (Dict.singleton "Default TEC" defaultCvt)

-- TODO: Make sure to check if the id exists and return the model ONLY if it doesn't; right now it is fragile
update: Msg -> Model -> Model
update msg model =
    case msg of
        UpdateP id p->
            let
                data = Maybe.withDefault defaultCvt (Dict.get id model.cvt)
                d = {data | p = p}
                cvt_ = Dict.insert id d model.cvt
            in
                {model | cvt = cvt_}
        UpdateI id i->
            let
                data = Maybe.withDefault defaultCvt (Dict.get id model.cvt)
                d = {data | i = i}
                cvt_ = Dict.insert id d model.cvt
            in
                {model | cvt = cvt_}
        UpdateD id d->
            let
                data = Maybe.withDefault defaultCvt (Dict.get id model.cvt)
                data_ = {data | d = d}
                cvt_ = Dict.insert id data_ model.cvt
            in
                {model | cvt = cvt_}
        UpdateSp id sp->
            let
                data = Maybe.withDefault defaultCvt (Dict.get id model.cvt)
                d = {data | setpoint = sp}
                cvt_ = Dict.insert id d model.cvt
            in
                {model | cvt = cvt_}
        SendCurrentValues id ->
            model
        TogglePower id ->
                    case Dict.get id model.cvt of
                        Just d->
                            let
                                n_data = {d | power = not d.power}
                                cvt_ = Dict.insert id n_data model.cvt
                            in
                                {model | cvt = cvt_}
                        Nothing ->
                            model

-- SETUP DECODERS

{-| Decode the individual elements for the CVT. -}
decodeCvt: Decoder Cvt
decodeCvt =
    Decoder.map6 Cvt
        (field "label" Decoder.string)
        (field "sp" Decoder.string)
        (field "p" Decoder.string)
        (field "i" Decoder.string)
        (field "d" Decoder.string)
        (field "power" Decoder.bool)

{-| Create a tuple representing the key and value of a dictionary list.  The element
`id` can be found in the json for each TEC element.
-}
idAndDevice: Decoder (String, Cvt)
idAndDevice =
    Decoder.map2 (,)
        (field "id" Decoder.string)
        decodeCvt

{-| This is called by the outside world to update the client side CVT for the
TEC.-}
tecCvt: Decoder (Dict.Dict String Cvt)
tecCvt =
    Decoder.list idAndDevice |> Decoder.map Dict.fromList

idAndDeviceData: Decoder (String, Float)
idAndDeviceData =
    Decoder.map2 (,)
        (field "id" Decoder.string)
        (field "val" Decoder.float)

{-|This is the public facing function for decoding the data.-}
tecData: Decoder (Dict.Dict String Float)
tecData =
    Decoder.list idAndDeviceData |> Decoder.map Dict.fromList
