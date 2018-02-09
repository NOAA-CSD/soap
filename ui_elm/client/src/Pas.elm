module Pas exposing (..)

{-| This is a package that contains all of the information required for operation of the photoacoustic spectrometer.
This includes both the control and display of the photoacoustic data.


# Display


# Control

-}

import Array exposing (Array)
import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


type alias Model =
    { cvt : PasCvt
    , data : PasData
    }


{-| -}
init : Model
init =
    { cvt = defaultCvt, data = defaultPasData }


{-| Update messages for the PAS
-}
type Msg
    = UpdateFreq Int String
    | HandleGeneric (Result Http.Error String)
    | UpdateSpkVscale String
    | UpdateSpkVoffset String
    | UpdateSpkDf String
    | UpdateSpkFcenter String
    | UpdateSpkPeriod String
    | UpdateSpkLength String



-- TODO: Add code for changing these values on the server side...


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateFreq cell freq ->
            {- let

                   f = Result.withDefault 0 (String.toInt freq)
                   d =
                       case (Array.get cell model.cvt) of
                           Nothing ->
                               PasCellCvt "Default Label" False Speaker 1350
                           Just val ->
                               val

                   new_d = {d | frequency = f}
                   cvt_ = Array.set cell new_d model.cvt
               in
                   {model | cvt = cvt_ }
            -}
            model

        HandleGeneric _ ->
            model

        UpdateSpkVscale v ->
            let
                new_model =
                    model.cvt.spk
                        |> setSpkVscale v
                        |> asSpeakerIn model.cvt
                        |> asCvtIn model
            in
            new_model

        UpdateSpkVoffset v ->
            let
                new_model =
                    model.cvt.spk
                        |> setVoffsetSpk v
                        |> asSpeakerIn model.cvt
                        |> asCvtIn model
            in
            new_model

        UpdateSpkDf df ->
            let
                new_model =
                    model.cvt.spk
                        |> setDfSpk df
                        |> asSpeakerIn model.cvt
                        |> asCvtIn model
            in
            new_model

        UpdateSpkFcenter fcenter ->
            let
                new_model =
                    model.cvt.spk
                        |> setFcenterSpk fcenter
                        |> asSpeakerIn model.cvt
                        |> asCvtIn model
            in
            new_model

        UpdateSpkLength len ->
            let
                new_model =
                    model.cvt.spk
                        |> setLengthSpk len
                        |> asSpeakerIn model.cvt
                        |> asCvtIn model
            in
            new_model

        UpdateSpkPeriod period ->
            let
                new_model =
                    model.cvt.spk
                        |> setPeriodSpk period
                        |> asSpeakerIn model.cvt
                        |> asCvtIn model
            in
            new_model


{-| The type `Drive` is used to indicate whether the PAS is operating the laser or speaker.
-}
type Drive
    = Laser
    | Speaker


type alias PasData =
    { cell : Array PasCell
    , drive : Drive
    }


{-| Definition of the PAS data that is returned by cell.
-}
type alias PasCell =
    { resonant_frequency : Float
    , integrated_area : Float
    , q : Float
    , max : List Float
    , absorption : Float
    , laserRMS : Float
    , laserDiodeData : List Int
    , frequencyData : List Float
    , timeData : List Int
    , fittedData : List Float
    , iaBackground : Float
    }


type alias PasCvt =
    { spk : Speaker_
    , b : List Float
    , m : List Float
    , fmod_0 : Int
    , fmod_1 : Int
    , heater_0 : Heater
    , heater_1 : Heater
    , enable_0 : Bool
    , enable_1 : Bool
    }



type alias Speaker_ =
    { period : String
    , length : String
    , auto : Bool
    , vscale : String
    , voffset : String
    , center : String
    , df : String
    , pos : Drive
    }

type alias Heater =
    { pid : Array Float
    , sp : Float
    }


decodeCellData : Decoder PasCell
decodeCellData =
    decode PasCell
        |> requiredAt [ "derived", "f0" ] float
        |> requiredAt [ "derived", "IA" ] float
        |> requiredAt [ "derived", "Q" ] float
        |> requiredAt [ "derived", "max" ] (list float)
        |> requiredAt [ "derived", "ext" ] float
        |> required "lRMS" float
        |> requiredAt [ "PhotoDiode", "Y" ] (list int)
        |> requiredAt [ "MicFreq", "Y" ] (list float)
        |> requiredAt [ "MicTime", "Y" ] (list int)
        |> requiredAt [ "FittedData", "Y" ] (list float)
        |> required "IABack" float


asDataIn : Model -> PasData -> Model
asDataIn model data =
    { model | data = data }


decodePASData : Decoder PasData
decodePASData =
    map2 PasData
        (field "CellData" (array decodeCellData))
        (field "Drive" decodeDrive)


retrievePasData : String -> String -> Model -> Model
retrievePasData head data model =
    let
        new_model =
            Result.withDefault model.data (decodeString (field head decodePASData) data)
                |> asDataIn model
    in
    new_model


setVscaleSpk : String -> Speaker_ -> Speaker_
setVscaleSpk vscale spk =
    { spk | vscale = vscale }


setVoffsetSpk : String -> Speaker_ -> Speaker_
setVoffsetSpk voffset spk =
    { spk | voffset = voffset }


setFcenterSpk : String -> Speaker_ -> Speaker_
setFcenterSpk fcenter spk =
    { spk | center = fcenter }


setDfSpk : String -> Speaker_ -> Speaker_
setDfSpk df spk =
    { spk | df = df }


setPeriodSpk : String -> Speaker_ -> Speaker_
setPeriodSpk period spk =
    { spk | period = period }


setLengthSpk : String -> Speaker_ -> Speaker_
setLengthSpk length spk =
    { spk | length = length }


toggleSpeakerPosition : Speaker_ -> Speaker_
toggleSpeakerPosition spk =
    let
        drive =
            if spk.pos == Speaker then
                Laser
            else
                Speaker
    in
    { spk | pos = drive }


toggleSpeakerCycle : Speaker_ -> Speaker_
toggleSpeakerCycle spk =
    { spk | auto = not spk.auto }


decodeSpeaker : Decoder Speaker_
decodeSpeaker =
    map8 Speaker_
        (field "period" intString)
        (field "length" intString)
        (field "auto" bool)
        (field "vscale" floatString)
        (field "voffset" floatString)
        (field "center" intString)
        (field "df" intString)
        (field "pos" decodeDrive)


setHeaterSP : Float -> Heater -> Heater
setHeaterSP sp htr =
    { htr | sp = sp }


setHeaterPID : Array Float -> Heater -> Heater
setHeaterPID pid htr =
    { htr | pid = pid }


decodeHeater : Decoder Heater
decodeHeater =
    map2 Heater
        (field "pid" (array float))
        (field "sp" float)


setSpkVscale : String -> Speaker_ -> Speaker_
setSpkVscale vscale spk =
    { spk | vscale = vscale }


setPasHeater0 : Heater -> PasCvt -> PasCvt
setPasHeater0 htr cvt =
    { cvt | heater_0 = htr }


asHeater0In : PasCvt -> Heater -> PasCvt
asHeater0In =
    flip setPasHeater0


setPasHeater1 : Heater -> PasCvt -> PasCvt
setPasHeater1 htr cvt =
    { cvt | heater_1 = htr }


asHeater1In : PasCvt -> Heater -> PasCvt
asHeater1In =
    flip setPasHeater0


asCvtIn : Model -> PasCvt -> Model
asCvtIn model cvt =
    { model | cvt = cvt }


setSpk : Speaker_ -> PasCvt -> PasCvt
setSpk spk cvt =
    { cvt | spk = spk }


asSpeakerIn : PasCvt -> Speaker_ -> PasCvt
asSpeakerIn =
    flip setSpk


setFrequency0 : Int -> PasCvt -> PasCvt
setFrequency0 f cvt =
    { cvt | fmod_0 = f }


setFrequency1 : Int -> PasCvt -> PasCvt
setFrequency1 f cvt =
    { cvt | fmod_1 = f }


toggleLaserPower : Int -> PasCvt -> PasCvt
toggleLaserPower cell cvt =
    let
        new_cvt =
            case cell of
                0 ->
                    { cvt | enable_0 = not cvt.enable_0 }

                1 ->
                    { cvt | enable_1 = not cvt.enable_1 }

                _ ->
                    cvt
    in
    new_cvt


defaultHeater : Heater
defaultHeater =
    Heater (Array.fromList [ 1, 0, 0 ]) 18


defaultSpk : Speaker_
defaultSpk =
    Speaker_ "360" "30" True "1" "0.5" "1350" "100" Speaker


defaultCvt : PasCvt
defaultCvt =
    PasCvt defaultSpk [ 0, 0 ] [ 1, 1 ] 1350 1350 defaultHeater defaultHeater False False


decodePasCvt : PasCvt -> Decoder PasCvt
decodePasCvt cvt =
    decode PasCvt
        |> required "spk" decodeSpeaker
        |> required "b" (list float)
        |> required "m" (list float)
        |> required "fmod_0" int
        |> required "fmod_1" int
        |> required "heater0" decodeHeater
        |> required "heater1" decodeHeater
        |> requiredAt [ "ch0", "laser_enable" ] bool
        |> requiredAt [ "ch1", "laser_enable" ] bool


retrievePasCvt : String -> String -> PasCvt -> PasCvt
retrievePasCvt heading data cvt =
    if data == "{}" then
        cvt
    else
        Result.withDefault cvt (decodeString (field heading (decodePasCvt cvt)) data)


defaultPasData : PasData
defaultPasData =
    PasData defaultPasCellData Laser


defaultPasCellData : Array PasCell
defaultPasCellData =
    Array.fromList [ PasCell 0 0 0 [ 0, 0 ] 0 0 [ 0 ] [ 0 ] [ 0 ] [ 0 ] 0 ]


{-| Helper function for decoding the drive. This value is a boolean in the
json file but we want to make it explicit as to what the value actually is.
We will convert `True` to `Speaker` and `False` to `Laser`.
-}
decodeDrive : Decoder Drive
decodeDrive =
    let
        helper : Bool -> Decoder Drive
        helper d =
            if d then
                succeed Speaker
            else
                succeed Laser
    in
    bool |> andThen helper

-- The following functions are used to take CVT data that comes back as a number in the
-- server string but is stored as a string
intString : Decoder String
intString =
  map toString int

floatString : Decoder String
floatString =
  map toString float
