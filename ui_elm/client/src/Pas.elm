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
    , speaker_0 : Bool
    , speaker_1 : Bool
    }


type alias Speaker_ =
    { vscale : String
    , voffset : String
    , center : String
    , df : String
    }


type alias Heater =
    { pid : Array String
    , sp : String
    , enable_pid : Bool
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


toggleSpeaker0Position : PasCvt -> PasCvt
toggleSpeaker0Position cvt =
    { cvt | speaker_0 = not cvt.speaker_0 }


toggleSpeaker1Position : PasCvt -> PasCvt
toggleSpeaker1Position cvt =
    { cvt | speaker_1 = not cvt.speaker_1 }


decodeSpeaker : Decoder Speaker_
decodeSpeaker =
    map4 Speaker_
        (field "vscale" floatString)
        (field "voffset" floatString)
        (field "center" intString)
        (field "df" intString)


setHeaterSP : String -> Heater -> Heater
setHeaterSP sp htr =
    { htr | sp = sp }


setHeaterPID : Array String -> Heater -> Heater
setHeaterPID pid htr =
    { htr | pid = pid }


decodeHeater : Decoder Heater
decodeHeater =
    map3 Heater
        (field "pid" (array string))
        (field "sp" string)
        (field "enable" bool)


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
    flip setPasHeater1


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


asFreqIn : PasCell -> List Float -> PasCell
asFreqIn cell freqData =
    { cell | frequencyData = freqData }


setCellData : Model -> Int -> PasCell -> Model
setCellData model cint cell =
    let
        newCell =
            Array.set cint cell model.data.cell

        n_data =
            model.data

        nn_data =
            { n_data | cell = newCell }
    in
        { model | data = nn_data }


truncateFrequencyData : Int -> Int -> Int -> Model -> Model
truncateFrequencyData cell min max model =
    let
        defaultCell =
            PasCell 0 0 0 [ 0, 0 ] 0 0 [ 0 ] [ 0 ] [ 0 ] [ 0 ] 0

        cellData =
            Maybe.withDefault defaultCell (Array.get cell model.data.cell)

        newFreqData =
            Array.toList (Array.slice min max (Array.fromList cellData.frequencyData))

        new_cell =
            asFreqIn cellData newFreqData
    in
        setCellData model cell new_cell


defaultHeater : Heater
defaultHeater =
    Heater (Array.fromList [ "1", "0", "0" ]) "18" False


defaultSpk : Speaker_
defaultSpk =
    Speaker_ "1" "0.5" "1350" "100"


defaultCvt : PasCvt
defaultCvt =
    PasCvt defaultSpk [ 0, 0 ] [ 1, 1 ] 1350 1350 defaultHeater defaultHeater False False True True


decodePasCvt : PasCvt -> Decoder PasCvt
decodePasCvt cvt =
    decode PasCvt
        |> required "spk" decodeSpeaker
        |> required "b" (list float)
        |> required "m" (list float)
        |> requiredAt [ "ch0", "mod" ] int
        |> requiredAt [ "ch1", "mod" ] int
        |> required "heater0" decodeHeater
        |> required "heater1" decodeHeater
        |> requiredAt [ "ch0", "laser_enable" ] bool
        |> requiredAt [ "ch1", "laser_enable" ] bool
        |> requiredAt [ "ch0", "spk" ] bool
        |> requiredAt [ "ch1", "spk" ] bool


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
