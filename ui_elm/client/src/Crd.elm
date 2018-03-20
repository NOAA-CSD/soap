module Crd exposing (..)

import Array exposing (Array)
import Html exposing (Html)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, optional, required, requiredAt)
import Plot
import Svg.Attributes as Attributes


--TODO: Fix the CRDS data type so that it can take cell data *and* running data


type alias Model =
    { cvt : CrdCvt
    , data : Array CrdsCell
    }


type alias Data =
    { cellData : Array CrdsCell, runningData : Array (Array Float) }


init : Model
init =
    { cvt = defaultCvt, data = Array.fromList [ CrdsCell 0 0 0 0 0 0 0 0 0 [ [ 0 ] ] ] }


type Msg
    = UpdateFrequency String
    | UpdateDutyCycle String
    | TogglePower


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateFrequency freq ->
            let
                f =
                    Result.withDefault 1000 (String.toInt freq)

                new_model =
                    model.cvt
                        |> setRate f
                        |> asCvtIn model
            in
            new_model

        UpdateDutyCycle dc ->
            let
                dc_ =
                    Debug.log "dc" (Result.withDefault 0 (String.toInt dc))

                new_model =
                    model.cvt
                        |> setDc dc_
                        |> asCvtIn model
            in
            new_model

        TogglePower ->
            model.cvt
                |> togglePower
                |> asCvtIn model



-- TODO: Add the ringdown data!!  List or Array??


type alias CrdsCell =
    { tau : Float
    , tau0corr : Float
    , extinction : Float
    , extinctionCorrected : Float
    , stdDevTau : Float
    , tauError : Float
    , tauCorrected : Float
    , tau0 : Float
    , max : Float
    , ringdowns : List (List Int)
    }


type alias CrdCvt =
    { labels : List String
    , rate : Int
    , dc : Int
    , heater : Heater
    , power : Bool
    }


type alias Heater =
    { pid : Array String
    , sp : String
    , enable_pid : Bool
    }


defaultCvt : CrdCvt
defaultCvt =
    CrdCvt
        [ "cell_0", "cell_1" ]
        1000
        400
        (Heater (Array.fromList [ "1", "0", "0" ]) "18" False)
        False


setRate : Int -> CrdCvt -> CrdCvt
setRate f cvt =
    { cvt | rate = f }


setDc : Int -> CrdCvt -> CrdCvt
setDc dc cvt =
    { cvt | dc = dc }


togglePower : CrdCvt -> CrdCvt
togglePower cvt =
    { cvt | power = not cvt.power }


setHeaterSP : String -> Heater -> Heater
setHeaterSP sp htr =
    { htr | sp = sp }


setHeaterPID : Array String -> Heater -> Heater
setHeaterPID pid htr =
    { htr | pid = pid }


asHeaterIn : CrdCvt -> Heater -> CrdCvt
asHeaterIn cvt htr =
    { cvt | heater = htr }


decodeHeater : Decoder Heater
decodeHeater =
    map3 Heater
        (field "pid" (array string))
        (field "sp" string)
        (field "enable" bool)


asCvtIn : Model -> CrdCvt -> Model
asCvtIn model cvt =
    { model | cvt = cvt }


asDataIn : Model -> Array CrdsCell -> Model
asDataIn model data_array =
    { model | data = data_array }



-- BEGIN DATA DECODING ROUTINES!!


{-| Decodes the data from a json string for the CVT
-}
decodeCvt : CrdCvt -> Decoder CrdCvt
decodeCvt cvt =
    decode CrdCvt
        |> required "cell_ids" (list string)
        |> required "f" int
        |> required "samp_per_cycle" int
        |> required "heater" decodeHeater
        |> optional "laser_enable" bool cvt.power


decodeExtData : Decoder CrdsCell
decodeExtData =
    decode CrdsCell
        |> requiredAt [ "extParam", "Tau" ] float
        |> requiredAt [ "extParam", "Tau0cor" ] float
        |> requiredAt [ "extParam", "ext" ] float
        |> requiredAt [ "extParam", "extCorr" ] float
        |> requiredAt [ "extParam", "stdevTau" ] float
        |> requiredAt [ "extParam", "eTau" ] float
        |> requiredAt [ "extParam", "taucorr" ] float
        |> requiredAt [ "extParam", "Tau0" ] float
        |> requiredAt [ "extParam", "max" ] float
        |> required "Ringdowns" (list (list int))


retrieveData : String -> String -> Array CrdsCell -> Array CrdsCell
retrieveData head data cell_data =
    Result.withDefault cell_data (decodeString (field head (array decodeExtData)) data)


viewRingdown : Model -> Html Msg
viewRingdown model =
    let
        blueCircle ( x, y ) =
            Plot.dot (Plot.viewCircle 5 "#cfd8ea") x (y * 1.2)

        line =
            { axis = Plot.normalAxis
            , interpolation = Plot.Linear Nothing [ Attributes.stroke "#cfd8ea" ]
            , toDataPoints = List.map blueCircle
            }

        cell_0 =
            Maybe.withDefault (CrdsCell 0 0 0 0 0 0 0 0 0 [ [ 0 ] ]) (Array.get 0 model.data)

        raw_data =
            Maybe.withDefault [] (List.head cell_0.ringdowns)

        data =
            List.indexedMap (,) raw_data
    in
    Plot.viewSeries
        [ Plot.line <| List.map (\( x, y ) -> Plot.circle (toFloat x) (toFloat y)) ]
        data


{-| This command actually retrieves the CVT data from the json string returned by the server.

  - `heading`: section heading in the json string provided
  - `data`: json data string returned from the server
  - `cvt`: current cvt; required if the `data` string is bad

-}
retrieveCrdCvt : String -> String -> CrdCvt -> CrdCvt
retrieveCrdCvt heading data cvt =
    if data == "{}" then
        cvt
    else
        Result.withDefault cvt (decodeString (field heading (decodeCvt cvt)) data)
