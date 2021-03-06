module GcrdTypes exposing (..)

import Array as Array
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, optional, required, requiredAt)


-- Data returned by the Vaisala hygrometers


type alias VaisalaData =
    { id : String
    , temperature : Float
    , relative_humidity : Float
    , dewpoint : Float
    }



{- The following are some elm-plot helpers -}


type alias PlotLimits =
    { max : Float -> Float, min : Float -> Float }


type alias PlotDescription =
    { xLim : PlotLimits
    , yLim : PlotLimits
    , autoscale : Bool
    }


defaultVaisalaData : VaisalaData
defaultVaisalaData =
    { id = "default", temperature = 0, relative_humidity = 0, dewpoint = 0 }



-- PPT Data


type alias PptData =
    { id : String
    , pressure : Float
    , temperature : Float
    }


defaultPptData : PptData
defaultPptData =
    { id = "default", pressure = 0, temperature = 0 }



-- This is the data returned by the server


type alias Data =
    { time : String
    , msg : List String
    , interlock : Bool
    , temperatures : Temperatures
    , pas0_heater : HeaterData
    , pas1_heater : HeaterData
    , crd_heater : HeaterData
    }


setMessages : List String -> Data -> Data
setMessages msgs data =
    { data | msg = msgs }


decodeData : Decoder Data
decodeData =
    decode Data
        |> requiredAt [ "general", "Time" ] string
        |> requiredAt [ "general", "msgs" ] (list string)
        |> requiredAt [ "general", "interlock" ] bool
        |> requiredAt [ "general", "temperatures" ] decodeTemperatures
        |> required "pas_1_heater" decodeHeaterData
        |> required "pas_2_heater" decodeHeaterData
        |> required "crd_heater" decodeHeaterData


defaultData : Data
defaultData =
    Data "0" [] False (Temperatures 0 0 0 0 0 0 0 0 0 0) defaultHeaterData defaultHeaterData defaultHeaterData



--type FilterPos = Cell_1 | Cell_2
-- TODO: Clean this up so that we don't need this.


type alias Filter =
    { pos : Int
    }


toggleFilterPosition : Filter -> Filter
toggleFilterPosition f =
    let
        val =
            if f.pos == 0 then
                1
            else
                0
    in
        { f | pos = val }


{-| -}
setFilter : Filter -> Cvt -> Cvt
setFilter filt cvt =
    { cvt | filter = filt }


asFilterIn : Cvt -> Filter -> Cvt
asFilterIn =
    flip setFilter


decodeCal : Decoder Calibration
decodeCal =
    map3 Calibration
        (field "o2_add" bool)
        (field "o3_add" bool)
        (field "uv_lamp" bool)


type alias Temperatures =
    { pasCell1 : Float
    , pasCell2 : Float
    , pasLaserHead1 : Float
    , pasLaserHead2 : Float
    , boxExit : Float
    , crdHeater : Float
    , boxInlet : Float
    , crdLaserHead : Float
    , cjc : Float
    , autozero : Float
    }


temperatureToArray : Temperatures -> Array.Array Float
temperatureToArray t =
    Array.fromList
        [ t.pasCell1
        , t.pasCell2
        , t.pasLaserHead1
        , t.pasLaserHead2
        , t.boxExit
        , t.crdHeater
        , t.boxInlet
        , t.crdLaserHead
        , t.cjc
        , t.autozero
        ]


getTemperature : Int -> Temperatures -> Float
getTemperature index t =
    Maybe.withDefault 0 (Array.get index (temperatureToArray t))


decodeTemperatures : Decoder Temperatures
decodeTemperatures =
    decode Temperatures
        |> required "pas_cell_1" float
        |> required "pas_cell_2" float
        |> required "pas_las_head_1" float
        |> required "pas_las_head_2" float
        |> required "box_exit" float
        |> required "crd_heater" float
        |> required "box_inlet" float
        |> required "crd_laser_head" float
        |> required "cjc" float
        |> required "autozero" float


type alias Calibration =
    { o2_add : Bool
    , o3_add : Bool
    , uv_lamp : Bool
    }


toggleO3AddPosition : Calibration -> Calibration
toggleO3AddPosition cal =
    { cal | o3_add = not cal.o3_add }


toggleO2AddPosition : Calibration -> Calibration
toggleO2AddPosition cal =
    { cal | o2_add = not cal.o2_add }


toggleUVLampPosition : Calibration -> Calibration
toggleUVLampPosition cal =
    { cal | uv_lamp = not cal.uv_lamp }


setCalibrationCvt : Calibration -> Cvt -> Cvt
setCalibrationCvt cal cvt =
    { cvt | cal_state = cal }


asCalibrationIn : Cvt -> Calibration -> Cvt
asCalibrationIn =
    flip setCalibrationCvt



-- This is the data that is maintained by the server


type alias Cvt =
    { pump : Bool
    , cal_state : Calibration
    , devIds : List String
    , filter : Filter
    , fan : Bool
    , fan_voltage : Float
    , sequence_state : String
    , save : Bool
    }


setFanEnable : Bool -> Cvt -> Cvt
setFanEnable val cvt =
    { cvt | fan = val }


setFanVoltage : Float -> Cvt -> Cvt
setFanVoltage volts cvt =
    { cvt | fan_voltage = volts }


switchPump : Cvt -> Cvt
switchPump cvt =
    { cvt | pump = not cvt.pump }


setCalibration : Calibration -> Cvt -> Cvt
setCalibration cal cvt =
    { cvt | cal_state = cal }


defaultCvtData : Cvt
defaultCvtData =
    { pump = False
    , cal_state = Calibration False False False
    , devIds = []
    , filter = Filter 0
    , fan = False
    , fan_voltage = 0
    , sequence_state = "Pause"
    , save = False
    }


decodeFilter : Decoder Filter
decodeFilter =
    map Filter
        (field "cell" int)


decodeCvt : Cvt -> Decoder Cvt
decodeCvt cvt =
    decode Cvt
        |> requiredAt [ "general", "vacuum_pump" ] bool
        |> required "calibration" decodeCal
        |> requiredAt [ "general", "dev_ids" ] (list string)
        |> required "filter" decodeFilter
        |> requiredAt [ "general", "fan" ] bool
        |> requiredAt [ "general", "fan_speed" ] float
        |> requiredAt [ "general", "seq_state" ] string
        |> requiredAt [ "general", "save" ] bool


type alias HeaterData =
    { enable : Bool, sp : Float, dc : Int }


defaultHeaterData : HeaterData
defaultHeaterData =
    { enable = False, sp = 0, dc = 0 }


decodeHeaterData : Decoder HeaterData
decodeHeaterData =
    decode HeaterData
        |> required "enable" bool
        |> required "sp" float
        |> required "dc" int



--|> List.map (\id -> requiredAt [ "device", id ] decodeDevice) cvt.devIds
