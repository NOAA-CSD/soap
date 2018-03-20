module Main exposing (..)

--import Material.Typography
--import Material.Icon as Icon
--import Material.Card as Card
--import Svg.Events exposing (..)
{- Allow the models to handle changes to state while the main loop handles commands over
   the network!!
-}
-- TODO: DateTime stuff wrt conversion of LV time should be moved to a general file...
--import ContextMenu as CM

import Array
import Crd
import Debug exposing (log)
import Devices.Alicat as Alicat
import Devices.Device as Device
import Devices.Ppt as Ppt
import Devices.Vaisala as Vaisala
import Dict
import GcrdTypes exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (property)
import Http
import Json.Decode as Decoder
import Json.Encode exposing (string)
import List.Extra as ListExtra
import Material
import Material.Badge
import Material.Button exposing (..)
import Material.Color exposing (..)
import Material.Footer as Footer
import Material.Grid as Grid
import Material.Icon as Icon
import Material.Layout as Layout
import Material.List as MList
import Material.Options exposing (Style, css, onBlur, onInput)
import Material.Slider as Slider
import Material.Table as Table
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Material.Typography as Typo
import Network
import Pas
import Plot exposing (defaultSeriesPlotCustomizations)
import Round
import Svg exposing (Svg)
import Svg.Attributes
import Task
import Time as Time
import Time.DateTime as DateTime exposing (zero)


{-| The name of the web service.
-}
webService : String
webService =
    "soap"



{- getCurrentCvt: Model -> Cmd Msg
   getCurrentCvt model =
       Http.send GetCVT <| (Http.get ((buildAddress <| model) ++ "cvt")
-}
-- TODO: Move MDL formatting to different file along with app.css for consistent formatting


type alias RangeData =
    { xmin : Float, xmax : Float, ymin : Float, ymax : Float }


type HeaterID
    = CrdHeater
    | Pas0Heater
    | Pas1Heater


type Context
    = RunningData


type alias Model =
    { pas : Pas.Model
    , crd : Crd.Model
    , genData : Data
    , cvt : Cvt
    , save : Bool
    , selectedTab : Int
    , dt : DateTime.DateTime
    , mdl : Material.Model
    , network : Network.Model
    , runningData : List (List Float)
    , crdRunningData : List (List Float)
    , currentMsgList : List String
    , alicats : Alicat.Model
    , vaisalas : Vaisala.Model
    , ppts : Ppt.Model
    , msgs : List Int
    , pasPlotData : List Bool
    , pasRange : RangeData
    , crdPlotData : List Bool
    , crdRange : RangeData
    }



{- All of the populate functions are used populate the CVT entries on a CVT update.
   These functions essentially require some functionality for extracting the data and
   populating the fields and then returning a new model with the correct CVT values.
-}


populateAlicatDevices : (Alicat.Model -> Alicat.Model) -> Model -> Model
populateAlicatDevices func model =
    { model | alicats = func model.alicats }


populateVaisalaDevices : (Vaisala.Model -> Vaisala.Model) -> Model -> Model
populateVaisalaDevices func model =
    { model | vaisalas = func model.vaisalas }


populatePptDevices : (Ppt.Model -> Ppt.Model) -> Model -> Model
populatePptDevices func model =
    { model | ppts = func model.ppts }


populateCrdCvt : (Crd.CrdCvt -> Crd.CrdCvt) -> Model -> Model
populateCrdCvt func model =
    { model | crd = Crd.asCvtIn model.crd (func model.crd.cvt) }


populatePasCvt : (Pas.PasCvt -> Pas.PasCvt) -> Model -> Model
populatePasCvt func model =
    { model | pas = Pas.asCvtIn model.pas (func model.pas.cvt) }


populateGeneralCvt : String -> Model -> Model
populateGeneralCvt data model =
    Result.withDefault defaultCvtData (Decoder.decodeString (decodeCvt model.cvt) data)
        |> asCvtIn model


populateGeneralData : String -> Model -> Model
populateGeneralData data model =
    Result.withDefault model.genData (Decoder.decodeString decodeData data)
        |> asDataIn model


populateCrdData : (Array.Array Crd.CrdsCell -> Array.Array Crd.CrdsCell) -> Model -> Model
populateCrdData func model =
    { model | crd = Crd.asDataIn model.crd (func model.crd.data) }


populatePasData : (Pas.Model -> Pas.Model) -> Model -> Model
populatePasData func model =
    { model | pas = func model.pas }


performPasTruncation : Model -> Model
performPasTruncation model =
    model.pas
        |> Pas.truncateFrequencyData 0 1200 1500
        |> Pas.truncateFrequencyData 1 1200 1500
        |> asPasIn model


populateAlicatData : (Alicat.Model -> Alicat.Model) -> Model -> Model
populateAlicatData func model =
    { model | alicats = func model.alicats }


populatePptData : (Ppt.Model -> Ppt.Model) -> Model -> Model
populatePptData func model =
    { model | ppts = func model.ppts }


populateVaisalaData : (Vaisala.Model -> Vaisala.Model) -> Model -> Model
populateVaisalaData func model =
    { model | vaisalas = func model.vaisalas }


populateMsgData : Model -> Model
populateMsgData model =
    { model
        | currentMsgList =
            List.append model.currentMsgList model.genData.msg
    }


getCrdCvt : Model -> Crd.CrdCvt
getCrdCvt model =
    model.crd.cvt


getAlicatModel : Model -> Alicat.Model
getAlicatModel model =
    model.alicats


getVaisalaModel : Model -> Vaisala.Model
getVaisalaModel model =
    model.vaisalas


getPptModel : Model -> Ppt.Model
getPptModel model =
    model.ppts


getPasCvt : Model -> Pas.PasCvt
getPasCvt model =
    model.pas.cvt


asRunningDataIn : Model -> List (List Float) -> Model
asRunningDataIn model ldata =
    { model | runningData = ldata }


asCrdRunningDataIn : Model -> List (List Float) -> Model
asCrdRunningDataIn model ldata =
    { model | crdRunningData = ldata }


asDataIn : Model -> Data -> Model
asDataIn model data =
    { model | genData = data }


asNetworkIn : Model -> Network.Model -> Model
asNetworkIn model network =
    { model | network = network }


asCrdIn : Model -> Crd.Model -> Model
asCrdIn model crd =
    { model | crd = crd }


asAlicatIn : Model -> Alicat.Model -> Model
asAlicatIn model alicat =
    { model | alicats = alicat }


asPptIn : Model -> Ppt.Model -> Model
asPptIn model ppt =
    { model | ppts = ppt }


asVaisalaIn : Model -> Vaisala.Model -> Model
asVaisalaIn model vaisala =
    { model | vaisalas = vaisala }


asPasIn : Model -> Pas.Model -> Model
asPasIn model pas =
    { model | pas = pas }


setCvt : Cvt -> Model -> Model
setCvt cvt mdl =
    { mdl | cvt = cvt }


asCvtIn : Model -> Cvt -> Model
asCvtIn =
    flip setCvt



-- TODO: FIX THIS SITUATION WITH THE CVT IN TWO PLACES!!!


defaultModelData : Model
defaultModelData =
    { pas = Pas.init
    , crd = Crd.init
    , genData = defaultData
    , cvt = defaultCvtData
    , save = True
    , selectedTab = 0
    , dt = DateTime.fromTuple ( 0, 0, 0, 0, 0, 0, 0 )
    , mdl = Material.model
    , network = { ip = "192.168.172.123", port_ = "8001", service = "soap" }
    , runningData = [ [] ]
    , crdRunningData = [ [] ]
    , currentMsgList = []
    , alicats = Alicat.init
    , vaisalas = Vaisala.init
    , ppts = Ppt.init
    , msgs = [ 0, 0, 0 ]
    , pasPlotData = [ True, True, True ]
    , pasRange = RangeData 0 1000 0 1500
    , crdPlotData = [ True, True, True ]
    , crdRange = RangeData 0 1000 0 1500
    }


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }



-- TODO: Init does nothing here...is that ok?


init : ( Model, Cmd Msg )
init =
    let
        model =
            defaultModelData
    in
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every Time.second CheckCvtData
        , Network.initPort InitializeNetwork
        , Time.every Time.second CheckData
        ]



{- When updating controls, there are usually two messages that will be fired.
   The first message will be a message to update the local CVT.  The second will
   be a message to *send* the current values to the controller.  The first
   message will look something like *Updatexxx* while the second will be
   something that looks like *Sendxxx*.
-}


type Msg
    = Mdl (Material.Msg Msg)
    | SelectTab Int
    | CheckData Time.Time
    | Network Network.Msg
    | UpdateTime Time.Time
    | ClearMessages
    | SyncTime
    | SaveData
    | StopServer
      -- Power and valves
    | ToggleO3
    | ToggleO2
    | ToggleFilter
    | TogglePump
    | ToggleUVLamp
    | ToggleUSB
    | ToggleFan
    | UpdateFanVoltage Float
      -- Regular messages - acquired periodically
    | GetCVT (Result Http.Error String)
    | GetData (Result Http.Error String)
    | InitializeNetwork ( String, String )
    | HandleGeneric (Result Http.Error String)
    | CheckCvtData Time.Time
    | ForceCvtCheck
      -- Begin heater interactions
    | UpdateHeaterSP HeaterID String
    | SendHeaterSP HeaterID
    | UpdateHeaterCtl HeaterID Int String
    | SendHeaterCtl HeaterID
    | ToggleHeaterPid HeaterID
      -- CRD
    | ToggleCrdPower
    | SendCrdFrequency
    | SendCrdSampleRate
    | Crd Crd.Msg
    | UpdateCrdRange String String
    | UpdateCrdScaling
    | ToggleCrdPlot Int
      -- PAS
    | TogglePasLaserPower Int
    | ToggleSpeaker Int
    | SendSpkVoltage
    | UpdateChirp
    | UpdateMod0 String
    | UpdateMod1 String
    | SendModulation String
    | Pas Pas.Msg
    | TogglePasPlot Int
    | UpdatePasRange String String
    | UpdatePasScaling
      -- Devices
    | UpdateDevSP String String
    | SendDevSP String
      -- Sequence
    | SequenceState
    | ResetSequence


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl message_ ->
            Material.update Mdl message_ model

        SelectTab tab ->
            let
                new_model =
                    { model | selectedTab = tab }
            in
            ( new_model, updateWaveforms new_model )

        CheckCvtData tick ->
            ( model, getCvtData model "0" )

        -- Use this state to ensure that the current CVT is returned...
        ForceCvtCheck ->
            ( model, getCvtData model "1" )

        CheckData tick ->
            ( model, getData model )

        Network subMsg ->
            let
                new_model =
                    Network.update subMsg model.network
                        |> asNetworkIn model

                ip =
                    new_model.network.ip

                port_ =
                    new_model.network.port_
            in
            -- When the network model is updated, remember to update the config on the
            -- javascript side...
            ( new_model, Network.updateIpConfig [ ip, port_ ] )

        Pas subMsg ->
            let
                new_model =
                    Pas.update subMsg model.pas
                        |> asPasIn model
            in
            ( new_model, Cmd.none )

        Crd subMsg ->
            let
                new_model =
                    Crd.update subMsg model.crd
                        |> asCrdIn model
            in
            ( new_model, Cmd.none )

        SaveData ->
            let
                new_model =
                    { model | save = not model.save }
            in
            ( new_model, toggleSave new_model )

        StopServer ->
            ( model, shutdownSystem model )

        ToggleO3 ->
            let
                new_model =
                    model.cvt.cal_state
                        |> toggleO3AddPosition
                        |> asCalibrationIn model.cvt
                        |> asCvtIn model

                val =
                    if new_model.cvt.cal_state.o3_add then
                        "1"
                    else
                        "0"
            in
            ( new_model, toggleO3 new_model val )

        ToggleO2 ->
            let
                new_model =
                    model.cvt.cal_state
                        |> toggleO2AddPosition
                        |> asCalibrationIn model.cvt
                        |> asCvtIn model

                val =
                    if new_model.cvt.cal_state.o2_add then
                        "1"
                    else
                        "0"
            in
            ( new_model, toggleO2 new_model val )

        ToggleFilter ->
            let
                new_model =
                    model.cvt.filter
                        |> toggleFilterPosition
                        |> asFilterIn model.cvt
                        |> asCvtIn model
            in
            ( new_model, toggleFilter new_model )

        TogglePump ->
            let
                new_model =
                    model.cvt
                        |> switchPump
                        |> asCvtIn model
            in
            ( new_model, togglePump new_model )

        ToggleUVLamp ->
            let
                new_model =
                    model.cvt.cal_state
                        |> toggleUVLampPosition
                        |> asCalibrationIn model.cvt
                        |> asCvtIn model

                val =
                    if model.cvt.cal_state.uv_lamp then
                        "0"
                    else
                        "1"
            in
            ( new_model, toggleUV new_model val )

        ToggleUSB ->
            ( model, Cmd.none )

        GetCVT (Ok data) ->
            let
                {- If the value {} is returned, then the server does not have a
                   current update.  Don't bother with this.
                -}
                new_model =
                    if data == "{}" then
                        model
                    else
                        let
                            dev_cvt =
                                Result.withDefault
                                    Device.defaultDeviceDict
                                    (Decoder.decodeString Device.decodeDeviceCvt data)
                        in
                        model
                            |> populateAlicatDevices (Alicat.insertAlicatDev dev_cvt)
                            |> populateVaisalaDevices (Vaisala.insertVaisalaDev dev_cvt)
                            |> populatePptDevices (Ppt.insertPptDev dev_cvt)
                            |> populateCrdCvt (Crd.retrieveCrdCvt "crd" data)
                            |> populatePasCvt (Pas.retrievePasCvt "pas" data)
                            |> populateGeneralCvt data
            in
            ( new_model, Cmd.none )

        GetCVT (Err err) ->
            let
                data_ =
                    Debug.log "CVT-Error" "There was an error contacting " ++ Network.buildAddress model.network
            in
            ( model, Cmd.none )

        GetData (Ok data) ->
            let
                new_model =
                    model
                        |> populateAlicatData (Alicat.getAlicatData data)
                        |> populatePptData (Ppt.getPptData data)
                        |> populateVaisalaData (Vaisala.getVaisalaData data)
                        |> populateCrdData (Crd.retrieveData "CellData" data)
                        |> populatePasData (Pas.retrieveData "PAS" data)
                        |> populateGeneralData data
                        |> populateMsgData
                        |> performPasTruncation

                -- The following is for testing purposes for handling running data.  There is a better, more generic
                -- way of doing this that needs to be implemented.
                -- TODO: implement generic functionality for doing running data.
                rPasData =
                    Maybe.withDefault
                        (Pas.PasCell 0 0 0 [ 0, 0 ] 0 0 [ 0 ] [ 0 ] [ 0 ] [ 0 ] 0)
                        (Array.get 1 model.pas.data.cell)

                rCrdData =
                    Maybe.withDefault
                        (Crd.CrdsCell 0 0 0 0 0 0 0 0 0 [ [ 0 ] ])
                        (Array.get 1 model.crd.data)

                cListData =
                    [ rCrdData.tau
                    , rCrdData.tau0
                    , rCrdData.max
                    ]

                listData =
                    [ rPasData.resonant_frequency
                    , rPasData.integrated_area
                    , rPasData.laserRMS
                    , rPasData.absorption
                    ]

                n_model =
                    addDataToList 10 listData new_model.runningData
                        |> asRunningDataIn new_model

                nn_model =
                    addDataToList 100 cListData n_model.crdRunningData
                        |> asCrdRunningDataIn n_model
            in
            ( nn_model, Cmd.none )

        GetData (Err _) ->
            ( model, Cmd.none )

        HandleGeneric (Ok data) ->
            ( model, Cmd.none )

        HandleGeneric (Err _) ->
            ( model, Cmd.none )

        InitializeNetwork config ->
            let
                network =
                    model.network

                ip =
                    Tuple.first config

                p =
                    Tuple.second config

                network_ =
                    { network | ip = ip }

                new_model =
                    { model | network = { network_ | port_ = p } }
            in
            ( new_model, getCvtData new_model "1" )

        ToggleSpeaker cell ->
            let
                new_model =
                    case cell of
                        0 ->
                            model.pas.cvt
                                |> Pas.toggleSpeaker0Position
                                |> Pas.asCvtIn model.pas
                                |> asPasIn model

                        1 ->
                            model.pas.cvt
                                |> Pas.toggleSpeaker1Position
                                |> Pas.asCvtIn model.pas
                                |> asPasIn model

                        default ->
                            model

                val =
                    case cell of
                        0 ->
                            if new_model.pas.cvt.speaker_0 then
                                "1"
                            else
                                "0"

                        1 ->
                            if new_model.pas.cvt.speaker_1 then
                                "1"
                            else
                                "0"

                        default ->
                            "1"

                ncell =
                    if cell == 0 then
                        "0"
                    else
                        "1"
            in
            ( new_model, toggleSpk new_model val ncell )

        UpdateMod0 f ->
            let
                f_ =
                    Result.withDefault 1350 (String.toInt f)

                new_model =
                    model.pas.cvt
                        |> Pas.setFrequency0 f_
                        |> Pas.asCvtIn model.pas
                        |> asPasIn model
            in
            ( new_model, Cmd.none )

        UpdateMod1 f ->
            let
                f_ =
                    Result.withDefault 1350 (String.toInt f)

                new_model =
                    model.pas.cvt
                        |> Pas.setFrequency1 f_
                        |> Pas.asCvtIn model.pas
                        |> asPasIn model
            in
            ( new_model, Cmd.none )

        SendModulation cell ->
            let
                index =
                    Result.withDefault 0 (String.toInt cell)

                freq =
                    if cell == "1" then
                        model.pas.cvt.fmod_1
                    else
                        model.pas.cvt.fmod_0

                f =
                    toString freq
            in
            ( model, setCellFrequency model cell f )

        SendCrdFrequency ->
            ( model, setCrdFrequency model )

        SendCrdSampleRate ->
            ( model, setCrdRate model )

        ToggleCrdPower ->
            let
                new_model =
                    model.crd.cvt
                        |> Crd.togglePower
                        |> Crd.asCvtIn model.crd
                        |> asCrdIn model

                val =
                    if new_model.crd.cvt.power then
                        "1"
                    else
                        "0"
            in
            ( new_model, toggleCrdPower model val )

        ToggleHeaterPid id ->
            ( model, toggleHeaterPid id model )

        UpdateHeaterSP id sp ->
            let
                nid =
                    Debug.log "heater_id" id

                nsp =
                    Debug.log "heater_sp" sp
            in
            case id of
                CrdHeater ->
                    let
                        new_model =
                            model.crd.cvt.heater
                                |> Crd.setHeaterSP sp
                                |> Crd.asHeaterIn model.crd.cvt
                                |> Crd.asCvtIn model.crd
                                |> asCrdIn model
                    in
                    ( new_model, Cmd.none )

                Pas0Heater ->
                    let
                        new_model =
                            model.pas.cvt.heater_0
                                |> Pas.setHeaterSP sp
                                |> Pas.asHeater0In model.pas.cvt
                                |> Pas.asCvtIn model.pas
                                |> asPasIn model
                    in
                    ( new_model, Cmd.none )

                Pas1Heater ->
                    let
                        new_model =
                            model.pas.cvt.heater_1
                                |> Pas.setHeaterSP sp
                                |> Pas.asHeater1In model.pas.cvt
                                |> Pas.asCvtIn model.pas
                                |> asPasIn model
                    in
                    ( new_model, Cmd.none )

        UpdateHeaterCtl id ctl val ->
            case id of
                CrdHeater ->
                    let
                        new_model =
                            model.crd.cvt.heater
                                |> Crd.setHeaterPID
                                    (Array.set
                                        ctl
                                        val
                                        model.crd.cvt.heater.pid
                                    )
                                |> Crd.asHeaterIn model.crd.cvt
                                |> Crd.asCvtIn model.crd
                                |> asCrdIn model
                    in
                    ( new_model, Cmd.none )

                Pas0Heater ->
                    let
                        new_model =
                            model.pas.cvt.heater_0
                                |> Pas.setHeaterPID
                                    (Array.set
                                        ctl
                                        val
                                        model.pas.cvt.heater_0.pid
                                    )
                                |> Pas.asHeater0In model.pas.cvt
                                |> Pas.asCvtIn model.pas
                                |> asPasIn model
                    in
                    ( new_model, Cmd.none )

                Pas1Heater ->
                    let
                        new_model =
                            model.pas.cvt.heater_1
                                |> Pas.setHeaterPID
                                    (Array.set
                                        ctl
                                        val
                                        model.pas.cvt.heater_1.pid
                                    )
                                |> Pas.asHeater1In model.pas.cvt
                                |> Pas.asCvtIn model.pas
                                |> asPasIn model
                    in
                    ( new_model, Cmd.none )

        SendHeaterSP id ->
            ( model, sendHeaterSP id model )

        SendHeaterCtl id ->
            ( model, sendHeaterCtl id model )

        TogglePasLaserPower cell ->
            let
                new_model =
                    Pas.toggleLaserPower cell model.pas.cvt
                        |> Pas.asCvtIn model.pas
                        |> asPasIn model

                c =
                    toString cell

                cmd =
                    case cell of
                        0 ->
                            let
                                val =
                                    if new_model.pas.cvt.enable_0 then
                                        "1"
                                    else
                                        "0"
                            in
                            togglePasLaserPower new_model val (toString cell)

                        1 ->
                            let
                                val =
                                    if new_model.pas.cvt.enable_1 then
                                        "1"
                                    else
                                        "0"
                            in
                            togglePasLaserPower new_model val (toString cell)

                        _ ->
                            Cmd.none
            in
            ( new_model, cmd )

        ClearMessages ->
            ( { model | currentMsgList = [] }, Cmd.none )

        SendSpkVoltage ->
            ( model, sendSpkVoltage model )

        UpdateChirp ->
            ( model, sendChirp model )

        ToggleFan ->
            let
                new_model =
                    model.cvt
                        |> setFanEnable (not model.cvt.fan)
                        |> asCvtIn model

                val =
                    if new_model.cvt.fan then
                        "1"
                    else
                        "0"
            in
            ( new_model, toggleFan new_model val )

        UpdateFanVoltage sp ->
            let
                new_model =
                    setFanVoltage sp model.cvt
                        |> asCvtIn model
            in
            ( new_model, sendFanVoltage new_model )

        UpdateDevSP idx sp ->
            let
                dev =
                    Maybe.withDefault Device.defaultDevice
                        (Dict.get idx model.alicats.cvt)

                new_dev =
                    Device.setSpIn sp dev

                new_model =
                    Dict.insert idx new_dev model.alicats.cvt
                        |> Alicat.asCvtIn model.alicats
                        |> asAlicatIn model
            in
            ( new_model, Cmd.none )

        -- TODO: Implement device setpoint updating for the server
        SendDevSP idx ->
            let
                dev =
                    Maybe.withDefault Device.defaultDevice
                        (Dict.get idx model.alicats.cvt)
            in
            ( model, sendDevSp idx dev model )

        -- TODO: Implement time updating so that user can apply correct time to server.
        UpdateTime t ->
            ( model, sendNewTime t model )

        SyncTime ->
            -- This is called when the sync button is pushed; this gets the current time and
            -- fires the message above - UpdateTime - which then executes the function sendNewTime
            ( model, getCurrentTime )

        TogglePasPlot input ->
            let
                d =
                    Maybe.withDefault False (ListExtra.getAt input model.pasPlotData)

                newModel =
                    { model | pasPlotData = ListExtra.setAt input (not d) model.pasPlotData }
            in
            ( newModel, Cmd.none )

        -- TODO: Make this more generic for range entry
        UpdatePasRange entry val ->
            let
                v =
                    Result.withDefault 0 (String.toFloat val)

                range =
                    model.pasRange

                newRange =
                    case entry of
                        "xmin" ->
                            { range | xmin = v }

                        "xmax" ->
                            { range | xmax = v }

                        "ymin" ->
                            { range | ymin = v }

                        "ymax" ->
                            { range | ymax = v }

                        default ->
                            range
            in
            ( { model | pasRange = newRange }, Cmd.none )

        UpdatePasScaling ->
            let
                d =
                    if Maybe.withDefault False (ListExtra.getAt 2 model.pasPlotData) then
                        MicFreq
                    else
                        MicTime

                max_ =
                    if d == MicFreq then
                        1200
                    else
                        0

                maxdata =
                    List.foldl max3 ( 0, 0, 0 ) (getPasTimeData model max_ d)

                mindata =
                    List.foldl min3 maxdata (getPasTimeData model max_ d)

                range =
                    { xmin = firstElement mindata
                    , xmax = firstElement maxdata
                    , ymax = toFloat (ceiling (max (secondElement maxdata) (thirdElement maxdata)))
                    , ymin = toFloat (floor (min (secondElement mindata) (thirdElement mindata)))
                    }
            in
            ( { model | pasRange = range }, Cmd.none )

        UpdateCrdRange entry val ->
            let
                v =
                    Result.withDefault 0 (String.toFloat val)

                range =
                    model.crdRange

                newRange =
                    case entry of
                        "xmin" ->
                            { range | xmin = v }

                        "xmax" ->
                            { range | xmax = v }

                        "ymin" ->
                            { range | ymin = v }

                        "ymax" ->
                            { range | ymax = v }

                        default ->
                            range
            in
            ( { model | crdRange = newRange }, Cmd.none )

        UpdateCrdScaling ->
            let
                maxdata =
                    List.foldl max3 ( 0, 0, 0 ) (getRingdownData model)

                mindata =
                    List.foldl min3 maxdata (getRingdownData model)

                range =
                    { xmin = firstElement mindata
                    , xmax = firstElement maxdata
                    , ymax = toFloat (ceiling (max (secondElement maxdata) (thirdElement maxdata)))
                    , ymin = toFloat (floor (min (secondElement mindata) (thirdElement mindata)))
                    }
            in
            ( { model | crdRange = range }, Cmd.none )

        ToggleCrdPlot input ->
            let
                d =
                    Maybe.withDefault False (ListExtra.getAt input model.crdPlotData)

                newModel =
                    { model | crdPlotData = ListExtra.setAt input (not d) model.crdPlotData }
            in
            ( newModel, Cmd.none )

        SequenceState ->
            let
                newState =
                    if model.cvt.sequence_state == "Run" then
                        "Pause"
                    else
                        "Run"
            in
            ( model, changeSequenceState newState model )

        ResetSequence ->
            ( model, changeSequenceState "Reset" model )


toggleHeaterPid : HeaterID -> Model -> Cmd Msg
toggleHeaterPid id model =
    let
        val =
            case id of
                CrdHeater ->
                    { instr = "crd"
                    , htr = "heater"
                    , val =
                        if model.crd.cvt.heater.enable_pid then
                            0
                        else
                            1
                    }

                Pas0Heater ->
                    { instr = "pas"
                    , htr = "heater0"
                    , val =
                        if model.pas.cvt.heater_0.enable_pid then
                            0
                        else
                            1
                    }

                Pas1Heater ->
                    { instr = "pas"
                    , htr = "heater1"
                    , val =
                        if model.pas.cvt.heater_1.enable_pid then
                            0
                        else
                            1
                    }
    in
    --http://10.172.240.107:8001/soap/heater/enable?heater={value}&instr={value}&val={value}
    Http.send HandleGeneric <|
        Http.getString
            (Network.buildAddress model.network
                ++ "heater/enable?heater="
                ++ val.htr
                ++ "&instr="
                ++ val.instr
                ++ "&val="
                ++ toString val.val
            )


sendHeaterSP : HeaterID -> Model -> Cmd Msg
sendHeaterSP id model =
    let
        htr =
            case id of
                CrdHeater ->
                    { instr = "crd", htr = "heater", sp = model.crd.cvt.heater.sp }

                Pas0Heater ->
                    { instr = "pas", htr = "heater0", sp = model.pas.cvt.heater_0.sp }

                Pas1Heater ->
                    { instr = "pas", htr = "heater1", sp = model.pas.cvt.heater_1.sp }
    in
    --/soap/heater/setpoint?sp={value}&htr={value}&instr={value}
    Http.send HandleGeneric <|
        Http.getString
            (Network.buildAddress model.network
                ++ "heater/setpoint?sp="
                ++ htr.sp
                ++ "&htr="
                ++ htr.htr
                ++ "&instr="
                ++ htr.instr
            )


sendHeaterCtl : HeaterID -> Model -> Cmd Msg
sendHeaterCtl id model =
    let
        val =
            case id of
                CrdHeater ->
                    { instr = "crd"
                    , htr = "heater"
                    , p = Maybe.withDefault "1" (Array.get 0 model.crd.cvt.heater.pid)
                    , i = Maybe.withDefault "0" (Array.get 1 model.crd.cvt.heater.pid)
                    , d = Maybe.withDefault "0" (Array.get 2 model.crd.cvt.heater.pid)
                    }

                Pas0Heater ->
                    { instr = "pas"
                    , htr = "heater0"
                    , p = Maybe.withDefault "1" (Array.get 0 model.pas.cvt.heater_0.pid)
                    , i = Maybe.withDefault "0" (Array.get 1 model.pas.cvt.heater_0.pid)
                    , d = Maybe.withDefault "0" (Array.get 2 model.pas.cvt.heater_0.pid)
                    }

                Pas1Heater ->
                    { instr = "pas"
                    , htr = "heater1"
                    , p = Maybe.withDefault "1" (Array.get 0 model.pas.cvt.heater_1.pid)
                    , i = Maybe.withDefault "0" (Array.get 1 model.pas.cvt.heater_1.pid)
                    , d = Maybe.withDefault "0" (Array.get 2 model.pas.cvt.heater_1.pid)
                    }
    in
    Http.send HandleGeneric <|
        Http.getString
            (Network.buildAddress model.network
                ++ "heater/ctl?heater="
                ++ val.htr
                ++ "&d="
                ++ val.d
                ++ "&i="
                ++ val.i
                ++ "&p="
                ++ val.p
                ++ "&instr="
                ++ val.instr
            )


getCurrentTime : Cmd Msg
getCurrentTime =
    Time.now |> Task.perform UpdateTime


sendNewTime : Time.Time -> Model -> Cmd Msg
sendNewTime t model =
    let
        -- Take the current time which is unix time (1970) and subtract a new zero
        -- (2014) which forms the base for LabVIEW time
        -- Divide by 1000 to convert to seconds
        lvTime =
            (t
                - (DateTime.dateTime { zero | year = 1904 }
                    |> DateTime.toTimestamp
                  )
            )
                / 1000
    in
    Http.send HandleGeneric <|
        Http.getString
            (Network.buildAddress model.network
                ++ "time?t="
                ++ toString lvTime
            )


changeSequenceState : String -> Model -> Cmd Msg
changeSequenceState state model =
    Http.send HandleGeneric <|
        Http.getString
            (Network.buildAddress model.network
                ++ "SequenceState?st="
                ++ state
            )


toggleSave : Model -> Cmd Msg
toggleSave model =
    let
        s =
            if model.save then
                "1"
            else
                "0"
    in
    Http.send HandleGeneric <|
        Http.getString
            (Network.buildAddress model.network
                ++ "SaveMain?save_="
                ++ s
            )


sendDevSp : String -> Device.Device -> Model -> Cmd Msg
sendDevSp idx dev model =
    let
        sp =
            Maybe.withDefault "0" dev.sp
    in
    Http.send HandleGeneric <|
        Http.getString
            (Network.buildAddress model.network
                ++ "UpdateDevSP?sp="
                ++ sp
                ++ "&idx="
                ++ idx
            )


updateWaveforms : Model -> Cmd Msg
updateWaveforms model =
    Http.send HandleGeneric <|
        Http.getString
            (Network.buildAddress model.network
                ++ "PAS/UpdateSerializeWvfm?page="
                ++ toString model.selectedTab
            )


sendFanVoltage : Model -> Cmd Msg
sendFanVoltage model =
    let
        val =
            toString model.cvt.fan_voltage
    in
    Http.send HandleGeneric <|
        Http.getString
            (Network.buildAddress model.network
                ++ "UpdateFanVoltage?voltage="
                ++ val
            )


setCrdRate : Model -> Cmd Msg
setCrdRate model =
    let
        s =
            toString model.crd.cvt.dc
    in
    Http.send HandleGeneric <|
        Http.getString
            (Network.buildAddress model.network
                ++ "CRD/samp_per_cycle?samp_cycle="
                ++ s
            )


setCrdFrequency : Model -> Cmd Msg
setCrdFrequency model =
    let
        f =
            toString model.crd.cvt.rate
    in
    Http.send HandleGeneric <|
        Http.getString
            (Network.buildAddress model.network
                ++ "CRD/laser_rep_rate?RepRate="
                ++ f
            )



-- BEGIN PAS FUNCTIONALITY


sendChirp : Model -> Cmd Msg
sendChirp model =
    let
        df =
            toString model.pas.cvt.spk.df

        center =
            toString model.pas.cvt.spk.center
    in
    Http.send HandleGeneric <|
        Http.getString
            (Network.buildAddress model.network
                ++ "PAS/chirp?fcenter="
                ++ center
                ++ "&df="
                ++ df
            )


toggleSpk : Model -> String -> String -> Cmd Msg
toggleSpk model val cell =
    Http.send HandleGeneric <|
        Http.getString
            (Network.buildAddress model.network
                ++ "PAS/SpeakerState?state="
                ++ val
                ++ "&cell="
                ++ cell
            )



{- Http.send HandleGeneric <|
   Http.getString ((Network.buildAddress model.network) ++"PAS/SpeakerState?state="++ val ++ "&cell=0")
-}


sendSpkVoltage : Model -> Cmd Msg
sendSpkVoltage model =
    Http.send HandleGeneric <|
        Http.getString
            (Network.buildAddress model.network
                ++ "PAS/spk_voltage?Vscale="
                ++ model.pas.cvt.spk.vscale
                ++ "&Voffset="
                ++ model.pas.cvt.spk.voffset
            )


setCellFrequency : Model -> String -> String -> Cmd Msg
setCellFrequency model cell frequency =
    Http.send HandleGeneric <|
        Http.getString
            (Network.buildAddress model.network
                ++ "PAS/mod_frequency?fmod="
                ++ frequency
                ++ "&cell="
                ++ cell
            )



-- END PAS FUNCTIONALITY
-- BEGIN CALIBRATION FUNCTIONALITY


toggleUV : Model -> String -> Cmd Msg
toggleUV model val =
    Http.send HandleGeneric <|
        Http.getString
            (Network.buildAddress model.network
                ++ "ToggleUVLamp?lamp="
                ++ val
            )


toggleO2 : Model -> String -> Cmd Msg
toggleO2 model val =
    Http.send HandleGeneric <|
        Http.getString
            (Network.buildAddress model.network
                ++ "ToggleO2?valve="
                ++ val
            )


toggleO3 : Model -> String -> Cmd Msg
toggleO3 model val =
    Http.send HandleGeneric <|
        Http.getString
            (Network.buildAddress model.network
                ++ "ToggleO3?valve="
                ++ val
            )



-- END CALIBRATION FUNCTIONALITY


toggleFan : Model -> String -> Cmd Msg
toggleFan model val =
    Http.send HandleGeneric <|
        Http.getString
            (Network.buildAddress model.network
                ++ "ToggleFan?enable="
                ++ val
            )


toggleFilter : Model -> Cmd Msg
toggleFilter model =
    Http.send HandleGeneric <|
        Http.getString
            (Network.buildAddress model.network
                ++ "ToggleFilter?cell="
                ++ toString model.cvt.filter.pos
            )


togglePump : Model -> Cmd Msg
togglePump model =
    let
        val =
            if model.cvt.pump then
                "1"
            else
                "0"
    in
    Http.send HandleGeneric <|
        Http.getString (Network.buildAddress model.network ++ "TogglePump?Pump=" ++ val)


toggleCrdPower : Model -> String -> Cmd Msg
toggleCrdPower model val =
    Http.send HandleGeneric <|
        Http.getString
            (Network.buildAddress model.network
                ++ "CRD/cLaserState?state="
                ++ val
            )


togglePasLaserPower : Model -> String -> String -> Cmd Msg
togglePasLaserPower model val cell =
    Http.send HandleGeneric <|
        Http.getString
            (Network.buildAddress model.network
                ++ "PAS/pLaserState?state="
                ++ val
                ++ "&cell="
                ++ cell
            )


shutdownSystem : Model -> Cmd Msg
shutdownSystem model =
    Http.send HandleGeneric <|
        Http.getString (Network.buildAddress model.network ++ "Shutdown")



-- RETRIEVE SERVER DATA PACKETS


getCvtData : Model -> String -> Cmd Msg
getCvtData model force =
    Http.send GetCVT <|
        Http.getString (Network.buildAddress model.network ++ "cvt?force=" ++ force)


getData : Model -> Cmd Msg
getData model =
    Http.send GetData <|
        Http.getString (Network.buildAddress model.network ++ "data")



-- END RETRIEVE SERVER DATA PACKETS
-- MAIN VIEW


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader
        , Layout.waterfall True
        , Layout.onSelectTab SelectTab
        , Layout.selectedTab model.selectedTab
        ]
        { header = pageHeader model
        , drawer = viewDrawer model
        , tabs =
            ( [ Html.text "Main"
              , Html.text "PAS"
              , Html.text "CRD"
              , Html.text "Auxilary"
              , Html.text "Automation"
              , Html.text "Configuration"
              , Html.text "Health"
              ]
            , [ Material.Color.background (Material.Color.color Material.Color.LightBlue Material.Color.S400)
              ]
            )
        , main = [ viewBody model, viewFooter model ]
        }


viewDrawer : Model -> List (Html Msg)
viewDrawer model =
    [ Toggles.switch Mdl
        [ 10 ]
        model.mdl
        [ Toggles.ripple
        , Toggles.value model.cvt.save
        , Material.Options.onToggle SaveData
        ]
        [ Html.text "Save" ]
    , button 1 "Stop Server" model StopServer
    , button 9 "Sync Time" model SyncTime
    , Toggles.radio Mdl
        [ 2 ]
        model.mdl
        [ Toggles.value False
        , Toggles.group "FilterPath"
        , Toggles.ripple
        , Toggles.value (model.cvt.filter.pos == 0)
        , Material.Options.onToggle ToggleFilter
        , css "margin-top" "10px"
        ]
        [ Html.text "Channel 1" ]
    , Html.div []
        [ Toggles.radio Mdl
            [ 3 ]
            model.mdl
            [ Toggles.value True
            , Toggles.group "FilterPath"
            , Toggles.ripple
            , Toggles.value (model.cvt.filter.pos == 1)
            , Material.Options.onToggle ToggleFilter
            , css "margin-top" "10px"
            ]
            [ Html.text "Channel 2" ]
        ]
    , Toggles.switch Mdl
        [ 7 ]
        model.mdl
        [ Toggles.ripple
        , Toggles.value model.cvt.cal_state.o3_add
        , Material.Options.onToggle ToggleO3
        , css "margin-top" "10px"
        ]
        [ Html.text "O3 Addition" ]
    , Toggles.switch Mdl
        [ 4 ]
        model.mdl
        [ Toggles.ripple
        , Toggles.value model.cvt.cal_state.o2_add
        , Material.Options.onToggle ToggleO2
        , css "margin-top" "10px"
        ]
        [ Html.text "O2 Addition" ]
    , Toggles.switch Mdl
        [ 5 ]
        model.mdl
        [ Toggles.ripple
        , Toggles.value model.cvt.pump
        , Material.Options.onToggle TogglePump
        , css "margin-top" "10px"
        ]
        [ Html.text "Pump" ]
    , Toggles.switch Mdl
        [ 6 ]
        model.mdl
        [ Toggles.ripple
        , Toggles.value model.cvt.cal_state.uv_lamp
        , Material.Options.onToggle ToggleUVLamp
        , css "margin-top" "10px"
        ]
        [ Html.text "UV Lamp" ]
    , Toggles.switch Mdl
        [ 8 ]
        model.mdl
        [ Toggles.ripple
        , Toggles.value model.cvt.fan
        , Material.Options.onToggle ToggleFan
        , css "margin-top" "10px"
        ]
        [ Html.text "Fan" ]
    ]


pageHeader : Model -> List (Html msg)
pageHeader model =
    [ Layout.row
        [ Material.Options.nop
        , css "transition" "height 333ms ease-in-out 0s"
        ]
        [ Layout.title [] [ Html.text "NOAA SOAP" ]
        , Layout.spacer
        , Layout.navigation []
            [ Layout.link
                [{- Material.Options.onClick ToggleHeader -}]
                [{- Icon.i "photo" -}]
            , Layout.link
                [ Layout.href "https://esrl.noaa.gov/csd" ]
                [ Html.span [] [ Html.text "noaa" ] ]
            ]
        ]
    ]


viewBody : Model -> Html Msg
viewBody model =
    Grid.grid [ Material.Options.css "padding-bottom" "35px" ]
        [ Grid.cell [ Grid.size Grid.All 12 ]
            [ case model.selectedTab of
                0 ->
                    viewMain model

                1 ->
                    viewPas model

                2 ->
                    viewCrd model

                3 ->
                    viewAux model

                4 ->
                    viewCal model

                5 ->
                    viewConfig model

                6 ->
                    viewStatus model

                _ ->
                    Html.text "404"
            ]
        ]


interlockIcon : Model -> Html m
interlockIcon model =
    if model.genData.interlock then
        Icon.view "lock_outline"
            [ Icon.size24
            , Material.Color.text (Material.Color.color Material.Color.Green Material.Color.S400)
            ]
    else
        Icon.view "lock_open"
            [ Icon.size24
            , Material.Color.text (Material.Color.color Material.Color.Red Material.Color.S400)
            ]


viewFooter : Model -> Html Msg
viewFooter model =
    Footer.mini
        [ css "position" "fixed"
        , css "bottom" "0px"
        , css "width" "100%"
        , css "padding" "5px"
        , Material.Color.background (Material.Color.color Material.Color.LightBlue Material.Color.S400)
        ]
        { left =
            Footer.left []
                [ Footer.logo []
                    [ Footer.html <|
                        Html.text
                            ("Time: " ++ model.genData.time)
                    ]
                ]
        , right =
            Footer.right []
                [ Footer.logo [] [ Footer.html <| Material.Options.span [ Material.Badge.add (toString (Maybe.withDefault 0 (ListExtra.getAt 0 model.msgs))) ] [] ]
                , Footer.logo []
                    [ Footer.html <| interlockIcon model
                    ]
                ]
        }


viewMain : Model -> Html Msg
viewMain model =
    plotData model.runningData 0


viewConfig : Model -> Html Msg
viewConfig model =
    Grid.grid
        [ css "padding-bottom" "50px"

        --Grid.noSpacing
        ]
        [ {- One cell filling the page width -}
          Grid.cell
            [ Grid.size Grid.Desktop 3
            , Grid.size Grid.Tablet 5
            , Grid.size Grid.Phone 3
            ]
            [ textfield 0 model (Just "IP Address") model.network.ip (Network << Network.UpdateIP) Nothing
            , textfield 1 model (Just "Port") model.network.port_ (Network << Network.UpdatePort) Nothing
            ]
        ]


getHeaterPid : Model -> Int -> Int -> String
getHeaterPid model heater pid =
    case heater of
        0 ->
            Maybe.withDefault "0" (Array.get pid model.pas.cvt.heater_0.pid)

        1 ->
            Maybe.withDefault "0" (Array.get pid model.pas.cvt.heater_1.pid)

        2 ->
            Maybe.withDefault "0" (Array.get pid model.crd.cvt.heater.pid)

        _ ->
            "0"



-- setHeaterPid: Model -> Int -> Int -> String


getHeaterSP : Model -> Int -> String
getHeaterSP model heater =
    case heater of
        0 ->
            model.pas.cvt.heater_0.sp

        1 ->
            model.pas.cvt.heater_1.sp

        2 ->
            model.crd.cvt.heater.sp

        _ ->
            ""


viewAux : Model -> Html Msg
viewAux model =
    Grid.grid []
        ([ Grid.cell [ Grid.size Grid.All 12 ]
            [ Material.Options.styled Html.p [ Typo.headline ] [ Html.text "Heater Controls" ] ]
         ]
            ++ List.indexedMap
                (\index heater ->
                    let
                        x =
                            case index of
                                0 ->
                                    Pas0Heater

                                1 ->
                                    Pas1Heater

                                2 ->
                                    CrdHeater

                                _ ->
                                    CrdHeater

                        y =
                            case index of
                                0 ->
                                    model.pas.cvt.heater_0.enable_pid

                                1 ->
                                    model.pas.cvt.heater_1.enable_pid

                                2 ->
                                    model.crd.cvt.heater.enable_pid

                                _ ->
                                    model.crd.cvt.heater.enable_pid

                        z =
                            case index of
                                0 ->
                                    model.genData.pas0_heater.dc

                                1 ->
                                    model.genData.pas1_heater.dc

                                _ ->
                                    model.genData.crd_heater.dc
                    in
                    Grid.cell [ Grid.size Grid.All 2 ]
                        [ Material.Options.div
                            [ css "border-style" "solid"
                            , css "border-width" "1px"
                            , css "border-radius" "5px"
                            , css "padding-left" "10px"
                            , css "padding-top" "10px"
                            ]
                            [ Material.Options.styled Html.p [ Typo.title ] [ Html.text heater ]
                            , Toggles.switch Mdl
                                [ 18 + 5 * index ]
                                model.mdl
                                [ Toggles.ripple
                                , Material.Options.onToggle (ToggleHeaterPid x)
                                , Toggles.value y
                                ]
                                [ Html.text "Power" ]
                            , Textfield.render Mdl
                                [ 19 + 5 * index ]
                                model.mdl
                                [ Textfield.floatingLabel
                                , css "width" "125px"
                                , Textfield.maxlength 15
                                , Textfield.value (getHeaterSP model index)
                                , onInput (UpdateHeaterSP x)
                                , Material.Options.onBlur (SendHeaterSP x)
                                , Textfield.label "Setpoint"
                                ]
                                []
                            , Textfield.render Mdl
                                [ 20 + 5 * index ]
                                model.mdl
                                [ Textfield.floatingLabel
                                , css "width" "125px"
                                , Textfield.maxlength 15
                                , onInput (UpdateHeaterCtl x 0)
                                , onBlur (SendHeaterCtl x)
                                , Textfield.value (getHeaterPid model index 0)
                                , Textfield.label "P"
                                ]
                                []
                            , Textfield.render Mdl
                                [ 21 + 5 * index ]
                                model.mdl
                                [ Textfield.floatingLabel
                                , css "width" "125px"
                                , Textfield.maxlength 15
                                , onInput (UpdateHeaterCtl x 1)
                                , onBlur (SendHeaterCtl x)
                                , Textfield.value (getHeaterPid model index 1)
                                , Textfield.label "I"
                                ]
                                []
                            , Textfield.render Mdl
                                [ 22 + 5 * index ]
                                model.mdl
                                [ Textfield.floatingLabel
                                , css "width" "125px"
                                , Textfield.maxlength 15
                                , onInput (UpdateHeaterCtl x 2)
                                , onBlur (SendHeaterCtl x)
                                , Textfield.value (getHeaterPid model index 2)
                                , Textfield.label "D"
                                ]
                                []
                            , Textfield.render Mdl
                                [ 23 + 5 * index ]
                                model.mdl
                                [ Textfield.floatingLabel
                                , css "width" "125px"
                                , Textfield.maxlength 15
                                , Textfield.value (toString z)
                                , Textfield.label "DC"
                                ]
                                []
                            ]
                        ]
                )
                [ "PAS 0", "PAS 1", "CRD" ]
            ++ [ Grid.cell [ Grid.size Grid.All 12 ]
                    [ Material.Options.styled Html.p
                        [ Typo.headline ]
                        [ Html.text "Fan Control" ]
                    ]
               , Grid.cell [ Grid.size Grid.All 10 ]
                    [ Slider.view
                        [ Slider.value model.cvt.fan_voltage
                        , Slider.onChange UpdateFanVoltage
                        , Slider.max 5
                        , Slider.min 0
                        , Slider.step 0.1
                        ]
                    ]
               , Grid.cell [ Grid.size Grid.All 2 ] [ Html.text (toString model.cvt.fan_voltage) ]
               ]
            ++ [ --Grid.cell [ Grid.size Grid.All 12 ] []
                 --, Grid.cell [ Grid.size Grid.All 3 ] [ Material.Options.styled Html.p [ Typo.headline ] [ Html.text "Temperatures" ] ]
                 Grid.cell [ Grid.size Grid.All 3 ]
                    [ MList.ul []
                        (List.indexedMap
                            (\index temp ->
                                MList.li []
                                    [ MList.content []
                                        [ Html.text temp
                                        ]
                                    , MList.content2
                                        []
                                        [ Html.text
                                            (printableNumeric
                                                (getTemperature index model.genData.temperatures)
                                            )
                                        ]
                                    ]
                            )
                            [ "PAS Channel 1"
                            , "PAS Channel 2"
                            , "PAS Laser Head 1"
                            , "PAS Laser Head 2"
                            , "Box Exit"
                            , "CRD Laser Head"
                            , "Box Inlet"
                            , "CRD Heater"
                            , "CJC 1"
                            ]
                        )
                    ]
               , Grid.cell [ Grid.size Grid.All 12 ]
                    [ Material.Options.styled Html.p
                        [ Typo.headline ]
                        [ Html.text "Device List" ]
                    ]
               , Grid.cell [ Grid.size Grid.All 12 ]
                    [ Table.table []
                        [ Table.thead []
                            [ Table.tr []
                                [ Table.th [] [ Html.text "Device" ]
                                , Table.th [] [ Html.text "Type" ]
                                , Table.th [] [ Html.text "SN" ]
                                , Table.th [] [ Html.text "Address" ]
                                , Table.th [] [ Html.text "Model" ]
                                , Table.th [] [ Html.text "Active?" ]
                                ]
                            ]
                        , Table.tbody []
                            (List.map
                                (\dev ->
                                    let
                                        device =
                                            Tuple.second dev

                                        c =
                                            if device.active then
                                                "black"
                                            else
                                                "red"
                                    in
                                    Table.tr [ css "color" c ]
                                        [ Table.td [] [ Html.text device.label ]
                                        , Table.td [] [ Html.text device.type_ ]
                                        , Table.td [] [ Html.text device.sn ]
                                        , Table.td [] [ Html.text device.address ]
                                        , Table.td [] [ Html.text device.model ]
                                        , Table.td [] [ Html.text (toString device.active) ]
                                        ]
                                )
                                (List.concat
                                    [ Dict.toList model.alicats.cvt
                                    , Dict.toList model.vaisalas.cvt
                                    , Dict.toList model.ppts.cvt
                                    ]
                                )
                            )
                        ]
                    ]
               , Grid.cell [ Grid.size Grid.All 12 ]
                    [ Material.Options.styled Html.p
                        [ Typo.headline ]
                        [ Html.text "Alicat" ]
                    ]
               , Grid.cell [ Grid.size Grid.All 2 ]
                    (List.indexedMap
                        (\i dev ->
                            let
                                idx =
                                    Tuple.first dev

                                device =
                                    Tuple.second dev

                                m =
                                    if device.active && device.controller then
                                        Textfield.render Mdl
                                            [ 12 + i ]
                                            model.mdl
                                            [ Textfield.floatingLabel
                                            , css "width" "125px"
                                            , Textfield.maxlength 15
                                            , Textfield.value (Maybe.withDefault "0" device.sp)
                                            , onInput (UpdateDevSP idx)
                                            , onBlur (SendDevSP idx)
                                            , Textfield.label device.label
                                            ]
                                            []
                                    else
                                        Html.text ""
                            in
                            m
                        )
                        (Dict.toList model.alicats.cvt)
                    )
               , Grid.cell [ Grid.size Grid.All 10 ]
                    [ Table.table []
                        [ Table.thead []
                            [ Table.tr []
                                [ Table.th [] [ Html.text "ID" ]
                                , Table.th [] [ Html.text "Pressure" ]
                                , Table.th [] [ Html.text "Temperature" ]
                                , Table.th [] [ Html.text "Flow Rate" ]
                                , Table.th [] [ Html.text "Setpoint" ]
                                , Table.th [] [ Html.text "Mass Flow Rate" ]
                                ]
                            , Table.tr []
                                [ Table.th [] []
                                , Table.th [] [ Html.text "mb" ]
                                , Table.th [] [ Html.text "degC" ]
                                , Table.th [] [ Html.text "lpm" ]
                                , Table.th [] [ Html.text "lpm" ]
                                , Table.th [] [ Html.text "slpm" ]
                                ]
                            ]
                        , Table.tbody []
                            (List.map
                                (\dev ->
                                    let
                                        id =
                                            Tuple.first dev

                                        dcvt =
                                            Tuple.second dev

                                        data =
                                            Maybe.withDefault Alicat.defaultData
                                                (Dict.get id model.alicats.data)
                                    in
                                    Table.tr []
                                        [ Table.td [] [ Html.text dcvt.label ]
                                        , Table.td [] [ Html.text (toString (Maybe.withDefault 0 data.pressure)) ]
                                        , Table.td [] [ Html.text (toString (Maybe.withDefault 0 data.temperature)) ]
                                        , Table.td [] [ Html.text (toString data.output) ]
                                        , Table.td [] [ Html.text (toString (Maybe.withDefault 0 data.setpoint)) ]
                                        , Table.td [] [ Html.text (toString (Maybe.withDefault 0 data.mass_flow)) ]
                                        ]
                                )
                                (Dict.toList
                                    model.alicats.cvt
                                )
                            )
                        ]
                    ]
               , Grid.cell [ Grid.size Grid.All 12 ]
                    [ Material.Options.styled Html.p
                        [ Typo.headline ]
                        [ Html.text "Vaisala" ]
                    ]
               , Grid.cell [ Grid.size Grid.All 10 ]
                    [ Table.table []
                        [ Table.thead []
                            [ Table.tr []
                                [ Table.th [] [ Html.text "ID" ]
                                , Table.th [] [ Html.text "Temperature" ]
                                , Table.th [] [ Html.text "Relative Humidity" ]
                                , Table.th [] [ Html.text "Dew Point" ]
                                ]
                            , Table.tr []
                                [ Table.th [] []
                                , Table.th [] [ Html.text "degC" ]
                                , Table.th [] [ Html.text "%" ]
                                , Table.th [] [ Html.text "degC" ]
                                ]
                            ]
                        , Table.tbody []
                            (List.map
                                (\dev ->
                                    let
                                        id =
                                            Tuple.first dev

                                        dcvt =
                                            Tuple.second dev

                                        data =
                                            Maybe.withDefault (Vaisala.Data 0 0 0)
                                                (Dict.get id model.vaisalas.data)
                                    in
                                    Table.tr []
                                        [ Table.td [] [ Html.text dcvt.label ]
                                        , Table.td [] [ Html.text (toString data.temperature) ]
                                        , Table.td [] [ Html.text (toString data.relative_humidity) ]
                                        , Table.td [] [ Html.text (toString data.dewpoint) ]
                                        ]
                                )
                                (Dict.toList
                                    model.vaisalas.cvt
                                )
                            )
                        ]
                    ]
               , Grid.cell [ Grid.size Grid.All 12 ]
                    [ Material.Options.styled Html.p
                        [ Typo.headline ]
                        [ Html.text "Honeywell PPT" ]
                    ]
               , Grid.cell [ Grid.size Grid.All 10 ]
                    [ Table.table []
                        [ Table.thead []
                            [ Table.tr []
                                [ Table.th [] [ Html.text "ID" ]
                                , Table.th [] [ Html.text "Pressure" ]
                                , Table.th [] [ Html.text "Temperature" ]
                                ]
                            , Table.tr []
                                [ Table.th [] []
                                , Table.th [] [ Html.text "mb" ]
                                , Table.th [] [ Html.text "degC" ]
                                ]
                            ]
                        , Table.tbody []
                            (List.map
                                (\dev ->
                                    let
                                        id =
                                            Tuple.first dev

                                        dcvt =
                                            Tuple.second dev

                                        data =
                                            Maybe.withDefault (Ppt.Data 0 0)
                                                (Dict.get id model.ppts.data)
                                    in
                                    Table.tr []
                                        [ Table.td [] [ Html.text dcvt.label ]
                                        , Table.td [] [ Html.text (toString data.pressure) ]
                                        , Table.td [] [ Html.text (toString data.temperature) ]
                                        ]
                                )
                                (Dict.toList
                                    model.ppts.cvt
                                )
                            )
                        ]
                    ]
               ]
        )


viewPas : Model -> Html Msg
viewPas model =
    Grid.grid []
        [ Grid.cell [ Grid.size Grid.Desktop 2, Grid.size Grid.Tablet 4, Grid.size Grid.Phone 5 ]
            [ Material.Options.div
                [ css "border-style" "solid"
                , css "border-width" "1px"
                , css "border-radius" "5px"
                , css "padding-left" "10px"
                , css "padding-top" "10px"
                ]
                [ Material.Options.styled Html.p [ Typo.title ] [ Html.text "Laser Input" ]
                , toggle 18 "Power 1" model.pas.cvt.enable_0 model (TogglePasLaserPower 0)
                , intTextfield 11
                    model
                    (Just "Channel 1")
                    model.pas.cvt.fmod_0
                    UpdateMod0
                    (Just (SendModulation <| "0"))
                , toggle 19 "Power 2" model.pas.cvt.enable_1 model (TogglePasLaserPower 1)
                , intTextfield 12
                    model
                    (Just "Channel 2")
                    model.pas.cvt.fmod_1
                    UpdateMod1
                    (Just (SendModulation <| "1"))
                ]
            , Material.Options.div
                [ css "border-style" "solid"
                , css "border-width" "1px"
                , css "border-radius" "5px"
                , css "padding-left" "10px"
                , css "padding-top" "10px"
                , css "margin-top" "10px"
                ]
                [ Material.Options.styled Html.p [ Typo.title ] [ Html.text "Speaker Input" ]
                , toggle 10 "Channel 1" model.pas.cvt.speaker_0 model (ToggleSpeaker 0)
                , toggle 11 "Channel 2" model.pas.cvt.speaker_1 model (ToggleSpeaker 1)
                , txtfield 14
                    model
                    (Just "Center (Hz)")
                    model.pas.cvt.spk.center
                    (Pas << Pas.UpdateSpkFcenter)
                    (Just UpdateChirp)
                , txtfield 15
                    model
                    (Just "Df (Hz)")
                    model.pas.cvt.spk.df
                    (Pas << Pas.UpdateSpkDf)
                    (Just UpdateChirp)
                , txtfield 16
                    model
                    (Just "Vscale (V)")
                    model.pas.cvt.spk.vscale
                    (Pas << Pas.UpdateSpkVscale)
                    (Just SendSpkVoltage)
                , txtfield 17
                    model
                    (Just "Voffset (V)")
                    model.pas.cvt.spk.voffset
                    (Pas << Pas.UpdateSpkVoffset)
                    (Just SendSpkVoltage)
                ]
            ]
        , Grid.cell
            [ Grid.size Grid.Desktop 10
            , Grid.size Grid.Tablet 8
            , Grid.size Grid.Phone 7
            ]
            [ Grid.grid []
                [ Grid.cell [ Grid.size Grid.All 12 ]
                    [ Table.table []
                        [ Table.thead []
                            [ Table.tr []
                                [ Table.th [] [ Html.text "Cell" ]
                                , Table.th [] [ Html.span [ property "innerHTML" (string "f<sub>0</sub>") ] [] ]
                                , Table.th [] [ Html.text "Q" ]
                                , Table.th [] [ Html.text "IA" ]
                                , Table.th [] [ Html.span [ property "innerHTML" (string "&sigma;<sub>abs</sub>") ] [] ]
                                , Table.th [] [ Html.span [ property "innerHTML" (string "V<sub>rms</sub>") ] [] ]
                                ]
                            , Table.tr []
                                [ Table.th [] []
                                , Table.th [] [ Html.text "(Hz)" ]
                                , Table.th [] [ Html.text "(a.u.)" ]
                                , Table.th [] [ Html.text "(a.u.)" ]
                                , Table.th [] [ Html.span [ property "innerHTML" (string "(Mm<sup>-1</sup>)") ] [] ]
                                , Table.th [] [ Html.text "(V)" ]
                                ]
                            ]
                        , Table.tbody []
                            (List.map
                                (\cell ->
                                    let
                                        name =
                                            Tuple.first cell

                                        data =
                                            Tuple.second cell
                                    in
                                    Table.tr []
                                        [ Table.td [] [ Html.text name ]
                                        , Table.td [] [ Html.text (printableNumeric data.resonant_frequency) ]
                                        , Table.td [] [ Html.text (printableNumeric data.q) ]
                                        , Table.td [] [ Html.text (printableNumeric data.integrated_area) ]
                                        , Table.td [] [ Html.text (printableNumeric data.absorption) ]
                                        , Table.td [] [ Html.text (printableNumeric data.laserRMS) ]
                                        ]
                                )
                             <|
                                List.map2 (,) [ "Channel 1", "Channel 2" ] (Array.toList model.pas.data.cell)
                            )
                        ]
                    ]
                , Grid.cell
                    [ Grid.size Grid.All 12
                    ]
                    [ let
                        p =
                            if Maybe.withDefault False (ListExtra.getAt 2 model.pasPlotData) then
                                MicFreq
                            else
                                MicTime

                        max_ =
                            if p == MicFreq then
                                1200
                            else
                                0
                      in
                      timeData model model.pasRange (getPasTimeData model max_ p) (List.take 2 model.pasPlotData)
                    ]
                , Grid.cell [ Grid.size Grid.All 12 ]
                    [ Toggles.checkbox Mdl
                        [ 100 ]
                        model.mdl
                        [ Toggles.value (Maybe.withDefault False (ListExtra.getAt 0 model.pasPlotData))
                        , Toggles.ripple
                        , Material.Options.onToggle (TogglePasPlot 0)
                        , css "color" "blue"
                        ]
                        [ Html.text "Channel 1" ]
                    , Toggles.checkbox Mdl
                        [ 101 ]
                        model.mdl
                        [ Toggles.value (Maybe.withDefault False (ListExtra.getAt 1 model.pasPlotData))
                        , Material.Options.onToggle (TogglePasPlot 1)
                        , Toggles.ripple
                        , css "color" "red"
                        ]
                        [ Html.text "Channel 2" ]
                    , Toggles.switch Mdl
                        [ 102 ]
                        model.mdl
                        [ Toggles.value
                            (Maybe.withDefault False
                                (ListExtra.getAt 2
                                    model.pasPlotData
                                )
                            )
                        , Material.Options.onToggle (TogglePasPlot 2)
                        ]
                        [ let
                            s =
                                if Maybe.withDefault False (ListExtra.getAt 2 model.pasPlotData) then
                                    "Frequency"
                                else
                                    "Time"
                          in
                          Html.text s
                        ]
                    , Textfield.render Mdl
                        [ 103 ]
                        model.mdl
                        [ Textfield.floatingLabel
                        , Textfield.value (toString model.pasRange.xmin)
                        , Material.Options.onInput (UpdatePasRange "xmin")
                        , css "width" "125px"
                        , Textfield.maxlength 15
                        , Textfield.label "xMin"
                        ]
                        []
                    , Textfield.render Mdl
                        [ 103 ]
                        model.mdl
                        [ Textfield.floatingLabel
                        , Textfield.value (toString model.pasRange.xmax)
                        , Material.Options.onInput (UpdatePasRange "xmax")
                        , css "width" "125px"
                        , Textfield.maxlength 15
                        , Textfield.label "xMax"
                        ]
                        []
                    , Textfield.render Mdl
                        [ 104 ]
                        model.mdl
                        [ Textfield.floatingLabel
                        , Textfield.value (toString model.pasRange.ymin)
                        , Material.Options.onInput (UpdatePasRange "ymin")
                        , css "width" "125px"
                        , Textfield.maxlength 15
                        , Textfield.label "yMin"
                        ]
                        []
                    , Textfield.render Mdl
                        [ 105 ]
                        model.mdl
                        [ Textfield.floatingLabel
                        , Textfield.value (toString model.pasRange.ymax)
                        , Material.Options.onInput (UpdatePasRange "ymax")
                        , css "width" "125px"
                        , Textfield.maxlength 15
                        , Textfield.label "yMax"
                        ]
                        []
                    , button 106 "Autoscale 1x" model UpdatePasScaling
                    ]
                ]
            ]
        ]


viewCrd : Model -> Html Msg
viewCrd model =
    Grid.grid []
        [ Grid.cell [ Grid.size Grid.All 2 ]
            [ Textfield.render Mdl
                [ 1 ]
                model.mdl
                [ Textfield.label "f (Hz)"
                , Textfield.floatingLabel
                , Material.Options.css "width" "75px"
                , Textfield.maxlength 15
                , Textfield.value (getNumericField 0 model.crd.cvt.rate)
                , Material.Options.onInput (Crd << Crd.UpdateFrequency)
                , onBlur SendCrdFrequency
                ]
                []
            , Textfield.render Mdl
                [ 2 ]
                model.mdl
                [ Textfield.label "Samples per Cycle"
                , Textfield.floatingLabel
                , Material.Options.css "width" "75px"
                , Textfield.maxlength 4
                , Textfield.value (toString model.crd.cvt.dc)
                , onInput (Crd << Crd.UpdateDutyCycle)
                , onBlur SendCrdSampleRate
                ]
                []
            , toggle 11 "Power" model.crd.cvt.power model ToggleCrdPower
            ]
        , Grid.cell [ Grid.size Grid.Desktop 10, Grid.size Grid.Tablet 6 ]
            [ Grid.grid []
                [ Grid.cell [ Grid.size Grid.All 12 ]
                    [ Table.table []
                        [ Table.thead []
                            [ Table.tr []
                                [ Table.th [] [ Html.text "ID" ]
                                , Table.th [] [ Html.span [ property "innerHTML" (string "&tau;") ] [] ]
                                , Table.th [] [ Html.span [ property "innerHTML" (string "&tau;<sub>0</sub>") ] [] ]
                                , Table.th [] [ Html.span [ property "innerHTML" (string "&tau;<sub>0</sub>'") ] [] ]
                                , Table.th [] [ Html.span [ property "innerHTML" (string "&tau;'") ] [] ]
                                , Table.th [] [ Html.span [ property "innerHTML" (string "&sigma;") ] [] ]
                                , Table.th [] [ Html.text "max" ]
                                ]
                            , Table.tr []
                                [ Table.th [] []
                                , Table.th [] [ Html.text "(us)" ]
                                , Table.th [] [ Html.text "(us)" ]
                                , Table.th [] [ Html.text "(us)" ]
                                , Table.th [] [ Html.text "(us)" ]
                                , Table.th [] [ Html.span [ property "innerHTML" (string "Mm<sup>-1</sup>") ] [] ]
                                , Table.th [] [ Html.text "(a.u.)" ]
                                ]
                            ]
                        , Table.tbody []
                            (List.map
                                (\cell ->
                                    let
                                        id =
                                            Tuple.first cell

                                        data =
                                            Tuple.second cell
                                    in
                                    Table.tr []
                                        [ Table.td [] [ Html.text id ]
                                        , Table.td [] [ Html.text (printableNumeric data.tau) ]
                                        , Table.td [] [ Html.text (printableNumeric data.tau0) ]
                                        , Table.td [] [ Html.text (printableNumeric data.tau0corr) ]
                                        , Table.td [] [ Html.text (printableNumeric data.tauCorrected) ]
                                        , Table.td [] [ Html.text (printableNumeric data.extinction) ]
                                        , Table.td [] [ Html.text (printableNumeric data.max) ]
                                        ]
                                )
                             <|
                                List.map2 (,) model.crd.cvt.labels (Array.toList model.crd.data)
                            )
                        ]
                    ]
                , Grid.cell [ Grid.size Grid.All 12 ]
                    [ timeData model model.crdRange (getRingdownData model) model.crdPlotData
                    ]
                , Grid.cell [ Grid.size Grid.All 12 ]
                    [ Toggles.checkbox Mdl
                        [ 100 ]
                        model.mdl
                        [ Toggles.value (Maybe.withDefault False (ListExtra.getAt 0 model.crdPlotData))
                        , Toggles.ripple
                        , Material.Options.onToggle (ToggleCrdPlot 0)
                        , css "color" "blue"
                        ]
                        [ Html.text "Channel 1" ]
                    , Toggles.checkbox Mdl
                        [ 101 ]
                        model.mdl
                        [ Toggles.value (Maybe.withDefault False (ListExtra.getAt 1 model.crdPlotData))
                        , Material.Options.onToggle (ToggleCrdPlot 1)
                        , Toggles.ripple
                        , css "color" "red"
                        ]
                        [ Html.text "Channel 2" ]
                    , Textfield.render Mdl
                        [ 103 ]
                        model.mdl
                        [ Textfield.floatingLabel
                        , Textfield.value (toString model.crdRange.xmin)
                        , Material.Options.onInput (UpdateCrdRange "xmin")
                        , css "width" "125px"
                        , Textfield.maxlength 15
                        , Textfield.label "xMin"
                        ]
                        []
                    , Textfield.render Mdl
                        [ 103 ]
                        model.mdl
                        [ Textfield.floatingLabel
                        , Textfield.value (toString model.crdRange.xmax)
                        , Material.Options.onInput (UpdateCrdRange "xmax")
                        , css "width" "125px"
                        , Textfield.maxlength 15
                        , Textfield.label "xMax"
                        ]
                        []
                    , Textfield.render Mdl
                        [ 104 ]
                        model.mdl
                        [ Textfield.floatingLabel
                        , Textfield.value (toString model.crdRange.ymin)
                        , Material.Options.onInput (UpdateCrdRange "ymin")
                        , css "width" "125px"
                        , Textfield.maxlength 15
                        , Textfield.label "yMin"
                        ]
                        []
                    , Textfield.render Mdl
                        [ 105 ]
                        model.mdl
                        [ Textfield.floatingLabel
                        , Textfield.value (toString model.crdRange.ymax)
                        , Material.Options.onInput (UpdateCrdRange "ymax")
                        , css "width" "125px"
                        , Textfield.maxlength 15
                        , Textfield.label "yMax"
                        ]
                        []
                    , button 106 "Autoscale 1x" model UpdateCrdScaling
                    ]
                , Grid.cell [ Grid.size Grid.All 12 ]
                    [ plotData model.crdRunningData 0
                    ]
                ]
            ]
        ]


viewCal : Model -> Html Msg
viewCal model =
    Grid.grid []
        [ Grid.cell [ Grid.size Grid.All 12 ]
            [ Toggles.switch Mdl
                [ 11 ]
                model.mdl
                [ Toggles.ripple
                , Toggles.value (model.cvt.sequence_state == "Run")
                , Material.Options.onToggle SequenceState
                ]
                [ Html.text "Run Sequence" ]
            , button 106 "Reset Sequence" model ResetSequence
            ]
        , Grid.cell [ Grid.size Grid.All 3 ]
            [ MList.ul []
                [ MList.li [ MList.withBody ]
                    [ MList.content []
                        [ Html.text "Speaker"
                        , MList.body [] [ Html.text "Use this to toggle the speaker.  " ]
                        ]
                    ]
                , MList.li [ MList.withBody ]
                    [ MList.content []
                        [ Html.text "Flow Path"
                        , MList.body []
                            [ Html.text "Use this to toggle the flow path. "
                            ]
                        ]
                    ]
                , MList.li [ MList.withBody ]
                    [ MList.content []
                        [ Html.text "UV Lamp"
                        , MList.body []
                            [ Html.text "Toggle power to the UV lamp."
                            ]
                        ]
                    ]
                , MList.li [ MList.withBody ]
                    [ MList.content []
                        [ Html.text "O2 Valve"
                        , MList.body []
                            [ Html.text "Toggle O2 valve."
                            ]
                        ]
                    ]
                , MList.li [ MList.withBody ]
                    [ MList.content []
                        [ Html.text "O3 Valve"
                        , MList.body []
                            [ Html.text "Toggle O3 valve."
                            ]
                        ]
                    ]
                ]
            ]
        , Grid.cell [ Grid.size Grid.All 3 ]
            [ MList.ul []
                [ MList.li []
                    [ MList.content []
                        [ Html.text "Speaker" ]
                    , MList.content2 []
                        [ Toggles.checkbox Mdl
                            [ 4 ]
                            model.mdl
                            [ Toggles.value True
                            ]
                            []
                        ]
                    ]
                , MList.li []
                    [ MList.content [] [ Html.text "Radio button!" ]
                    , MList.content2 []
                        [ Material.Options.span
                            [ MList.action2 ]
                            [ Toggles.radio Mdl
                                [ 5 ]
                                model.mdl
                                [ Toggles.value True
                                , Material.Options.css "display" "inline"
                                ]
                                []
                            ]
                        ]
                    ]
                , MList.li []
                    [ MList.content [] [ Html.text "Include switch?" ]
                    , MList.content2 []
                        [ Toggles.switch Mdl
                            [ 6 ]
                            model.mdl
                            [ Toggles.value True
                            ]
                            []
                        ]
                    ]
                ]
            ]
        ]


viewStatus : Model -> Html Msg
viewStatus model =
    Grid.grid []
        [ Grid.cell [ Grid.size Grid.All 12 ]
            [ Material.Options.styled Html.p
                [ Typo.title
                ]
                [ Html.text "System Messages" ]
            , Material.Options.div
                [ css "max-height" "250px"
                , css "overflow" "scroll"
                , css "border" "1px solid grey"
                , css "border-radius" "4px"
                ]
                (List.map
                    (\msg ->
                        let
                            msg_color =
                                if String.contains "[ERROR]" msg then
                                    "red"
                                else if String.contains "[WARNING]" msg then
                                    "yellow"
                                else
                                    "black"
                        in
                        Material.Options.styled Html.p
                            [ Typo.body1, css "color" msg_color, css "width" "100%", css "padding" "0", css "margin" "0" ]
                            [ Html.text msg ]
                    )
                 <|
                    model.currentMsgList
                )
            , button 10 "Clear" model ClearMessages
            ]
        ]


ringdownTitle : Svg msg
ringdownTitle =
    Plot.viewLabel
        [ Svg.Attributes.fill "#afafaf"
        , Svg.Attributes.style "text-anchor: end; font-style: italic;"
        ]
        "Ringdown Data"


{-| This takes enumerated time data for two cells and produces a plot containing
the data from those two cells. The data is assumed to be in the order of
('index', 'cell_1_data', 'cell_2_data').
-}
timeData : Model -> RangeData -> List ( Float, Float, Float ) -> List Bool -> Html Msg
timeData model range data selectData =
    let
        cell1 coords =
            List.map (\( x, y, z ) -> Plot.dot (Plot.viewCircle 2 "red") x z) coords

        cell0 coords =
            List.map (\( x, y, z ) -> Plot.dot (Plot.viewCircle 2 "blue") x y) coords

        seriesList =
            List.filterMap
                (\x ->
                    if Tuple.second x then
                        Just (Tuple.first x)
                    else
                        Nothing
                )
                (List.map2 (,) [ Plot.line <| cell0, Plot.line <| cell1 ] selectData)
    in
    Plot.viewSeriesCustom
        { defaultSeriesPlotCustomizations
            | height = 200
            , toRangeLowest = \y -> range.xmin
            , toRangeHighest = \y -> range.xmax
            , toDomainLowest = \y -> range.ymin
            , toDomainHighest = \y -> range.ymax
            , margin = { top = 25, right = 25, bottom = 25, left = 50 }

            --, junk = \summary -> [ Plot.junk ringdownTitle 100 1500 ]
        }
        seriesList
        data


selectData : Bool -> Plot.Series data msg -> Maybe (Plot.Series data msg)
selectData selected_ data =
    if selected_ then
        Just data
    else
        Nothing


{-| Helper function for customizing the lower part of a plot range.
-}
lowestRange : Float -> Float -> Float
lowestRange low y =
    min low y


{-| Helper function for customizing the upper portion of a plot range
-}
highestRange : Float -> Float -> Float
highestRange high y =
    max high y


dataAxis : Plot.Axis
dataAxis =
    Plot.customAxis <|
        \summary ->
            { position = Plot.closestToZero
            , axisLine = Just (Plot.simpleLine summary)
            , ticks = []
            , labels = []
            , flipAnchor = False
            }


plotData : List (List Float) -> Int -> Html Msg
plotData data data_index =
    let
        pdata =
            List.indexedMap (\idx d -> ( toFloat idx, d )) (findAll data_index data)

        plotf coords =
            List.map (\( x, y ) -> Plot.dot (Plot.viewCircle 2 "blue") x y) coords

        plotCustomizations =
            pcs { min = \y -> max y 0, max = \y -> min y 1500 } { min = identity, max = identity }
    in
    Plot.viewSeriesCustom plotCustomizations
        [ Plot.line <| plotf ]
        pdata


{-| Retrieve the ringdown data for plotting.
-}
getRingdownData : Model -> List ( Float, Float, Float )
getRingdownData model =
    let
        cell_0 =
            Maybe.withDefault (Crd.CrdsCell 0 0 0 0 0 0 0 0 0 [ [ 0 ] ]) (Array.get 0 model.crd.data)

        cell_1 =
            Maybe.withDefault (Crd.CrdsCell 0 0 0 0 0 0 0 0 0 [ [ 0 ] ]) (Array.get 1 model.crd.data)

        raw_data_0 =
            Maybe.withDefault [] (List.head cell_0.ringdowns)

        raw_data_1 =
            Maybe.withDefault [] (List.head cell_1.ringdowns)
    in
    List.map3
        (\i a b -> ( i, a, b ))
        (convertToFloat (List.range 0 (List.length raw_data_0)))
        (convertToFloat raw_data_0)
        (convertToFloat raw_data_1)


{-| Basic type for expressing exactly what time data we need from the PAS.
-}
type PasDataType
    = MicFreq
    | MicTime
    | PhotoDiodDiode


{-| Retrieve the time waveform data from the PAS data structure. The waveform
data is microphone time domain, microphone frequency domain and photodiode time
domain data.
-}
getPasTimeData : Model -> Float -> PasDataType -> List ( Float, Float, Float )
getPasTimeData model xstart dataType =
    let
        cell_0 =
            Maybe.withDefault
                (Pas.PasCell 0 0 0 [ 0, 0 ] 0 0 [ 0 ] [ 0 ] [ 0 ] [ 0 ] 0)
                (Array.get 0 model.pas.data.cell)

        cell_1 =
            Maybe.withDefault
                (Pas.PasCell 0 0 0 [ 0, 0 ] 0 0 [ 0 ] [ 0 ] [ 0 ] [ 0 ] 0)
                (Array.get 1 model.pas.data.cell)

        data_in =
            case dataType of
                MicFreq ->
                    ( cell_0.frequencyData, cell_1.frequencyData )

                MicTime ->
                    ( convertToFloat cell_0.timeData, convertToFloat cell_1.timeData )

                PhotoDiodDiode ->
                    ( convertToFloat cell_0.laserDiodeData, convertToFloat cell_1.laserDiodeData )
    in
    List.map3 (\i a b -> ( xstart + i, a, b ))
        (convertToFloat (List.range 0 (List.length (Tuple.first data_in))))
        (Tuple.first data_in)
        (Tuple.second data_in)


{-| Convert a list of ints to a list of floats.
-}
convertToFloat : List Int -> List Float
convertToFloat =
    List.map (\a -> toFloat a)


{-| Generate a numeric that looks ok based on the size. If the number is between a thousandth and
a thousand, convert it to a a float with a precision of 2. If outside these bounds, then convert it
to a scientific expression.
TODO: Make this function more flexible...
-}
printableNumeric : Float -> String
printableNumeric number =
    if number >= 0.001 || number <= 1000 then
        Round.round 2 number
    else
        toScientific 2 number


{-| Generates a string for
-}
toScientific : Int -> Float -> String
toScientific x y =
    let
        exp =
            floor (logBase 10 y)

        num =
            Round.round x (y / 10 ^ toFloat exp)

        e =
            if exp < 10 then
                "0" ++ toString exp
            else
                toString exp
    in
    num ++ "e" ++ e


getNumericField : Int -> Int -> String
getNumericField reject num =
    if num == reject then
        ""
    else
        toString num


{-| Takes a list of data and appends it to a larger time based list. The size defines
how large the time based list will be.
-}
addDataToList : Int -> List Float -> List (List Float) -> List (List Float)
addDataToList size newData oldData =
    if List.length oldData >= size then
        List.append [ newData ] (List.take (size - 1) oldData)
    else if List.isEmpty oldData then
        [ newData ]
    else
        List.append [ newData ] oldData


{-| Takes an index and a list of lists to return a single list consisting of elements in
the individual lists at the index provided.
-}
findAll : Int -> List (List a) -> List a
findAll idx lists =
    List.filterMap (ListExtra.getAt idx) lists


max3 : ( Float, Float, Float ) -> ( Float, Float, Float ) -> ( Float, Float, Float )
max3 ( x, y, z ) ( newX, newY, newZ ) =
    ( max x newX, max y newY, max z newZ )


min3 : ( Float, Float, Float ) -> ( Float, Float, Float ) -> ( Float, Float, Float )
min3 ( x, y, z ) ( newX, newY, newZ ) =
    ( min x newX, min y newY, min z newZ )


thirdElement : ( a, b, c ) -> c
thirdElement ( _, _, c ) =
    c


firstElement : ( a, b, c ) -> a
firstElement ( a, _, _ ) =
    a


secondElement : ( a, b, c ) -> b
secondElement ( _, b, _ ) =
    b



{- START HELPER FUNCTIONS FOR DEALING WITH MATERIAL BASED UI STUFF... -}


textfield :
    Int -- mdl id
    -> Model
    -> Maybe String -- label for the text field
    -> String --value of text field
    -> (String -> Msg) -- message handled by onInput
    -> Maybe Msg -- possible message handled by onBlur
    -> Html Msg
textfield num model label value input_msg blur_msg =
    Textfield.render Mdl
        [ num ]
        model.mdl
        ([ Textfield.floatingLabel
         , css "width" "125px"
         , Textfield.maxlength 15
         , Textfield.value value
         , onInput input_msg
         ]
            ++ (label
                    |> Maybe.map (\lab -> [ Textfield.label lab ])
                    |> Maybe.withDefault []
               )
            ++ -- Handle onBlur events if there
               (blur_msg
                    |> Maybe.map
                        (\msg -> [ onBlur msg ])
                    |> Maybe.withDefault []
               )
        )
        []



-- TODO: Move MDL formatting to different file along with app.css for consistent formatting


floatTextfield :
    Int -- mdl id
    -> Model
    -> Maybe String -- label for the text field
    -> Float --value of text field
    -> (String -> Msg) -- message handled by onInput
    -> Maybe Msg -- possible message handled by onBlur
    -> Html Msg
floatTextfield num model label value input_msg blur_msg =
    Textfield.render Mdl
        [ num ]
        model.mdl
        ([ Textfield.floatingLabel
         , css "width" "125px"
         , Textfield.maxlength 15
         , Textfield.value (toString value)
         , onInput input_msg
         ]
            ++ (label
                    |> Maybe.map (\lab -> [ Textfield.label lab ])
                    |> Maybe.withDefault []
               )
            ++ -- Handle onBlur events if there
               (blur_msg
                    |> Maybe.map
                        (\msg -> [ onBlur msg ])
                    |> Maybe.withDefault []
               )
        )
        []


button : Int -> String -> Model -> Msg -> Html Msg
button num btn_txt model msg =
    Material.Button.render Mdl
        [ num ]
        model.mdl
        [ Material.Button.raised
        , Material.Button.ripple
        , Material.Button.primary
        , Material.Options.onClick msg
        , css "margin-top" "10px"
        , css "width" "125px"
        ]
        [ Html.text btn_txt ]



-- TODO: Move MDL formatting to different file along with app.css for consistent formatting


intTextfield :
    Int -- mdl id
    -> Model
    -> Maybe String -- label for the text field
    -> Int --value of text field
    -> (String -> Msg) -- message handled by onInput
    -> Maybe Msg -- possible message handled by onBlur
    -> Html Msg
intTextfield num model label value input_msg blur_msg =
    Textfield.render Mdl
        [ num ]
        model.mdl
        ([ Textfield.floatingLabel
         , css "width" "125px"
         , Textfield.maxlength 15
         , Textfield.value (toString value)
         , onInput input_msg
         ]
            ++ (label
                    |> Maybe.map (\lab -> [ Textfield.label lab ])
                    |> Maybe.withDefault []
               )
            ++ -- Handle onBlur events if there
               (blur_msg
                    |> Maybe.map
                        (\msg -> [ onBlur msg ])
                    |> Maybe.withDefault []
               )
        )
        []


txtfield :
    Int -- mdl id
    -> Model
    -> Maybe String -- label for the text field
    -> String --value of text field
    -> (String -> Msg) -- message handled by onInput
    -> Maybe Msg -- possible message handled by onBlur
    -> Html Msg
txtfield num model label value input_msg blur_msg =
    Textfield.render Mdl
        [ num ]
        model.mdl
        ([ Textfield.floatingLabel
         , css "width" "125px"
         , Textfield.maxlength 15
         , Textfield.value value
         , onInput input_msg
         ]
            ++ (label
                    |> Maybe.map (\lab -> [ Textfield.label lab ])
                    |> Maybe.withDefault []
               )
            ++ -- Handle onBlur events if there
               (blur_msg
                    |> Maybe.map
                        (\msg -> [ onBlur msg ])
                    |> Maybe.withDefault []
               )
        )
        []


toggle : Int -> String -> Bool -> Model -> Msg -> Html Msg
toggle num txt val model msg =
    Toggles.switch Mdl
        [ num ]
        model.mdl
        [ Toggles.ripple
        , Toggles.value val
        , Material.Options.onToggle msg
        , css "margin-top" "10px"
        ]
        [ Html.text txt ]


pcs : PlotLimits -> PlotLimits -> Plot.PlotCustomizations msg
pcs xlim ylim =
    { defaultSeriesPlotCustomizations
        | height = 200
        , margin = { top = 25, right = 25, bottom = 25, left = 50 }
        , toDomainLowest = ylim.min
        , toDomainHighest = ylim.max
        , toRangeLowest = xlim.min
        , toRangeHighest = xlim.max

        --, junk = \summary -> [ Plot.junk ringdownTitle 100 1500 ]
    }
