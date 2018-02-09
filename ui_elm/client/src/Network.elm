port module Network exposing (..)

{-| Model contains three parameters:

  - `ip` - the IP address
  - `port_` - port used for communication
  - `service` - this is the service name used in constructing the address for
    communication.

-}


type alias Model =
    { ip : String
    , port_ : String
    , service : String
    }


{-| This function will send the current ip and port to the js side for
storage using HTML5 `localstorage`.
-}
port updateIpConfig : List String -> Cmd msg


port initPort : (( String, String ) -> msg) -> Sub msg


type Msg
    = UpdateIP String
    | UpdatePort String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateIP ip ->
            { model | ip = ip }

        UpdatePort p ->
            { model | port_ = p }


init : Model -> Model
init model =
    model


{-| This will build the address required for communication with the server.
The address will look something like

> <http://10.172.240.107:8001/[service-name]/>

**Remember the last slash - you don't have to add this when building addresses
for communication.**

-}
buildAddress : Model -> String
buildAddress model =
    "http://" ++ model.ip ++ ":" ++ model.port_ ++ "/" ++ model.service ++ "/"
