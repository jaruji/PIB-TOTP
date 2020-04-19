module Main exposing (..)
import Json.Decode as Decode exposing (list, string)
import Json.Encode as Encode
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Crypto.Hash as Crypto
import QRCode

type alias Model =
  {
    name: String
    , password: String
    , code: String
    , mode: Mode
    , secret: Secret
    , state: State
    , warning: String
  }

server: String
server =
    "http://localhost:3000"

init: () -> (Model, Cmd Msg)
init model =
    (Model "" "" "" None NotReceived NoLogin "", Cmd.none)

type Msg
  = Name String
  | Password String
  | Code String
  | SwitchMode Mode
  | RegistrationResponse (Result Http.Error String)
  | LoginResponse (Result Http.Error())
  | OTPResponse (Result Http.Error())
  | LogIn
  | Register
  | OTP

type State
  = NoLogin
  | AwaitOTP String
  | Login String

type RegistrationStatus
  = Loading
  | Success String
  | Failure String

type Mode
  = None
  | SignIn
  | SignUp

type Secret
  = NotReceived
  | Received String

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Name name ->
            ({ model | name = name }, Cmd.none)
        Password password ->
            ({ model | password = password }, Cmd.none)
        Code code ->
            ({ model | code = code }, Cmd.none)
        SwitchMode mode ->
            ({ model | mode = mode, name = "", password = "", code = "", secret = NotReceived, warning = "", state = NoLogin }, Cmd.none)
        RegistrationResponse response ->
            case response of
                Ok string ->
                    ({ model | secret = Received string, warning = "" }, Cmd.none)
                Err _ ->
                    ({ model | warning = "Failure" }, Cmd.none)
        LoginResponse response ->
            case response of
                Ok _ ->
                    ({ model | state = AwaitOTP model.name, warning = "" }, Cmd.none)
                Err _ ->
                    ({ model | warning = "Login failed" }, Cmd.none)
        OTPResponse response ->
            case response of
                Ok _ ->
                    ({ model | state = Login model.name, warning = ""}, Cmd.none)
                Err _ ->
                    ({ model | warning = "Verification failed" }, Cmd.none)
        LogIn ->
            if model.name /= "" && model.password /= "" then
                (model, login model.name model.password)
            else 
                (model, Cmd.none)
        Register ->
            if model.name /= "" && model.password /= "" && String.length model.password > 5 then
                (model, register model.name model.password)
             else 
                (model, Cmd.none)
        OTP ->
            if model.code /= "" then
                (model, otp model.name model.code)
            else
                (model, Cmd.none)

view: Model -> Html Msg
view model =
    div[ style "text-align" "center" ][
        nav[ class "navbar navbar-expand-sm bg-success navbar-dark"][
            button[ class "btn btn-link nav-link navbar-brand", onClick (SwitchMode None) ][ text "OTP App" ]
            , ul[ class "navbar-nav ml-auto"][
                li[ class "nav-item" ][
                    button[ class "btn btn-link nav-link", onClick (SwitchMode SignUp) ][ text "Sign Up"]
                ]
                , li[ class "nav-item" ][
                    button[ class "btn btn-link nav-link", onClick (SwitchMode SignIn) ][ text "Sign In"]
                ]
            ]
        ]
        , case model.state of
            NoLogin ->
                case model.mode of 
                    SignUp ->
                        div[ style "margin-top" "30px" ][
                            h1[][ text "Register your account" ]
                            , div[ class "text-muted" ][
                                text "Password must be at least 6 characters long"
                            ]
                            , div[][ 
                                label[ for "name" ][ text "Username: " ]
                                , input[ class "form-control"
                                , style "width" "50%"
                                , style "margin" "auto"
                                , id "name"
                                , Html.Attributes.value model.name
                                , onInput Name
                                , type_ "text" ][]
                            ]
                            , div[][ 
                                label[ for "pass" ][ text "Password: " ] 
                                , input[ class "form-control"
                                , style "width" "50%"
                                , style "margin" "auto"
                                , id "pass"
                                , Html.Attributes.value model.password
                                , onInput Password
                                , type_ "password" ][] 
                            ]
                            , div[][
                                button[ class "btn btn-success"
                                , style "margin-top" "30px"
                                , case model.secret of
                                    Received _ ->
                                        disabled True
                                    _ ->
                                        class ""
                                , onClick Register ][ text "Register" ]
                            ]
                            , warning model.warning
                            , case model.secret of
                                Received secret ->
                                    div[][
                                        h3[ style "margin-top" "20px" ][
                                            text "Scan your QR Code through the Google Authenticator app"
                                            , br[][]
                                            , text "Store the secret key carefully! Do not leave this page without scanning the code"
                                        ]
                                        , qrCodeView secret
                                        , div [ class "text-muted" ][ text "Press confirm after scanning the code" ]
                                        , div [ style "margin-top" "20px" ][
                                            button [ class "btn btn-success", onClick (SwitchMode SignIn) ][ text "Confirm" ]
                                        ]
                                    ]
                                _ ->
                                    text ""
                        ]
                    SignIn ->
                        div[ style "margin-top" "30px" ][
                            h1[][ text "Log In" ]
                            , div[][ 
                                label[ for "name" ][ text "Username: " ]
                                , input[ class "form-control"
                                , style "width" "50%"
                                , style "margin" "auto"
                                , id "name"
                                , Html.Attributes.value model.name
                                , onInput Name
                                , type_ "text" ][]
                            ]
                            , div[][ 
                                label[ for "pass" ][ text "Password: " ] 
                                , input[ class "form-control"
                                , style "width" "50%"
                                , style "margin" "auto"
                                , id "pass"
                                , Html.Attributes.value model.password
                                , onInput Password
                                , type_ "password" ][] 
                            ]
                            , div[][
                                button[ class "btn btn-success"
                                , style "margin-top" "30px"
                                , onClick LogIn ][ text "Log In" ]
                            ]
                            , warning model.warning
                        ]
                    None ->
                        div[ style "margin-top" "30px" ][
                            h1[][ text "Welcome to OTP testing app" ]
                            , div [ class "text-muted" ][
                                text "This application was created to test TOTP implementation"
                            ]
                            , h2[][ text "Usage" ]
                            , div [ class "text-muted" ][
                                text "Make sure you have MongoDB database installed and run the server through node.js"
                                , br[][]
                                , text "Make sure you have all the npm packages installed (npm install)"
                                , br[][]
                                , text "Use the command \"node server.js\" to start the server on port 3000"
                                , br[][]
                                , text "Sign up and scan the secret QRcode to Google Authenticator application"
                                , br[][]
                                , text "Next, try signing in using all your creditentials!"
                            ]
                        ]
            AwaitOTP string ->
                div[ style "margin-top" "20px" ][
                    h3[][ text ("Almost done, " ++ string ++ "!")]
                    , div[ class "text-muted" ][ text "Enter your OTP to complete your login" ]
                    , div[][
                        label[ for "code" ][ text "OTP: " ] 
                        , input[ class "form-control"
                        , style "width" "50%"
                        , style "margin" "auto"
                        , id "code"
                        , Html.Attributes.value model.code
                        , onInput Code
                        , type_ "text" ][] 
                    ]
                    , button [ class "btn btn-success"
                    , style "margin-top" "20px"
                    , onClick OTP ][ text "Submit" ]
                    , warning model.warning
                ]
            Login string ->
                div[ style "margin-top" "30px" ][
                    h1[][text ("Welcome, " ++ string)]
                    , div [ class "text-muted" ][
                        text "Congratulations, your authentication was successfull"
                    ]
                    , img [ src "assets/congrats.jpg"][]
                ]
    ]

qrCodeView : String -> Html msg
qrCodeView message =
    QRCode.encode message
        |> Result.map QRCode.toSvg
        |> Result.withDefault
            (Html.text "Error while encoding to QRCode.")

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.none

warning: String -> Html msg
warning txt =
    case txt of
        "" ->
            text ""
        _ ->
            div[ class "alert alert-warning" 
            , style "width" "30%"
            , style "margin" "auto"
            , style "margin-top" "20px" ][ 
                text txt
            ]        

encodeAccount: String -> String -> Encode.Value
encodeAccount username password =
    Encode.object[
        ("username", Encode.string username)
        , ("password", Encode.string (Crypto.sha256 password))
    ]

encodeOtp: String -> String -> Encode.Value
encodeOtp username code =
    Encode.object[
        ("username", Encode.string username)
        , case String.toInt code of
            Just int ->
                ("otp", Encode.int int)
            _ ->
                ("otp", Encode.string code)
    ]

register: String -> String -> Cmd Msg
register username password =
    Http.post{
        url = server ++ "/sign_up"
        , body = Http.jsonBody <| (encodeAccount username password)
        , expect = Http.expectJson RegistrationResponse (Decode.field "secret" Decode.string)
    }

login: String -> String -> Cmd Msg
login username password =
    Http.post{
        url = server ++ "/sign_in"
        , body = Http.jsonBody <| (encodeAccount username password)
        , expect = Http.expectWhatever LoginResponse
    }

otp: String -> String -> Cmd Msg
otp username code =
    Http.post{
        url = server ++ "/validate"
        , body = Http.jsonBody <| (encodeOtp username code)
        , expect = Http.expectWhatever OTPResponse
    }

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }