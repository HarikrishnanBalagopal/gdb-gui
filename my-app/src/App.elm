module App exposing (main)

import Html exposing (Html, program, div, button, text)
import Html.Events exposing (onClick)
import Components.ConsoleView as ConsoleView
import Components.DisassemblyView as DisassemblyView
import Components.RegisterView as RegisterView
import WebSocket
import Json.Encode as Encode
import Json.Decode as Decode
import Maybe exposing (withDefault)
import Utils

-- LITERALS

socketServer : String
socketServer = "ws://echo.websocket.org"

debugServer : String
debugServer = "ws://localhost:8080"

-- MODEL

type alias Model =
  { consoleViewModel : ConsoleView.Model
  , disassemblyViewModel: DisassemblyView.Model
  , registerViewModel : RegisterView.Model
  }

init : (Model, Cmd Msg)
init =
  let (initConsoleViewModel, initConsoleViewCmd) = ConsoleView.init
      (initDisassemblyViewModel, initDisassemblyViewCmd) = DisassemblyView.init
      (initRegisterViewModel, initRegisterViewCmd) = RegisterView.init
   in (
        { consoleViewModel = initConsoleViewModel
        , disassemblyViewModel = initDisassemblyViewModel
        , registerViewModel = initRegisterViewModel
        }
      , Cmd.batch
        [ Cmd.map ConsoleViewMsg initConsoleViewCmd
        , Cmd.map DisassemblyViewMsg initDisassemblyViewCmd
        , Cmd.map RegisterViewMsg initRegisterViewCmd
        ]
      )

-- MESSAGES

type Msg =
    ConsoleViewMsg ConsoleView.Msg
  | DisassemblyViewMsg DisassemblyView.Msg
  | RegisterViewMsg RegisterView.Msg
--  | Echo String

-- COMMANDS

sendGdbCmd : String -> Cmd Msg
sendGdbCmd str = WebSocket.send debugServer <| Encode.encode 0 <| Encode.object [("gdbCmd", Encode.string str)]

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ConsoleViewMsg consoleMsg ->
      let newUserInput = ConsoleView.consoleCommand consoleMsg
          newDebugCmd = withDefault Cmd.none <| Maybe.map sendGdbCmd newUserInput

          (newModel, newCmd) = ConsoleView.update consoleMsg model.consoleViewModel
       in ({model | consoleViewModel = newModel}, Cmd.batch [newDebugCmd, Cmd.map ConsoleViewMsg newCmd])

    DisassemblyViewMsg disassemblyMsg ->
      let (newModel, newCmd) = DisassemblyView.update disassemblyMsg model.disassemblyViewModel
       in ({model | disassemblyViewModel = newModel}, Cmd.map DisassemblyViewMsg newCmd)

    RegisterViewMsg registerMsg ->
      case registerMsg of
        RegisterView.InterfaceMsg interfaceMsg ->
          case interfaceMsg of
            RegisterView.RefreshRegisters -> (model, sendGdbCmd "-data-list-register-values x\n")
            RegisterView.LoadDisassembly address ->
              let startAddress = Utils.addressToString address
                  endAddress = Utils.addressToString <| Utils.addressAdd32Int 256 address
               in ( model
                  , sendGdbCmd <| "-data-disassemble -s 0x" ++ startAddress ++ " -e 0x" ++ endAddress ++ " -- 0\n"
                  )
            otherwise -> (model, Cmd.none)

        otherwise ->
          let (newModel, newCmd) = RegisterView.update registerMsg model.registerViewModel
           in ({model | registerViewModel = newModel}, Cmd.map RegisterViewMsg newCmd)

{-    Echo str ->
      let inst =
            { address = (++) "0x" <| String.padLeft 16 '0' <| String.toUpper <| Hex.toString model.newAddress
            , offset = "OFF"
            , code = str
            , breakpointSet = False
            }
          newModel =
            { model
            | disassemblyViewModel = DisassemblyView.addInstruction inst model.disassemblyViewModel
            , newAddress = model.newAddress + 1
            }
       in (newModel, Cmd.none)
-}

-- VIEW

view : Model -> Html Msg
view model =
  div []
  [ Html.map DisassemblyViewMsg <| DisassemblyView.view model.disassemblyViewModel
  , Html.map ConsoleViewMsg <| ConsoleView.view model.consoleViewModel
  , button [onClick <| ConsoleViewMsg <| ConsoleView.addResponse "DUDE HELLL YEA\nHH\nHHH\nHHHHHHHH!!"] [text "DUDE!!"]
  , Html.map RegisterViewMsg <| RegisterView.view model.registerViewModel
  ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let jsonStringToResult = Decode.decodeString (Decode.field "gdbRes" Decode.string)
      resultToString res =
        case res of
          Ok str -> str
          Err err -> err
      jsonStringToMsg = jsonStringToResult >> resultToString >> ConsoleView.addResponse >> ConsoleViewMsg
   in WebSocket.listen debugServer jsonStringToMsg

-- MAIN

main : Program Never Model Msg
main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }