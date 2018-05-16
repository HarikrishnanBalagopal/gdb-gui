module Components.RegisterView exposing (Model, Msg(InterfaceMsg), Interface(..), init, update, view)

import Html exposing (Html, div, span, label, textarea, text, program, input, button, ol, li)
import Html.Attributes exposing (id, class, classList, type_, checked, value, readonly, title)
import Html.Events exposing (on, targetValue, onInput, onCheck, onClick)
import Dict exposing (Dict)
import Utils
import Json.Decode as Decode

-- LITERALS

registerNames : List String
registerNames = ["rax","rbx","rcx","rdx","rsi","rdi","rbp","rsp","r8","r9","r10","r11","r12","r13","r14","r15","rip","eflags","cs","ss","ds","es","fs","gs"]

-- MODEL

type alias RegisterKey = Int

type alias Address = Utils.Address

type alias Model =
  { registers : Dict RegisterKey Register
  }

type alias Register =
  { key : RegisterKey
  , name : String
  , value : Address
  , readOnly : Bool
  }

init : (Model, Cmd Msg)
init =
  ( { registers = Dict.fromList registerList
    }
  , Cmd.none
  )

registerList : List (RegisterKey, Register)
registerList = List.indexedMap createRegister registerNames

createRegister : Int -> String -> (RegisterKey, Register)
createRegister key name =
  ( key
  , { key = key
    , name = name
    , value = Utils.defaultAddress
    , readOnly = True 
    }
  )

-- MESSAGES

type Msg =
    InterfaceMsg Interface
  | RegisterChange RegisterKey String
  | ToggleRegister RegisterKey

-- INTERFACE

type Interface =
    RefreshRegisters
  | LoadDisassembly Address
  | NewRegisterData (Dict RegisterKey Register)

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RegisterChange key str ->
      let validAddress = Utils.addressFromString str
       in ( { model
            | registers = Dict.update key (Maybe.map <| \r -> {r | value = validAddress}) model.registers
            }
          , Cmd.none
          )

    ToggleRegister key ->
      ( { model
        | registers = Dict.update key (Maybe.map <| \r -> {r | readOnly = not r.readOnly}) model.registers
        }
      , Cmd.none
      )

    InterfaceMsg upperMsg ->
      case upperMsg of
        NewRegisterData newRegisters -> ({model | registers = newRegisters}, Cmd.none)
        otherwise                    -> (model, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div [class "register-view"]
  [ div [class "register-view-controls"]
    [ span []
      [ button [onClick <| InterfaceMsg RefreshRegisters] [Html.i [class "fas fa-sync-alt"] []]
      ]
    ]
  , div [class "register-view-register-list"] <| Dict.values <| Dict.map registerElement model.registers
  ]

registerElement : RegisterKey -> Register -> Html Msg
registerElement key register =
  div [class "register-view-register-list-register"]
  (
    [ label [Html.Attributes.for <| toString key] [text <| String.toUpper register.name ++ ":"]
    , input
      [ type_ "text"
      , Html.Attributes.id <| toString key
      , on "change" <| Decode.map (\s -> RegisterChange key s) targetValue
      , readonly register.readOnly
      , value <| Utils.addressToString register.value
      ] []
    , button [onClick <| ToggleRegister key] [Html.i [class "fas fa-edit"] []] 
    ]
  ++  if register.name == "rip" || register.name == "eip"
      then [button [onClick <| InterfaceMsg <| LoadDisassembly register.value][text "LOAD DISASM"]]
      else []
  )