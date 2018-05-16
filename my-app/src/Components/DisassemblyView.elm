module Components.DisassemblyView exposing (Model, Msg, init, update, view, addInstruction)

import Html exposing (Html, div, span, label, textarea, text, program, input, button, ol, li)
import Html.Attributes exposing (id, class, classList, type_, checked, value, readonly, title)
import Html.Events exposing (onInput, onCheck, onClick)
import Html.Keyed
import Dict exposing (Dict)

-- MODEL

type alias Address = String

type alias Model =
  { instructions : Dict Address Instruction
  , rip : Address
  , startAddress : Address
  , endAddress : Address
  }

type alias Instruction =
  { address : Address
  , offset : String
  , code : String
  , breakpointSet : Bool
  }

init : (Model, Cmd Msg)
init = ({instructions = Dict.fromList test2, rip = "", startAddress = "", endAddress = ""}, Cmd.none)

test2 : List (Address, Instruction)
test2 = List.map (\inst -> (inst.address, inst)) testInit

testInit : List Instruction
testInit =
  [ { address = "0xAABBCCDD10203040"
    , offset = "0"
    , code = "ADD RAX RBX"
    , breakpointSet = False
    }
  , { address = "0xAABBCCDD10203044"
    , offset = "4"
    , code = "ADD RAX RBX"
    , breakpointSet = False
    }
  , { address = "0xAABBCCDD10203048"
    , offset = "8"
    , code = "ADD RAX RBX"
    , breakpointSet = False
    }
  , { address = "0xAABBCCDD1020304C"
    , offset = "12"
    , code = "ADD RAX RBX"
    , breakpointSet = False
    }
  ]

-- MESSAGES

type Msg =
    AddInstruction Instruction
  | StartAddressChanged Address
  | EndAddressChanged Address
  | BreakpointChanged Address Bool
  | Disassemble
  | StartExecution
  | SingleStep
  | ContinueExecution
  | StepOver
  | NoOp

-- INTERFACE

addInstruction : Instruction -> Msg -- -> Model -> Model
addInstruction = AddInstruction -- model = {model | instructions = Dict.insert inst.address inst model.instructions}

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddInstruction inst ->
      (
        {model | instructions = Dict.insert inst.address inst model.instructions}
      , Cmd.none
      )

    StartAddressChanged newAddress -> ({model | startAddress = newAddress}, Cmd.none)

    EndAddressChanged newAddress -> ({model | endAddress = newAddress}, Cmd.none)

    BreakpointChanged address isSet ->
      ( { model
        | instructions = (Dict.update address (handleBreakpointChange isSet) model.instructions)
        }
      , Cmd.none)

    Disassemble -> (model, Cmd.none)

    StartExecution -> (model, Cmd.none)

    SingleStep -> (model, Cmd.none)

    ContinueExecution -> (model, Cmd.none)

    StepOver -> (model, Cmd.none)

    NoOp -> (model, Cmd.none)

handleBreakpointChange : Bool -> Maybe Instruction -> Maybe Instruction
handleBreakpointChange isSet maybeInstruction =
  case maybeInstruction of
    Just instruction -> Just <| {instruction | breakpointSet = isSet}
    Nothing -> Nothing

-- VIEW

view : Model -> Html Msg
view model =
  div [class "disassembly-view"]
  [ div [class "disassembly-view-controls"]
    [ span []
      [ label []
        [ text "START:"
        , input [type_ "text", value model.startAddress, onInput StartAddressChanged] []
        ]
      ]
    , span []
      [ label []
        [ text "END:"
        , input [type_ "text", value model.endAddress, onInput EndAddressChanged] []
        ]
      ]
    , span [] [button [onClick Disassemble] [text "DISASSEMBLE"]]
    , span [] [button [onClick StartExecution][text "START"]]
    , span [] [button [onClick SingleStep, title "SINGLE STEP"] [Html.i [class "fas fa-chevron-circle-right"] []]]
    , span [] [button [onClick ContinueExecution, title "CONTINUE"] [Html.i [class "fas fa-angle-double-right"] []]]
    , span [] [button [onClick StepOver, title "STEP OVER"] [Html.i [class "fas fa-share"] []]]
    ]
  , div [class "disassembly-view-header"]
    [ span [class "disassembly-view-header-break"] [text "BREAK"]
    , span [class "disassembly-view-header-address"] [text "ADDRESS"]
    , span [class "disassembly-view-header-offset"] [text "OFFSET"]
    , span [class "disassembly-view-header-instruction"] [text "INSTRUCTION"]
    ]
  , Html.Keyed.ol [class "disassembly-view-instructions"] <| List.map (listElement model.rip) <| Dict.toList model.instructions
  ]

listElement : Address -> (Address, Instruction) -> (String, Html Msg)
listElement rip (address, instruction) =
  ( address
  , li [classList [("rip-instruction", rip == address)]]
    [ span [class "disassembly-view-instructions-break"]
      [ input
        [ type_ "checkbox"
        , onCheck <| BreakpointChanged address
        , checked instruction.breakpointSet
        ] []
      ]
    , span [class "disassembly-view-instructions-address"] [text address]
    , span [class "disassembly-view-instructions-offset"] [text <| "<+" ++ instruction.offset ++ ">"]
    , span [class "disassembly-view-instructions-instruction"] [text instruction.code]
    ]
  )