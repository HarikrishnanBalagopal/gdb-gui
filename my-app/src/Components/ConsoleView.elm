module Components.ConsoleView exposing (Model, Msg, init, update, view, addResponse, consoleCommand)

import Html exposing (Html, div, span, label, textarea, text, input, button)
import Html.Attributes exposing (id, class, type_, checked, value, readonly)
import Html.Events exposing (onCheck, onClick)
import Components.InputWithHistory as CustomInput
import Dom.Scroll exposing (toBottom)
import Task exposing (attempt)
import Maybe exposing (andThen)

-- LITERALS

delimiter : String
delimiter = "\n---------------------------------------------------------------------------------------------\n"

-- MODEL

type alias Model = 
  { customInputModel : CustomInput.Model
  , consoleOutput : String
  , autoScroll : Bool
  , delimiter : Bool
  }

init : (Model, Cmd Msg)
init =
  let (initCustomInputModel, initCustomInputCmd) = CustomInput.init
   in (
        { customInputModel = initCustomInputModel
        , consoleOutput = "TESTING CONSOLE"
        , autoScroll = True
        , delimiter = False
        }
      , Cmd.map CustomInputMsg initCustomInputCmd
      )

-- MESSAGE

type Msg =
    InterfaceMsg Interface
  | CustomInputMsg CustomInput.Msg
  | ClearConsole
  | TextAreaAppend String
  | AutoScrollChange Bool
  | DelimiterChange Bool
  | NoOp

-- INTERFACE

type Interface =
    TextAreaAppend String
  | ConsoleCommand String

addResponse : String -> Msg
addResponse = TextAreaAppend

consoleCommandHelper : Msg -> Maybe CustomInput.Msg
consoleCommandHelper msg =
  case msg of
    CustomInputMsg inputMsg -> Just inputMsg
    otherwise               -> Nothing

consoleCommand : Msg -> Maybe String
consoleCommand = consoleCommandHelper >> andThen CustomInput.enterPressed

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    CustomInputMsg inputMsg ->
      let (newModel, newCmd) = CustomInput.update inputMsg model.customInputModel
       in ({model | customInputModel = newModel}, Cmd.map CustomInputMsg newCmd)

    ClearConsole -> ({model | consoleOutput = ""}, Cmd.none)

    TextAreaAppend str ->
      let newCmd = if model.autoScroll then attempt (\_ -> NoOp) <| toBottom "console-view-textarea" else Cmd.none
          newStr = if model.delimiter then delimiter ++ str else str
       in ({model | consoleOutput = model.consoleOutput ++ newStr}, newCmd)

    AutoScrollChange b -> ({model | autoScroll = b}, Cmd.none)

    DelimiterChange b -> ({model | delimiter = b}, Cmd.none)

    NoOp -> (model, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div [class "console-view"]
  [ div [class "console-view-controls"]
    [ span [] [button [onClick ClearConsole] [text "CLEAR"]]
    , span []
      [ label []
        [ text "AUTOSCROLL:"
        , input
          [ type_ "checkbox"
          , onCheck AutoScrollChange
          , checked model.autoScroll
          ] []
        ]
      ]
    , span []
      [ label []
        [ text "DELIMITER:"
        , input
          [ type_ "checkbox"
          , onCheck DelimiterChange
          , checked model.delimiter
          ] []
        ]
      ]
    ]
  , Html.map CustomInputMsg <| CustomInput.view model.customInputModel
  , textarea [id "console-view-textarea", value model.consoleOutput, readonly True] []
  ]