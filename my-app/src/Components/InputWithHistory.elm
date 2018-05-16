module Components.InputWithHistory exposing (Model, Msg, init, update, view, enterPressed)

import Html exposing (Html, Attribute, input)
import Html.Events exposing (on, onInput)
import Html.Attributes exposing (value, type_)
import Json.Decode as Decode
import List.Zipper exposing (Zipper, singleton, fromList, toList, next, previous)

-- MODEL

type alias Model = {current : String, history : Zipper String}

init : (Model, Cmd Msg)
init = ({ current = "", history = singleton ""}
        , Cmd.none)

-- MESSAGE

type Msg = InputChanged String | AddToHistory String | PrevEntry | NextEntry | NoOp

-- INTERFACE

enterPressed : Msg -> Maybe String
enterPressed msg =
  case msg of
    AddToHistory str -> Just str
    otherwise        -> Nothing

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    InputChanged str -> ({model | current = str}, Cmd.none)

    AddToHistory str -> if String.length str > 0 then (addToHistory str model, Cmd.none) else (model, Cmd.none)

    PrevEntry -> (previousEntry model , Cmd.none)

    NextEntry -> (nextEntry model, Cmd.none)

    NoOp -> (model, Cmd.none)

insertIntoHistory : String -> List String -> List String
insertIntoHistory str history =
  case history of
    []      -> [ str ]
    x :: xs -> x :: str :: xs

addToHistory : String -> Model -> Model
addToHistory str model =
  let newHistory = Maybe.withDefault model.history <| fromList <| insertIntoHistory str <| toList model.history
   in { model
      | current = List.Zipper.current newHistory
      , history = newHistory
      }

previousEntry : Model -> Model
previousEntry model =
  let newHistory = Maybe.withDefault model.history <| next model.history
   in { model
      | current = List.Zipper.current newHistory
      , history = newHistory
      }

nextEntry : Model -> Model
nextEntry model =
  let newHistory = Maybe.withDefault model.history <| previous model.history
   in { model
      | current = List.Zipper.current newHistory
      , history = newHistory
      }

-- VIEW

view : Model -> Html Msg
view model = input [type_ "text", value model.current, onInput InputChanged, onKeyUp <| keyUpHandler model] []

onKeyUp : (String -> Msg) -> Attribute Msg
onKeyUp handler =
  let attributeString = on "keyup" (Decode.field "key" Decode.string)
   in Html.Attributes.map handler attributeString

keyUpHandler : Model -> String -> Msg
keyUpHandler model key =
  case key of
    "Enter" -> AddToHistory model.current
    "ArrowUp" -> PrevEntry
    "ArrowDown" -> NextEntry
    otherwise -> NoOp