module CustomEventAttributes exposing (..)

import Html.Events exposing (onClick, onWithOptions)
import Html exposing (Attribute)
import Mouse
import Json.Decode as Decode exposing (Decoder)

import Types exposing (..)

onClickNoProp : Decoder msg -> Attribute msg
onClickNoProp decoder =
  onWithOptions 
    "click" 
    { stopPropagation = True, preventDefault = False } 
    decoder


clickableAttribute : List Int -> TreeCmd -> Attribute Msg
clickableAttribute pathFromRoot cmd =
  onClickNoProp << Decode.succeed <| ClickEl cmd (Just pathFromRoot)

clickableAttributeCorner : Corner -> Maybe PathFromRoot -> Position -> Dimensions -> Attribute Msg
clickableAttributeCorner corner pathFromRoot initialPos initialDims =
  onClickNoProp 
    (Decode.map (\p -> ControllerBoxClick corner p pathFromRoot initialPos initialDims) Mouse.position)