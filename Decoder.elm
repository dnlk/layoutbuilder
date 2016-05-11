module Decoder (decodeJson) where 

import Json.Decode exposing (..)
import Json.Decode.Extra exposing (lazy)

import Types exposing (..)


pointDecoder : Decoder (Int, Int)
pointDecoder = tuple2 (,) int int

decoder =
  map JsonTree <|
    object3
      JsonRecord
      ("dimensions" := pointDecoder)
      ("position" := pointDecoder)
      ("children" := list (lazy (\_ -> decoder)))

unpackDecodedJson : Result String JsonTree -> JsonTree
unpackDecodedJson result =
  case result of
    Err _ -> JsonTree {dimensions=(0,0), position=(0,0), children=[]}
    Ok jsonTree -> jsonTree

decodeJson : String -> JsonTree
decodeJson jsonStr =
  unpackDecodedJson <| decodeString decoder jsonStr