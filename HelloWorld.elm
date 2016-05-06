module HelloWorld where

import List
import Html exposing (Html, Attribute, node, div, toElement)
import Html.Attributes exposing (style, attribute)
import Signal
import Graphics.Element exposing (show)
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (lazy)
import Result exposing (Result)

{- Type Definitions-}

type alias Dimensions = (Int, Int)
type alias Position = (Int, Int)

type  JsonTree = JsonTree JsonRecord
type alias JsonRecord = 
  { dimensions : Dimensions
  , position : Position
  , children : List JsonTree
  }

type ModelTree = ModelTree ModelRecord
type alias ModelRecord =
  { dimensions : Dimensions
  , position : Position
  , children : List ModelTree
  , nodeId : Int
  , pathFromRoot : List Int
  }


{- Ports -}

port jsonMessage : Signal (String)


{- General Utils -}

listRangeRev : Int -> List Int
listRangeRev x =
  if x == 0
  then []
  else x :: listRange (x - 1)

listRange : Int -> List Int
listRange = List.reverse << listRangeRev


{- Json Decoding -}

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


{- Intermediate model represntation -}

modelFromJson pathFromRoot nodeId (JsonTree jsonTree) =
  let
    nextPathFromRoot = nodeId :: pathFromRoot
    childrenNodeIds = listRange (List.length jsonTree.children)
    children =
      List.map2
        (modelFromJson nextPathFromRoot) 
        childrenNodeIds 
        jsonTree.children
  in
    ModelTree
      { dimensions = jsonTree.dimensions
      , position = jsonTree.position
      , nodeId = nodeId
      , pathFromRoot = pathFromRoot
      , children = children
      }


{- HTML building functions -}

px : Int -> String
px x =
    (toString x) ++ "px"

makeStyle : ModelRecord -> Attribute
makeStyle modelRecord =
  let
    x = fst modelRecord.position
    y = snd modelRecord.position
    w = fst modelRecord.dimensions
    h = snd modelRecord.dimensions
  in
    style
      [ ("position", "absolute")
      , ("left", px x)
      , ("top", px y)
      , ("height", px h)
      , ("width", px w)
      , ("outline", "1px solid black")
      , ("nodeId", toString modelRecord.nodeId)
      , ("pathFromRoot", toString modelRecord.pathFromRoot)
      ]


makeAttributes : ModelRecord -> List Attribute
makeAttributes modelRecord =
 [ attribute "nodeId" <| toString modelRecord.nodeId
 , attribute "pathFromRoot" <| toString modelRecord.pathFromRoot
 , makeStyle modelRecord
 ]

treeToHtml : ModelTree -> Html
treeToHtml (ModelTree modelRecord) =
  div (makeAttributes modelRecord) (List.map treeToHtml modelRecord.children)


{- View -}

main = Signal.map (toElement 500 500 << treeToHtml << modelFromJson [] 0 << decodeJson) jsonMessage
