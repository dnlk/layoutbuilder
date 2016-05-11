module HelloWorld where

import Debug
import List
import Html exposing (Html, Attribute, node, div, toElement, text)
import Html.Attributes exposing (style, attribute)
import Html.Events exposing (onClick, onWithOptions)
import Signal
import Graphics.Element exposing (show, above)
import Result exposing (Result)
import Signal.Extra exposing (foldp')
import Json.Decode exposing (value)


import Decoder exposing (decodeJson)
import Types exposing (..)


{- Ports -}

port jsonMessage : Signal (String)

{- Signals -}

clickedElement : Signal.Mailbox (Maybe (List Int))
clickedElement = Signal.mailbox Nothing

triggerSig : Signal Trigger
triggerSig = 
  Signal.merge
    (Signal.map Init jsonMessage)
    (Signal.map ClickEl clickedElement.signal)


onClickNoProp : Signal.Address a -> a -> Attribute   
onClickNoProp address message =
    onWithOptions "click" { stopPropagation = True, preventDefault = False } value (\_ -> Signal.message address message)


{- General Utils -}

listRangeRev : Int -> List Int
listRangeRev x =
  if x == 0
  then []
  else x :: listRange (x - 1)

listRange : Int -> List Int
listRange = List.reverse << listRangeRev


clickableAttribute : List Int -> Attribute
clickableAttribute pathFromRoot =
  onClickNoProp clickedElement.address (Just pathFromRoot)


modifyDesc : List Int -> ModelTree -> (ModelTree -> ModelTree) -> ModelTree
modifyDesc pathToDesc completeTree newSubTreeFunc =

  case pathToDesc of
    [] ->
      EmptyModel

    x :: [] ->
      if x == (getModelRecord completeTree).nodeId
      then newSubTreeFunc completeTree
      else completeTree

    x :: xs ->
      let
        modelRecord = getModelRecord completeTree
        newChildren = 
          if x == modelRecord.nodeId
          then List.map (\mTree -> modifyDesc xs mTree newSubTreeFunc) modelRecord.children
          else (getModelRecord completeTree).children
      in
        ModelTree { modelRecord | children = newChildren }


addController : ModelTree -> ModelTree
addController mTree =
  case mTree of
    (ModelTree modelRecord) ->
      ModelTree { modelRecord | controllers = [{}] }
    EmptyModel -> mTree

addControllerToTree : List Int -> ModelTree -> ModelTree
addControllerToTree pathToDesc completeTree =
    modifyDesc pathToDesc completeTree addController

{- Intermediate model represntation -}

modelFromJson pathFromRoot nodeId (JsonTree jsonTree) =
  let
    nextPathFromRoot = pathFromRoot ++ [nodeId]
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
      , pathFromRoot = nextPathFromRoot
      , children = children
      , controllers = []
      }

updateModelTree : Trigger -> ModelTree -> ModelTree
updateModelTree trigger completeModelTree =
  case trigger of
    Init jsonStr -> modelFromJson [] 0 << decodeJson <| jsonStr

    ClickEl Nothing -> completeModelTree

    ClickEl (Just path) -> addControllerToTree path completeModelTree


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

makeControllerStyle : Controller -> Attribute
makeControllerStyle controller =
  style
    [ ("position", "absolute") 
    , ("width", "100%")
    , ("height", "100%")
    , ("border", "2px solid orange")
    ]

makeControllerAttributes : Controller -> List Attribute
makeControllerAttributes controller =
  [ makeControllerStyle controller ]


makeAttributes : ModelRecord -> List Attribute
makeAttributes modelRecord =
 [ attribute "nodeId" <| toString modelRecord.nodeId
 , attribute "pathFromRoot" <| toString modelRecord.pathFromRoot
 , makeStyle modelRecord
 , clickableAttribute (modelRecord.pathFromRoot)
 ]

controllerToHtml : Controller -> Html
controllerToHtml controller =
  div (makeControllerAttributes controller) []

treeToHtml : ModelTree -> Html
treeToHtml modelTree =
  case modelTree of
    EmptyModel -> text "empty model :("

    ModelTree modelRecord -> 
      let
        controllers = List.map controllerToHtml modelRecord.controllers
        children = List.map treeToHtml modelRecord.children
      in
        controllers ++ children
        |> div (makeAttributes modelRecord)



{- View -}

initModel trigger = updateModelTree trigger EmptyModel

modelSig = foldp' updateModelTree initModel triggerSig

viewSig = Signal.map (toElement 500 500 << treeToHtml) modelSig

main = 
  Signal.map2
    above
    (Signal.map2
      above
      (Signal.map showModel modelSig)
      (Signal.map displaySig triggerSig))
    viewSig


{- Some debugging stuff -}

displaySig val =
  case val of
    ClickEl el -> show el
    Init json -> show json


showModel model =
  case model of
    EmptyModel -> show "empty"
    ModelTree mRecord -> show mRecord
