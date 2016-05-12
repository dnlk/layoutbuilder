port module HelloWorld exposing (..)

import Debug
import List
import Html exposing (Html, Attribute, node, div, text)
import Html.Attributes exposing (style, attribute)
import Html.Events exposing (onClick, onWithOptions)
--import Signal
--import Graphics.Element exposing (show, above)
import Result exposing (Result)
--import Signal.Extra exposing (foldp')
import Json.Decode exposing (succeed)
import Html.App
import Platform.Cmd as Cmd exposing (Cmd)
import Platform.Sub as Sub exposing (Sub)


import Decoder exposing (decodeJson)
import Types exposing (..)


{- Ports -}

port jsonMessage : (String -> msg) -> Sub msg

subscriptions model =
  jsonMessage Init


{- Signals -}

--clickedElement : Signal.Mailbox (Maybe (List Int))
--clickedElement = Signal.mailbox Nothing

--triggerSig : Signal Trigger
--triggerSig = 
--  Signal.merge
--    (Signal.map Init jsonMessage)
--    (Signal.map ClickEl clickedElement.signal)


onClickNoProp : msg -> Attribute msg
onClickNoProp message =
    onWithOptions "click" { stopPropagation = True, preventDefault = False } (succeed message)


{- General Utils -}

listRangeRev : Int -> List Int
listRangeRev x =
  if x == 0
  then []
  else x :: listRange (x - 1)

listRange : Int -> List Int
listRange = List.reverse << listRangeRev


clickableAttribute : List Int -> Attribute Msg
clickableAttribute pathFromRoot =
  onClickNoProp <| ClickEl (Just pathFromRoot)


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

updateModelTree : Msg -> ModelTree -> (ModelTree, Cmd Msg)
updateModelTree msg completeModelTree =
  Debug.log ("msg = " ++ toString msg)
  (case msg of
    Init jsonStr -> (modelFromJson [] 0 << decodeJson <| jsonStr, Cmd.none)

    ClickEl Nothing -> (completeModelTree, Cmd.none)

    ClickEl (Just path) -> (addControllerToTree path completeModelTree, Cmd.none))


{- HTML building functions -}

px : Int -> String
px x =
    (toString x) ++ "px"

makeStyle : ModelRecord -> Attribute Msg
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

makeControllerStyle : Controller -> Attribute Msg
makeControllerStyle controller =
  style
    [ ("position", "absolute") 
    , ("width", "100%")
    , ("height", "100%")
    , ("border", "2px solid orange")
    ]

makeControllerAttributes : Controller -> List (Attribute Msg)
makeControllerAttributes controller =
  [ makeControllerStyle controller ]


makeAttributes : ModelRecord -> List (Attribute Msg)
makeAttributes modelRecord =
 [ attribute "nodeId" <| toString modelRecord.nodeId
 , attribute "pathFromRoot" <| toString modelRecord.pathFromRoot
 , makeStyle modelRecord
 , clickableAttribute (modelRecord.pathFromRoot)
 ]

controllerToHtml : Controller -> Html Msg
controllerToHtml controller =
  div (makeControllerAttributes controller) []

treeToHtml : ModelTree -> Html Msg
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

--initModel trigger = updateModelTree trigger EmptyModel

--modelSig = foldp' updateModelTree initModel triggerSig

--viewSig = Signal.map (toElement 500 500 << treeToHtml) modelSig

init : {jsonStr : String} -> (ModelTree, Cmd Msg)
init {jsonStr} =
  updateModelTree (Init jsonStr) EmptyModel



program = 
  Html.App.programWithFlags
    { init = init
    , update = updateModelTree
    , subscriptions = subscriptions
    , view = treeToHtml
    }


main = program

--main = 
--  Signal.map2
--    above
--    (Signal.map2
--      above
--      (Signal.map showModel modelSig)
--      (Signal.map displaySig triggerSig))
--    viewSig


{- Some debugging stuff -}

--displaySig val =
--  case val of
--    ClickEl el -> show el
--    Init json -> show json


--showModel model =
--  case model of
--    EmptyModel -> show "empty"
--    ModelTree mRecord -> show mRecord
