port module HelloWorld exposing (..)

import Debug
import List
import Html exposing (Html, Attribute, node, div, text)
import Html.Attributes exposing (style, attribute)
import Html.Events exposing (onClick, onWithOptions)
import Result exposing (Result)
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
      newSubTreeFunc completeTree

    x :: xs ->
      let 
        recurseIfMatches subTree =
          case (getModelRecord subTree).pathFromRoot of
            [] -> subTree
            y :: ys ->
              if y == x
              then modifyDesc xs subTree newSubTreeFunc
              else subTree
        modelRecord = getModelRecord completeTree
        newChildren = List.map recurseIfMatches modelRecord.children
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
    modifyDesc (List.reverse pathToDesc) completeTree addController

{- Intermediate model represntation -}

modelFromJson pathFromRoot (JsonTree jsonTree) =
  let
    childrenNodeIds = listRange (List.length jsonTree.children)
    children =
      List.map2
        (\ nextId -> modelFromJson (nextId :: pathFromRoot))
        childrenNodeIds 
        jsonTree.children
  in
    ModelTree
      { dimensions = jsonTree.dimensions
      , position = jsonTree.position
      , pathFromRoot = pathFromRoot
      , children = children
      , controllers = []
      }

updateModelTree : Msg -> ModelTree -> (ModelTree, Cmd Msg)
updateModelTree msg completeModelTree =
  Debug.log ("msg = " ++ toString msg)
  (case msg of
    Init jsonStr -> (modelFromJson [] << decodeJson <| jsonStr, Cmd.none)

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
 [ attribute "pathFromRoot" <| toString modelRecord.pathFromRoot
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
