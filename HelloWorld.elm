port module HelloWorld exposing (..)

import Debug

import List
import Html exposing (Html, Attribute, node, div, text)
import Html.Attributes exposing (style, attribute, id)
import Html.App
import Result exposing (Result)
import Platform.Cmd as Cmd exposing (Cmd)
import Platform.Sub as Sub exposing (Sub)
import Mouse
import Keyboard

import Util.List exposing (..)
import Decoder exposing (decodeJson)
import Types exposing (..)
import CustomEventAttributes exposing (..)


{- Ports -}

port jsonMessage : (String -> msg) -> Sub msg

subscriptions model =
  Sub.batch
    [ jsonMessage Init
    , Mouse.moves MouseMove
    , Keyboard.presses KeyPress
    ]


{- Tree Modifying Functions -}
{- There is a lot of code here. There needs to a better way to handle messages
   and events. Once that is figured out, this section will be ready to
   refactor -}

modifyDesc : PathFromRoot -> ModelTree -> (ModelTree -> ModelTree) -> ModelTree
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
addController (ModelTree modelRecord) =
  ModelTree { modelRecord | controllers = [{clickedCorner = Nothing}] }

addControllerToTree : PathFromRoot -> ModelTree -> ModelTree
addControllerToTree pathToDesc completeTree =
  modifyDesc (List.reverse pathToDesc) completeTree addController


deleteController : ModelTree -> ModelTree
deleteController (ModelTree modelRecord) =
  ModelTree { modelRecord | controllers = [] }

deleteControllerFromTree : PathFromRoot -> ModelTree -> ModelTree
deleteControllerFromTree pathToDesc completeTree =
  modifyDesc (List.reverse pathToDesc) completeTree deleteController


getController : List Controller -> Maybe Controller
getController cs =
  case cs of
    [] -> Nothing
    x :: xs -> Just x

toggleControllerResizeClick : Corner -> PathFromRoot -> PositionRec -> Position -> Dimensions -> ModelTree -> ModelTree
toggleControllerResizeClick corner pathToDesc p pos dim mTree =
  let
    toggleClick corner (ModelTree mRecord) =

        case getController mRecord.controllers of
          Nothing -> ModelTree mRecord

          Just controller -> 
            case controller.clickedCorner of
              Nothing -> 
                ModelTree 
                  { mRecord 
                  | controllers = 
                    [{clickedCorner = 
                      Just 
                        { corner = corner
                        , positionClicked = p
                        , initialPos = pos
                        , initialDim = dim
                        }}]}
              Just _ -> ModelTree { mRecord | controllers = [{clickedCorner = Nothing}]}

  in
    modifyDesc (List.reverse pathToDesc) mTree (toggleClick corner)


updateDimensions : ClickedCorner -> PositionRec -> ModelRecord -> ModelRecord
updateDimensions clickedCorner pMouse mRecord =
  let
    corner = clickedCorner.corner
    pClicked = clickedCorner.positionClicked
    dx = pMouse.x - pClicked.x
    dy = pMouse.y - pClicked.y
    posX = fst clickedCorner.initialPos
    posY = snd clickedCorner.initialPos
    dimX = fst clickedCorner.initialDim
    dimY = snd clickedCorner.initialDim

  in
    case corner of
      TopRight -> 
        { mRecord 
        | dimensions = (dimX + dx, dimY - dy)
        , position = (posX, posY + dy) 
        }
      BottomRight -> 
        { mRecord 
        | dimensions = (dimX + dx, dimY + dy)
        , position = (posX, posY) 
        }
      BottomLeft -> 
        { mRecord 
        | dimensions = (dimX - dx, dimY + dy)
        , position = (posX + dx, posY)
        }
      TopLeft -> 
        { mRecord 
        | dimensions = (dimX - dx, dimY - dy)
        , position = (posX + dx, posY + dy)
        }
      TopCenter -> 
        { mRecord 
        | dimensions = (dimX, dimY)
        , position = (posX + dx, posY + dy)
        }

getClickedCorner : ModelRecord -> Maybe ClickedCorner
getClickedCorner mRecord =
  case mRecord.controllers of
    [] -> Nothing
    c :: _ -> c.clickedCorner

resizeClicked : PositionRec -> ModelTree -> ModelTree
resizeClicked pMouse (ModelTree mRecord) =
    case getClickedCorner mRecord of
      Nothing -> 
        ModelTree 
          { mRecord 
          | children = (List.map (resizeClicked pMouse) mRecord.children)}
      Just clickedCorner -> 
        ModelTree <| updateDimensions clickedCorner pMouse mRecord

isUnClicked : ModelTree -> Bool
isUnClicked (ModelTree mRecord) =
    case mRecord.controllers of 
      [] -> True
      _ -> False

deleteClickedNode : ModelTree -> ModelTree
deleteClickedNode (ModelTree mRecord) =
    let
      nonClickedChildren = List.filter isUnClicked mRecord.children
      newChildren = List.map deleteClickedNode nonClickedChildren
    in
      ModelTree { mRecord | children = newChildren}


handleKeyPress : Int -> ModelTree -> ModelTree
handleKeyPress x mTree=
  if x == 100
  then deleteClickedNode mTree
  else mTree


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

{- Update handler -}
{- A better way of handling the messages needs to be figured out. -}

updateModelTree : Msg -> ModelTree -> (ModelTree, Cmd Msg)
updateModelTree msg completeModelTree =
  Debug.log ("msg = " ++ toString msg)
  (case msg of
    Init jsonStr -> (modelFromJson [] << decodeJson <| jsonStr, Cmd.none)

    ClickEl _ Nothing -> (completeModelTree, Cmd.none)

    ClickEl (AddController Self) (Just path) -> 
      (addControllerToTree path completeModelTree, Cmd.none)

    ClickEl (DeleteController Self) (Just path) -> 
      (deleteControllerFromTree path completeModelTree, Cmd.none)

    ControllerBoxClick corner p (Just path) pos dim -> 
      (toggleControllerResizeClick corner path p pos dim completeModelTree, Cmd.none)

    KeyPress x -> 
      (handleKeyPress x completeModelTree, Cmd.none)

    MouseMove p -> 
      (resizeClicked p completeModelTree, Cmd.none)

    _ -> (completeModelTree, Cmd.none)
  )


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

makeControllerAttributes : ModelRecord -> Controller -> List (Attribute Msg)
makeControllerAttributes modelRecord controller =
  [ makeControllerStyle controller 
  , clickableAttribute modelRecord.pathFromRoot (DeleteController Self)]


makeAttributes : ModelRecord -> List (Attribute Msg)
makeAttributes modelRecord =
 [ attribute "pathFromRoot" <| toString modelRecord.pathFromRoot
 , makeStyle modelRecord
 , clickableAttribute (modelRecord.pathFromRoot) (AddController Self)
 ]


resizeControllerStyle : Corner -> Dimensions -> Attribute Msg
resizeControllerStyle corner (x, y) =
  let
    (hPos, vPos) = case corner of
      TopRight -> (("top", px 0), ("right", px 0))
      BottomRight -> (("bottom", px 0), ("right", px 0))
      BottomLeft -> (("bottom", px 0), ("left", px 0))
      TopLeft -> (("top", px 0), ("left", px 0))
      TopCenter -> (("top", px 0), ("left", px 15))
  in
    style
      [ ("position", "absolute")
      , hPos
      , vPos
      , ("border", "1px solid green")
      , ("height", px y)
      , ("width", px x)
      ]

resizeControllerAttributes : Corner -> Dimensions -> Maybe PathFromRoot -> ModelRecord -> List (Attribute Msg)
resizeControllerAttributes corner dimensions pathFromRoot mRecord =
  [ resizeControllerStyle corner dimensions
  , clickableAttributeCorner corner pathFromRoot mRecord.position mRecord.dimensions
  , id (toString corner)
  ]

resizeControllerHtml : Corner -> Dimensions -> Maybe PathFromRoot -> ModelRecord -> Html Msg
resizeControllerHtml corner dimensions pathFromRoot mRecord =
  div (resizeControllerAttributes corner dimensions pathFromRoot mRecord) []

controllerToHtml : ModelRecord -> Controller -> Html Msg
controllerToHtml modelRecord controller =
  div
    (makeControllerAttributes modelRecord controller)
    (List.map 
      (\c -> resizeControllerHtml c (10, 10) (Just modelRecord.pathFromRoot) modelRecord) 
      [TopRight, BottomRight, BottomLeft, TopLeft, TopCenter])

treeToHtml : ModelTree -> Html Msg
treeToHtml (ModelTree modelRecord) =
    let
      controllers = List.map (controllerToHtml modelRecord) modelRecord.controllers
      children = List.map treeToHtml modelRecord.children
    in
      controllers ++ children
      |> div (makeAttributes modelRecord)


{- View -}

init : {jsonStr : String} -> (ModelTree, Cmd Msg)
init {jsonStr} =
  (modelFromJson [] << decodeJson <| jsonStr, Cmd.none)

program = 
  Html.App.programWithFlags
    { init = init
    , update = updateModelTree
    , subscriptions = subscriptions
    , view = treeToHtml
    }

main = program
