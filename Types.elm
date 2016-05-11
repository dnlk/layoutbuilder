module Types where

type alias Dimensions = (Int, Int)
type alias Position = (Int, Int)

type  JsonTree = JsonTree JsonRecord
type alias JsonRecord = 
  { dimensions : Dimensions
  , position : Position
  , children : List JsonTree
  }

type ModelTree = ModelTree ModelRecord | EmptyModel
type alias ModelRecord =
  { dimensions : Dimensions
  , position : Position
  , children : List ModelTree
  , nodeId : Int
  , pathFromRoot : List Int
  , controllers : List Controller
  }
getModelRecord mTree = 
  case mTree of
    (ModelTree mRecord) -> mRecord
    EmptyModel -> ModelRecord (0, 0) (0, 0) [] 0 [] []


type alias Controller =
  {}

type alias JsonStr = String

type Trigger
  = Init JsonStr
  | ClickEl (Maybe (List Int))

type alias Context =
  { clickedElement : Maybe (List Int) }