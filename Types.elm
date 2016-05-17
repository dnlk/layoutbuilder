module Types exposing (..)

type alias Dimensions = (Int, Int)
type alias Position = (Int, Int)
type alias PathFromRoot = (Maybe (List Int))
type alias PositionRec = {x : Int, y : Int}

type Corner 
  = TopRight
  | BottomRight
  | BottomLeft
  | TopLeft

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
  , pathFromRoot : List Int
  , controllers : List Controller
  }
getModelRecord mTree = 
  case mTree of
    (ModelTree mRecord) -> mRecord
    EmptyModel -> ModelRecord (0, 0) (0, 0) [] [] []


type alias ClickedCorner =
  { corner : Corner
  , positionClicked : PositionRec
  , initialPos : Position
  , initialDim : Dimensions
  }

type alias Controller =
  { clickedCorner : Maybe ClickedCorner }

type alias JsonStr = String


type Relation
  = Self
  | Parent
  | Child Int

type TreeCmd 
  = Delete Relation
  | AddController Relation
  | DeleteController Relation


type Msg
  = Init JsonStr
  | ClickEl TreeCmd PathFromRoot
  | ControllerBoxClick Corner {x : Int, y : Int } PathFromRoot Position Dimensions
  | MouseMove PositionRec
  --| ControllerBoxDrag Corner Position PathFromRoot

  --| ClickEl (Maybe (List Int))

type alias Context =
  { clickedElement : PathFromRoot }

