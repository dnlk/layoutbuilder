module Types exposing (..)

type alias Dimensions = (Int, Int)
type alias Position = (Int, Int)
type alias PathFromRoot = List Int
type alias PositionRec = {x : Int, y : Int}

type Corner 
  = TopRight
  | BottomRight
  | BottomLeft
  | TopLeft
  | TopCenter

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
  , pathFromRoot : PathFromRoot
  , controllers : List Controller
  }
getModelRecord (ModelTree mRecord) = mRecord


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
  | ClickEl TreeCmd (Maybe PathFromRoot)
  | ControllerBoxClick Corner PositionRec (Maybe PathFromRoot) Position Dimensions
  | MoverBoxClick PositionRec (Maybe PathFromRoot) Position
  | MouseMove PositionRec
  | KeyPress Int
  --| ControllerBoxDrag Corner Position (Maybe PathFromRoot)
  
  --| ClickEl (Maybe (List Int))

type alias Context =
  { clickedElement : Maybe PathFromRoot }

