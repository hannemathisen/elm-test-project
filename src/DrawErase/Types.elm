module DrawErase.Types exposing (..)

import Canvas.Point exposing (Point)
import Canvas exposing (Error, DrawOp(..), Canvas)
import Canvas.Events exposing (Touch)


type alias Model =
  { mode : Mode
  , drawData : DrawData
  , draw : Bool
  , image : Image
  }


type Mode
  = Draw
  | Erase


type alias PointData =
  { position : { x : Float, y : Float }
  , points : List Point
  }

type alias Points = List Point

type alias DrawData =
  { currentPoints : Points
  , drawnPoints : List Points
  , previousDrawnPoints : List (List Points)
  , drawOps : List DrawOp
  }


initDrawData : DrawData
initDrawData =
  { currentPoints = []
  , drawnPoints = []
  , previousDrawnPoints = []
  , drawOps = []
  }


type Msg
  = MouseDown Point
  | MouseUp Point
  | MouseMove Point
  | TouchDown { targetTouches : List Touch, points : List Point }
  | TouchUp { targetTouches : List Touch, points : List Point }
  | TouchMove { targetTouches : List Touch, points : List Point }
  | EraseClicked
  | ClearClicked
  | UndoClicked


type Image
  = Loading
  | GotCanvas Canvas
