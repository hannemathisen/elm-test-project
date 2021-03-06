module DrawOld.Types exposing (..)

import Canvas.Point exposing (Point)
import Canvas exposing (Error, DrawOp(..), Canvas)
import Canvas.Events exposing (Touch)


type alias Model =
  { drawData : DrawData
  , draw : Bool
  , image : Image
  }


type alias PointData =
  { position : { x : Float, y : Float }
  , points : List Point
  }


type alias DrawData =
  { currentPoints : List Point
  , drawnPoints : List (List Point)
  , drawOps : List DrawOp
  }


initDrawData : DrawData
initDrawData =
  { currentPoints = []
  , drawnPoints = []
  , drawOps = []
  }


type Msg
  = MouseDown Point
  | MouseUp Point
  | MouseMove Point
  | TouchDown { targetTouches : List Touch, points : List Point }
  | TouchUp { targetTouches : List Touch, points : List Point }
  | TouchMove { targetTouches : List Touch, points : List Point }
  | ClearClicked
  | UndoClicked


type Image
  = Loading
  | GotCanvas Canvas
