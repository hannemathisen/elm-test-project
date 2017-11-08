module Types exposing (..)

import Canvas.Point exposing (Point)
import Canvas exposing (Error, DrawOp(..), Canvas)
import Canvas.Events exposing (Touch)


type alias Model =
  { mode : Mode
  , drawData : DrawData
  , draw : Bool
  }


type Mode
  = Loading
  | DrawMode Canvas
  | EraseMode Canvas


type alias PointData =
  { position : { x : Float, y : Float }
  , points : List Point
  }


type alias DrawData =
  { currentPointData : PointData
  , allPointData : List PointData
  , drawOps : List DrawOp
  }


initDrawData : DrawData
initDrawData =
  { currentPointData =
    { position = { x = 0.0, y = 0.0 }
    , points = []
    }
  , allPointData = []
  , drawOps = []
  }


type Msg
  = ImageLoaded (Result Error Canvas)
  | MouseDown Point
  | MouseUp Point
  | MouseMove Point
  | TouchDown { targetTouches : List Touch, points : List Point }
  | TouchUp { targetTouches : List Touch, points : List Point }
  | TouchMove { targetTouches : List Touch, points : List Point }
  | EraseClicked Point
