module Types exposing (..)

import Canvas.Point exposing (Point)
import Canvas exposing (Error, DrawOp(..), Canvas)


type alias Model =
  { mode : Mode
  , drawData : DrawData
  , draw : Bool
  }


type Mode
  = Loading
  | DrawMode Canvas
  -- | Draw Canvas
  | EraseMode Canvas
  -- | Erase Canvas


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
  | EraseClicked Point
