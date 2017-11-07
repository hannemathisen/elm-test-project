module Types exposing (..)

import Canvas.Point exposing (Point)
import Canvas exposing (Error, DrawOp(..), Canvas)

type Msg
  = ImageLoaded (Result Error Canvas)
  | MouseDown Point
  | MouseUp Point
  | MouseMove Point
  | EraseClicked Point


type Model
  = Loading
  | DrawMode Canvas (List DrawOp)
  | Draw Canvas (List DrawOp)
  | EraseMode Canvas (List DrawOp)
  | Erase Canvas (List DrawOp)
