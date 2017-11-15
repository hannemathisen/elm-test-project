module App.Types exposing (..)

import Draw.Types
import Canvas.Point exposing (Point)
import Canvas exposing (Error, DrawOp(..), Canvas)


type alias Model =
  { globalMode : Mode
  , draw : Draw.Types.Model
  }


type Mode
  = Draw
  | Zoom
  | Main


type Msg
  = ChangeMode Mode
  | DrawMsg Draw.Types.Msg
  | ImageLoaded (Result Error Canvas)
