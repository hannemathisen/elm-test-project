module App.Types exposing (..)

import DrawErase.Types
import DrawOld.Types
import Zoom.Types
import Canvas.Point exposing (Point)
import Canvas exposing (Error, DrawOp(..), Canvas)


type alias Model =
  { globalMode : Mode
  , drawErase : DrawErase.Types.Model
  , drawOld : DrawOld.Types.Model
  , zoom : Zoom.Types.Model
  }


type Mode
  = DrawErase
  | DrawOld
  | Zoom
  | Main


type Msg
  = ChangeMode Mode
  | DrawEraseMsg DrawErase.Types.Msg
  | DrawOldMsg DrawOld.Types.Msg
  | ZoomMsg Zoom.Types.Msg
  | ImageLoaded (Result Error Canvas)
