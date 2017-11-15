module Zoom.Types exposing (..)

import Canvas exposing (Error, DrawOp(..), Canvas)


type alias Model =
  { image : Image
  }


type Msg
  = NoOp


type Image
  = Loading
  | GotCanvas Canvas
