module Zoom.State exposing (..)

import Zoom.Types exposing (..)

init : ( Model, Cmd Msg)
init =
  let
    model =
      { image = Loading }
  in
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
  ( model, Cmd.none )
