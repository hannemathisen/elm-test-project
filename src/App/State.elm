module App.State exposing (..)

import App.Types exposing (..)
import Canvas exposing (Error, DrawOp(..), Canvas)
import Draw.State
import Draw.Types
import Zoom.State
import Zoom.Types
import Task


init : ( Model, Cmd Msg)
init =
  let
    ( drawModel, drawCmd ) =
        Draw.State.init
    ( zoomModel, zoomCmd ) =
        Zoom.State.init
    model =
      { draw = drawModel
      , zoom = zoomModel
      , globalMode = Main
      }
  in
    ( model, loadImage )


loadImage : Cmd Msg
loadImage =
  Task.attempt
    ImageLoaded
    (Canvas.loadImage "white.png")


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
  case msg of
    ImageLoaded result ->
      case Result.toMaybe result of
        Just canvas ->
          let
            newDrawModel = model.draw
            newZoomModel = model.zoom
          in
            ( { model
                | draw = { newDrawModel | image = Draw.Types.GotCanvas canvas }
                , zoom = { newZoomModel | image = Zoom.Types.GotCanvas canvas }
              }
            , Cmd.none
            )
        Nothing ->
          (  model
          , loadImage
          )

    ChangeMode mode ->
      ( { model | globalMode = mode }, Cmd.none )

    DrawMsg drawMsg ->
      let
        ( drawModel, drawCmd) =
          Draw.State.update drawMsg model.draw
      in
        ( { model | draw = drawModel}
        , Cmd.map DrawMsg drawCmd
        )

    ZoomMsg zoomMsg ->
      let
        ( zoomModel, zoomCmd) =
          Zoom.State.update zoomMsg model.zoom
      in
        ( { model | zoom = zoomModel}
        , Cmd.map ZoomMsg zoomCmd
        )
