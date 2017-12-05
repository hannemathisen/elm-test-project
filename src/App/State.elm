module App.State exposing (..)

import App.Types exposing (..)
import Canvas exposing (Error, DrawOp(..), Canvas)
import DrawErase.State
import DrawErase.Types
import DrawOld.State
import DrawOld.Types
import Zoom.State
import Zoom.Types
import Task


init : ( Model, Cmd Msg)
init =
  let
    ( drawEraseModel, drawEraseCmd ) =
        DrawErase.State.init
    ( drawOldModel, drawOldCmd ) =
        DrawOld.State.init
    ( zoomModel, zoomCmd ) =
        Zoom.State.init
    model =
      { drawErase = drawEraseModel
      , drawOld = drawOldModel
      , zoom = zoomModel
      , globalMode = Main
      }
  in
    ( model, loadImage )


loadImage : Cmd Msg
loadImage =
  Task.attempt
    ImageLoaded
    (Canvas.loadImage "testpicture.png")


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
  case msg of
    ImageLoaded result ->
      case Result.toMaybe result of
        Just canvas ->
          let
            newDrawEraseModel = model.drawErase
            newDrawOldModel = model.drawOld
            newZoomModel = model.zoom
          in
            ( { model
                | drawErase = { newDrawEraseModel | image = DrawErase.Types.GotCanvas canvas }
                , drawOld = { newDrawOldModel | image = DrawOld.Types.GotCanvas canvas }
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

    DrawEraseMsg drawMsg ->
      let
        ( drawModel, drawCmd) =
          DrawErase.State.update drawMsg model.drawErase
      in
        ( { model | drawErase = drawModel}
        , Cmd.map DrawEraseMsg drawCmd
        )

    DrawOldMsg drawMsg ->
      let
        ( drawModel, drawCmd) =
          DrawOld.State.update drawMsg model.drawOld
      in
        ( { model | drawOld = drawModel}
        , Cmd.map DrawOldMsg drawCmd
        )

    ZoomMsg zoomMsg ->
      let
        ( zoomModel, zoomCmd) =
          Zoom.State.update zoomMsg model.zoom
      in
        ( { model | zoom = zoomModel}
        , Cmd.map ZoomMsg zoomCmd
        )
