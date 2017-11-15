module App.State exposing (..)

import App.Types exposing (..)
import Canvas exposing (Error, DrawOp(..), Canvas)
import Draw.State
import Draw.Types
import Task


init : ( Model, Cmd Msg)
init =
  let
    ( drawModel, drawCmd ) =
        Draw.State.init
    model =
      { draw = drawModel
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
          in
            ( { model | draw = { newDrawModel | image = Draw.Types.GotCanvas canvas } }
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
