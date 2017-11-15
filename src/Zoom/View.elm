module Zoom.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Zoom.Types exposing (..)
import Canvas.Point as Point
import Canvas exposing (Size, Error, DrawOp(..), DrawImageParams(..), Canvas)
import Color exposing (Color)


view : Model -> Html Msg
view model =
  div []
    [ div [] [ presentIfReady model ] ]

presentIfReady : Model -> Html Msg
presentIfReady model =
  case model.image of
    Loading ->
      p [] [ text "Loading image..." ]

    GotCanvas canvas ->
      canvas
        |> drawCanvas 
        |> Canvas.toHtml []


drawCanvas : Canvas -> Canvas
drawCanvas canvas =
  let
      { width, height } =
          Canvas.getSize canvas

      drawOpsWithBorder : List DrawOp
      drawOpsWithBorder =
        [ BeginPath
        , StrokeStyle (Color.rgb 255 0 0)
        , LineWidth 2.0
        , Rect (Point.fromInts ( 0, 0 )) (Size 800 600)
        , Stroke
        ]
  in
      Canvas.batch drawOpsWithBorder canvas
