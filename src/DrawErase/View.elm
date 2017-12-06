module DrawErase.View exposing (..)

import DrawErase.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Canvas.Events as Events
import Canvas.Point as Point
import Canvas exposing (Size, Error, DrawOp(..), DrawImageParams(..), Canvas)
import Color exposing (Color)

view : Model -> Html Msg
view model =
  div []
    [ div []
      [ p [] [ text "Try to draw the outline of the circle, or just play around." ] ]
    , div [] [ presentIfReady model ]
    , div []
      [ button
        [ class "btn", onClick EraseClicked ]
        [ case model.mode of
          Draw ->
             text "Erase off"
          Erase ->
            text "Erase on"
        ]
      , button
        [ class "btn", onClick UndoClicked ]
        [ text "Undo" ]
      , button
        [ class "btn", onClick ClearClicked ]
        [ text "Clear" ]
      ]
    ]


presentIfReady : Model -> Html Msg
presentIfReady model =
  case model.image of
    Loading ->
      p [] [ text "Loading image..." ]

    GotCanvas canvas ->
      let
        touchOptions =
          { stopPropagation = True
          , preventDefault = True
          }

        events =
          case model.draw of
            True ->
              [ Events.onMouseUp MouseUp
              , Events.onMouseMove MouseMove
              , Events.onMultiTouchMove touchOptions TouchMove
              , Events.onMultiTouchEnd touchOptions TouchUp
              , Events.onMultiTouchCancel touchOptions TouchUp
              ]

            False ->
              [ Events.onMouseDown MouseDown
              , Events.onMultiTouchStart touchOptions TouchDown
              ]

        allEvents =
          if model.mode == Erase then
            events ++ [ class "add-eraser" ]
          else
            events

      in
        canvas
          |> drawCanvas model.drawData.drawOps
          |> Canvas.toHtml allEvents


drawCanvas : List DrawOp -> Canvas -> Canvas
drawCanvas drawOps canvas =
  let
      { width, height } =
          Canvas.getSize canvas

      drawOpsWithBorder : List DrawOp
      drawOpsWithBorder =
          List.append
              drawOps
              [ BeginPath
              , StrokeStyle (Color.rgb 255 0 0)
              , LineWidth 2.0
              , Rect (Point.fromInts ( 0, 0 )) (Size width height)
              , Stroke
              ]
  in
      Canvas.batch drawOpsWithBorder canvas
