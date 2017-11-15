module Draw.View exposing (..)

import Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Canvas.Events as Events
import Canvas.Point as Point
import Canvas exposing (Size, Error, DrawOp(..), DrawImageParams(..), Canvas)
import Color exposing (Color)

view : Model -> Html Msg
view model =
  case model.mode of
    Draw ->
      div
        []
        [ div [] [ presentIfReady model ]
        , div [] [ button [ class "btn", Events.onClick EraseClicked ] [ text "Erase is off"] ]
        ]

    Erase ->
      div
        []
        [ div [] [ presentIfReady model ]
        , div [] [ button [ class "btn", Events.onClick EraseClicked ] [ text "Erase is on"] ]
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
      in
        case model.draw of
          True ->
            case model.mode of
              Draw ->
                let
                  touchOptions =
                    { stopPropagation = True
                    , preventDefault = True
                    }
                in
                  canvas
                    |> drawCanvas model.drawData.drawOps
                    |> Canvas.toHtml
                      [ Events.onMouseUp MouseUp
                      , Events.onMouseMove MouseMove
                      , Events.onMultiTouchMove touchOptions TouchMove
                      , Events.onMultiTouchEnd touchOptions TouchUp
                      , Events.onMultiTouchCancel touchOptions TouchUp
                      ]
              Erase ->
                let
                  touchOptions =
                    { stopPropagation = True
                    , preventDefault = True
                    }
                in
                  canvas
                    |> drawCanvas model.drawData.drawOps
                    |> Canvas.toHtml
                      [ class "eraser"
                      , Events.onMouseUp MouseUp
                      , Events.onMouseMove MouseMove
                      , Events.onMultiTouchMove touchOptions TouchMove
                      , Events.onMultiTouchEnd touchOptions TouchUp
                      , Events.onMultiTouchCancel touchOptions TouchUp
                      ]
          False ->
            case model.mode of
              Draw ->
                let
                  touchOptions =
                    { stopPropagation = True
                    , preventDefault = True
                    }
                in
                  canvas
                    |> drawCanvas model.drawData.drawOps
                    |> Canvas.toHtml
                      [ Events.onMouseDown MouseDown
                      , Events.onMultiTouchStart touchOptions TouchDown
                      ]
              Erase ->
                let
                  touchOptions =
                    { stopPropagation = True
                    , preventDefault = True
                    }
                in
                  canvas
                    |> drawCanvas model.drawData.drawOps
                    |> Canvas.toHtml
                      [ class "eraser"
                      , Events.onMouseDown MouseDown
                      , Events.onMultiTouchStart touchOptions TouchDown
                      ]


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
              , Rect (Point.fromInts ( 0, 0 )) (Size 800 600)
              , Stroke
              ]
  in
      Canvas.batch drawOpsWithBorder canvas
