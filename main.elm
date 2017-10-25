module Main exposing (..)

import Html exposing (..)
import Canvas exposing (Size, Error, DrawOp(..), DrawImageParams(..), Canvas)
import Canvas.Point exposing (Point)
import Canvas.Point as Point
import Canvas.Events as Events
import Color exposing (Color)
import Task


main =
  Html.program
    { init = ( Loading, loadImage)
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }


type Msg
  = ImageLoaded (Result Error Canvas)
  | MouseDown Point
  | MouseUp Point
  | MouseMove Point


type Model
  = GotCanvas Canvas (List DrawOp)
  | Loading
  | Draw Canvas (List DrawOp)


loadImage : Cmd Msg
loadImage =
  Task.attempt
    ImageLoaded
    (Canvas.loadImage "./cat.jpg")


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ImageLoaded result ->
      case Result.toMaybe result of
        Just canvas ->
          ( GotCanvas canvas  []
          , Cmd.none
          )
        Nothing ->
          ( Loading
          , loadImage
          )

    MouseDown point ->
      case model of
        Loading ->
          ( Loading
          , loadImage
          )
        GotCanvas canvas drawOps ->
          let
            newDrawOps =
              List.append
                drawOps
                  [ MoveTo point]
          in
            ( Draw canvas (draw point canvas newDrawOps)
            , Cmd.none
            )
        Draw canvas drawOps ->
          ( Draw canvas (draw point canvas drawOps)
          , Cmd.none
          )

    MouseUp point ->
      case model of
        Loading ->
          ( Loading
          , loadImage
          )
        GotCanvas canvas drawOps ->
          ( GotCanvas canvas (drawOps)
          , Cmd.none
          )
        Draw canvas drawOps ->
          ( GotCanvas canvas (drawOps)
          , Cmd.none
          )

    MouseMove point ->
      case model of
        Loading ->
          ( Loading
          , loadImage
          )
        GotCanvas canvas drawOps ->
          ( GotCanvas canvas (drawOps)
          , Cmd.none
          )
        Draw canvas drawOps ->
          ( Draw canvas (draw point canvas drawOps)
          , Cmd.none
          )


draw : Point -> Canvas -> List DrawOp -> List DrawOp
draw point canvas drawOps =
  List.append
    drawOps
      [ LineWidth 1.0
      , StrokeStyle (Color.rgb 255 0 0)
      , LineTo point
      , Stroke
      ]


view : Model -> Html Msg
view model =
  div
    []
    [ presentIfReady model ]


presentIfReady : Model -> Html Msg
presentIfReady model =
  case model of
    Loading ->
      p [] [ text "Loading..." ]

    GotCanvas canvas drawOps ->
      canvas
        |> drawCanvas drawOps
        |> Canvas.toHtml
          [ Events.onMouseDown MouseDown
          , Events.onMouseUp MouseUp
          , Events.onMouseMove MouseMove
          ]

    Draw canvas drawOps ->
      canvas
        |> drawCanvas drawOps
        |> Canvas.toHtml
          [ Events.onMouseDown MouseDown
          , Events.onMouseUp MouseUp
          , Events.onMouseMove MouseMove
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
              , Rect (Point.fromInts ( 0, 0 )) (Size 200 200)
              , Stroke
              ]
  in
      Canvas.batch drawOpsWithBorder canvas
