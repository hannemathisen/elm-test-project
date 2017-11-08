module Main exposing (..)

import Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Canvas exposing (Size, Error, DrawOp(..), DrawImageParams(..), Canvas)
import Canvas.Point exposing (Point)
import Canvas.Point as Point
import Canvas.Events as Events
import List.Extra exposing (..)
import Color exposing (Color)
import Task


main =
  Html.program
    { init = ( { mode = Loading}, loadImage)
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }


loadImage : Cmd Msg
loadImage =
  Task.attempt
    ImageLoaded
    (Canvas.loadImage "white.png")


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ImageLoaded result ->
      case Result.toMaybe result of
        Just canvas ->
          ( {mode = (DrawMode canvas []) }
          , Cmd.none
          )
        Nothing ->
          ( { mode = Loading }
          , loadImage
          )

    MouseDown point ->
      case model.mode of
        DrawMode canvas drawOps ->
          let
            newDrawOps =
              List.append
                drawOps
                  [ MoveTo point
                  , LineWidth 3.0
                  , StrokeStyle (Color.rgb 255 0 0)
                  ]
          in
            ( { mode = (Draw canvas newDrawOps) }
            , Cmd.none
            )

        EraseMode canvas drawOps ->
          ( { mode = (Erase canvas drawOps) }
          , Cmd.none
          )

        _ ->
          ( { mode = Loading }
          , loadImage
          )

    MouseUp point ->
      case model.mode of
        Draw canvas drawOps ->
          ( { mode = (DrawMode canvas drawOps) }
          , Cmd.none
          )

        Erase canvas drawOps ->
          ( { mode = (EraseMode canvas drawOps) }
          , Cmd.none
          )

        _ ->
          ( { mode = Loading }
          , loadImage
          )

    MouseMove point ->
      case model.mode of
        Loading ->
          ( {mode = Loading }
          , loadImage
          )

        DrawMode canvas drawOps ->
          ( { mode = (DrawMode canvas drawOps) }
          , Cmd.none
          )

        Draw canvas drawOps ->
          ( { mode = (Draw canvas (draw point canvas drawOps)) }
          , Cmd.none
          )

        EraseMode canvas drawOps ->
          ( { mode = (EraseMode canvas drawOps) }
          , Cmd.none
          )

        Erase canvas drawOps ->
          ( { mode = (Erase canvas (erase point canvas drawOps)) }
          , Cmd.none
          )

    EraseClicked point ->
      case model.mode of
        Loading ->
          ( { mode = Loading }
          , loadImage
          )

        DrawMode canvas drawOps ->
          ( { mode = (EraseMode canvas drawOps) }
          , Cmd.none
          )

        Draw canvas drawOps ->
          ( { mode = (EraseMode canvas drawOps) }
          , Cmd.none
          )

        EraseMode canvas drawOps ->
          ( { mode = (DrawMode canvas drawOps) }
          , Cmd.none
          )

        Erase canvas drawOps ->
          ( { mode = (DrawMode canvas drawOps) }
          , Cmd.none
          )


draw : Point -> Canvas -> List DrawOp -> List DrawOp
draw point canvas drawOps =
  List.append
    drawOps
      [ LineTo point
      , Stroke
      ]


erase : Point -> Canvas -> List DrawOp -> List DrawOp
erase point canvas drawOps =
  let

    ( x, y ) = Point.toInts point

    xPoints = List.range ( x-3 ) ( x+3 )
      |> List.map toFloat
    yPoints = List.range ( y-3 ) ( y+3 )
      |> List.map toFloat

    points = mapYPoints xPoints yPoints
  in
    removePoints drawOps points


removePoints : List DrawOp -> List (Float, Float) -> List DrawOp
removePoints drawOps points =
  case points of
    [] -> Debug.crash "Empty list"
    [x] ->
      let
        point = Point.fromFloats (x)
      in
        List.Extra.replaceIf (\x -> x == LineTo point) (MoveTo point) drawOps

    (x::xs) ->
      let
        point = Point.fromFloats (x)
      in
        let
          newDrawOps =
            List.Extra.replaceIf (\x -> x == LineTo point) (MoveTo point) drawOps
        in
          removePoints newDrawOps xs



mapXPoints : List Float -> List Float -> List( Float, Float )
mapXPoints xList yList =
  case xList of
    [] -> Debug.crash "Empty list"
    [x] ->
      List.map2 (,) [x] yList

    (x::xs) ->
      let
        mappedList =
          List.map2 (,) [x] yList
      in
        List.append
          mappedList
            (mapXPoints xs yList )


mapYPoints : List Float ->  List Float -> List (Float, Float)
mapYPoints xList yList =
  case yList of
    [] -> Debug.crash "Empty list"
    [y] ->
      mapXPoints xList [y]

    (y::ys) ->
      let
        mappedList =
          mapXPoints xList [y]
      in
        List.append
          mappedList
            (mapYPoints xList ys )


view : Model -> Html Msg
view model =
  case model.mode of
    EraseMode canvas drawOps ->
      div
        []
        [ div [] [ presentIfReady model ]
        , div [] [ button [ class "btn", Events.onClick EraseClicked ] [ text "Erase is on"] ]
        ]
    Erase canvas drawOps ->
      div
        []
        [ div [] [ presentIfReady model ]
        , div [] [ button [ class "btn", Events.onClick EraseClicked ] [ text "Erase is on"] ]
        ]
    _ ->
      div
        [ ]
        [ div [] [ presentIfReady model ]
        , div [] [ button [ class "btn", Events.onClick EraseClicked ] [ text "Erase is off"] ]
        ]


presentIfReady : Model -> Html Msg
presentIfReady model =
  case model.mode of
    Loading ->
      p [] [ text "Loading image..." ]

    DrawMode canvas drawOps ->
      canvas
        |> drawCanvas drawOps
        |> Canvas.toHtml
          [ Events.onMouseDown MouseDown ]

    Draw canvas drawOps ->
      canvas
        |> drawCanvas drawOps
        |> Canvas.toHtml
          [ Events.onMouseUp MouseUp
          , Events.onMouseMove MouseMove
          ]

    EraseMode canvas drawOps ->
      canvas
        |> drawCanvas drawOps
        |> Canvas.toHtml
          [ class "eraser"
          , Events.onMouseDown MouseDown
          ]

    Erase canvas drawOps ->
      canvas
        |> drawCanvas drawOps
        |> Canvas.toHtml
          [ class "eraser"
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
              , Rect (Point.fromInts ( 0, 0 )) (Size 800 600)
              , Stroke
              ]
  in
      Canvas.batch drawOpsWithBorder canvas
