module Main exposing (..)

import Types exposing (..)
import View exposing (..)
import Html exposing (..)
import Canvas exposing (Size, Error, DrawOp(..), DrawImageParams(..), Canvas)
import Canvas.Point exposing (Point)
import Canvas.Point as Point
import List.Extra exposing (..)
import Color exposing (Color)
import Task


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }


init : ( Model, Cmd Msg )
init =
  let model =
    { mode = Draw
    , drawData = initDrawData
    , draw = False
    , image = Loading
    }
  in
    ( model, loadImage )


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
          ( { model
                | mode = Draw
                , image = GotCanvas canvas
            }
          , Cmd.none
          )
        Nothing ->
          ( { model | image = Loading }
          , loadImage
          )

    MouseDown point ->
      ( { model | draw = True }, Cmd.none )

    MouseUp point ->
      case model.mode of
        Draw ->
          let
              drawData =
                  model.drawData

              newCurrentPointData =
                  { position = model.drawData.currentPointData.position
                  , points = []
                  }

              newDrawData =
                  { drawData
                      | allPointData = model.drawData.currentPointData :: model.drawData.allPointData
                      , currentPointData = newCurrentPointData
                  }
          in
              ( { model
                  | draw = False
                  , drawData = newDrawData
                }
              , Cmd.none
              )
        Erase ->
          ( { model | draw = False }, Cmd.none )

    MouseMove point ->
      case model.mode of
        Draw ->
          case model.draw of
            True ->
              let
                newPoints =
                  model.drawData.currentPointData.points ++ [ point ]

                pointData =
                  model.drawData.currentPointData

                newPointData =
                  { pointData | points = newPoints }

                lineDrawOps =
                  List.concat
                    (List.map (\pointData -> pointDataToLineOperations pointData)
                      (pointData :: model.drawData.allPointData)
                    )

                newDrawOps =
                  concatDrawOps lineDrawOps
              in
                ( { model
                      | drawData =
                        { currentPointData = newPointData
                        , drawOps = newDrawOps
                        , allPointData = model.drawData.allPointData
                        }
                  }
                  , Cmd.none
                )

            False ->
              ( model, Cmd.none )

        Erase ->
          let
            drawData = model.drawData

            newDrawOps = erase point drawData.drawOps

            newDrawData =
              { drawData | drawOps = newDrawOps}
          in
            ( { model | drawData = newDrawData }, Cmd.none )

    TouchDown event ->
      case event.points of
          [] ->
              ( model, Cmd.none )

          point :: tl ->
              let
                  drawData =
                      model.drawData

                  newCurrentPointData =
                      Debug.log "newCurrentPointData" <|
                          { position = model.drawData.currentPointData.position
                          , points = []
                          }

                  newDrawData =
                      { drawData
                          | allPointData =
                              model.drawData.allPointData
                                  ++ [ model.drawData.currentPointData ]
                          , currentPointData = newCurrentPointData
                      }
              in
                  ( { model | draw = True, drawData = newDrawData }, Cmd.none )

    TouchUp event ->
      case event.points of
          [] ->
              ( model, Cmd.none )

          point :: [] ->
              ( { model
                  | draw = False
                }
              , Cmd.none
              )

          point :: tl ->
              ( { model | draw = False }, Cmd.none )

    TouchMove event ->
      case event.points of
          [] ->
              ( model, Cmd.none )

          point :: [] ->
            case model.mode of
              Draw ->
                let
                  ( x, y ) =
                      Point.toFloats point

                  newPoint =
                      Point.fromFloats
                          ( x - model.drawData.currentPointData.position.x
                          , y - model.drawData.currentPointData.position.y
                          )

                  newPoints =
                      model.drawData.currentPointData.points ++ [ newPoint ]

                  pointData =
                      model.drawData.currentPointData

                  newPointData =
                      { pointData | points = newPoints }

                  lineDrawOps =
                      List.concat
                          (List.map (\pointData -> pointDataToLineOperations pointData)
                              (pointData :: model.drawData.allPointData)
                          )

                  newDrawOps = concatDrawOps lineDrawOps
                in
                  ( { model
                      | drawData =
                          { currentPointData = newPointData
                          , drawOps = newDrawOps
                          , allPointData = model.drawData.allPointData
                          }
                    }
                  , Cmd.none
                  )

              Erase ->
                let
                  drawData = model.drawData

                  -- ( x, y ) =
                  --     Point.toFloats point
                  --
                  -- newPoint =
                  --     Point.fromFloats
                  --         ( x - model.drawData.currentPointData.position.x
                  --         , y - model.drawData.currentPointData.position.y
                  --         )

                  newDrawOps = erase point drawData.drawOps

                  newDrawData =
                    { drawData | drawOps = newDrawOps}
                in
                  ( { model | drawData = newDrawData }, Cmd.none )

          point :: tl ->
              ( { model | draw = False }, Cmd.none )

    EraseClicked point ->
      case model.mode of
        Draw ->
          ( { model | mode = Erase }
          , Cmd.none
          )

        Erase ->
          ( { model | mode = Draw }
          , Cmd.none
          )


pointDataToLineOperations : PointData -> List DrawOp
pointDataToLineOperations pointData =
  case pointData.points of
    [] ->
      []

    x :: xs ->
      [ MoveTo x ] ++
        (List.map
          (\point ->
            LineTo point) xs
        )


concatDrawOps : List DrawOp -> List DrawOp
concatDrawOps drawOps =
  [ BeginPath
  , LineWidth 3
  , StrokeStyle Color.red
  , LineCap "round"
  ]
    ++ drawOps
    ++ [ Stroke ]


erase : Point -> List DrawOp -> List DrawOp
erase point drawOps =
  let
    ( x, y ) = Point.toInts point

    -- Change based on screen size
    xPoints = List.range ( x-10 ) ( x+10 )
      |> List.map toFloat
    yPoints = List.range ( y-10 ) ( y+10 )
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
