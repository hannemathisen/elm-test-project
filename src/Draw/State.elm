module Draw.State exposing (..)

import Draw.Types exposing (..)
import Canvas exposing (Size, Error, DrawOp(..), DrawImageParams(..), Canvas)
import Canvas.Point exposing (Point)
import Canvas.Point as Point
import List.Extra exposing (..)
import Color exposing (Color)


init : ( Model, Cmd Msg )
init =
  let model =
    { mode = Draw
    , drawData = initDrawData
    , draw = False
    , image = Loading
    }
  in
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    MouseDown point ->
      ( { model | draw = True }, Cmd.none )

    MouseUp point ->
      case model.mode of
        Draw ->
          let
              drawData =
                  model.drawData

              newCurrentPointData = []

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
                newPointData =
                  model.drawData.currentPointData ++ [ point ]

                lineDrawOps =
                  List.concat
                    (List.map (\newPointData -> pointDataToLineOperations newPointData)
                      (newPointData :: model.drawData.allPointData)
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

            newPoints = erase point drawData.allPointData

            lineDrawOps =
              List.concat
                (List.map (\newPoints -> pointDataToLineOperations newPoints)
                  newPoints
                )

            newDrawOps =
              concatDrawOps lineDrawOps

            newDrawData =
              { drawData | allPointData = newPoints, drawOps = newDrawOps }

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
                        []

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
              ( { model | draw = False }, Cmd.none )

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
                  newPointData =
                      model.drawData.currentPointData ++ [ point ]

                  lineDrawOps =
                      List.concat
                          (List.map (\newPointData -> pointDataToLineOperations newPointData)
                              (newPointData :: model.drawData.allPointData)
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

                  newPoints = erase point drawData.allPointData

                  lineDrawOps =
                    List.concat
                      (List.map (\newPoints -> pointDataToLineOperations newPoints)
                        newPoints
                      )

                  newDrawOps = concatDrawOps lineDrawOps

                  newDrawData =
                    { drawData | allPointData = newPoints, drawOps = newDrawOps }
                in
                  ( { model | drawData = newDrawData }, Cmd.none )

          point :: tl ->
              ( { model | draw = False }, Cmd.none )

    EraseClicked ->
      case model.mode of
        Draw ->
          ( { model | mode = Erase }
          , Cmd.none
          )

        Erase ->
          ( { model | mode = Draw }
          , Cmd.none
          )


pointDataToLineOperations : List Point -> List DrawOp
pointDataToLineOperations points =
  case points of
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


erase : Point -> List (List Point) -> List (List Point)
erase point oldPoints =
  let
    ( x, y ) = Point.toInts point
    -- Change based on screen size
    xPoints = List.range ( x-7 ) ( x+7 )
      |> List.map toFloat
    yPoints = List.range ( y-7 ) ( y+7 )
      |> List.map toFloat

    points = mapYPoints xPoints yPoints
  in
    removePoints oldPoints points


removePoints : List (List Point) -> List (Float, Float) -> List (List Point)
removePoints allPoints erasedPoints =
  case erasedPoints of
    [] -> Debug.crash "Empty list"
    [x] ->
      let
        point = Point.fromFloats x
      in
        splitAndRemove allPoints point
    (x::xs) ->
      let
        point = Point.fromFloats x
      in
        removePoints (splitAndRemove allPoints point) xs


splitAndRemove : List (List Point) -> Point -> List (List Point)
splitAndRemove allPoints point =
  case allPoints of
    [] -> Debug.crash "Empty list"
    [x] ->
      let
        index = List.Extra.elemIndex point x
      in
        case index of
          Nothing ->
            [x]
          Just i ->
            let
              list1 = List.take (i-1) x
              list2 = List.drop i x
            in
              if List.isEmpty list1 then
                [list2]
              else
                [list1, list2]
    (x::xs) ->
      let
        index = List.Extra.elemIndex point x
      in
        case index of
          Nothing ->
            List.append [x] (splitAndRemove xs point)
          Just i ->
            let
              list1 = List.take (i-1) x
              list2 = List.drop i x
            in
              if List.isEmpty list1 then
                List.append [list2] (splitAndRemove xs point)
              else
                List.append [list1, list2] (splitAndRemove xs point)


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
