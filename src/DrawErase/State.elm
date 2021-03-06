module DrawErase.State exposing (..)

import DrawErase.Types exposing (..)
import Canvas exposing (Size, Error, DrawOp(..), DrawImageParams(..), Canvas)
import Canvas.Point exposing (Point)
import Canvas.Point as Point
import List.Extra exposing (..)
import Color exposing (Color)


init : ( Model, Cmd Msg )
init =
  let
    model =
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
      case model.mode of
        Erase ->
          let
            x = List.head model.drawData.previousDrawnPoints
          in
            case x of
              Nothing ->
                ( { model | draw = False }, Cmd.none )
              Just a ->
                if model.drawData.drawnPoints == a then
                  ( { model | draw = True}, Cmd.none )
                else
                  let
                    drawData =
                        model.drawData

                    newDrawData =
                        { drawData
                          | previousDrawnPoints =
                              model.drawData.drawnPoints :: model.drawData.previousDrawnPoints }
                  in
                    ( { model
                        | draw = True
                        , drawData = newDrawData
                      }
                    , Cmd.none
                    )

        Draw ->
          ( { model | draw = True }, Cmd.none )

    MouseUp point ->
      case model.mode of
        Draw ->
          let
            x = List.head model.drawData.previousDrawnPoints
          in
            case x of
              Nothing ->
                let
                  drawData =
                      model.drawData

                  newcurrentPoints = []

                  newDrawData =
                      { drawData
                        | drawnPoints = model.drawData.currentPoints :: model.drawData.drawnPoints
                        , currentPoints = newcurrentPoints
                        , previousDrawnPoints = model.drawData.drawnPoints :: model.drawData.previousDrawnPoints
                      }
                in
                  ( { model
                      | draw = False
                      , drawData = newDrawData
                    }
                  , Cmd.none
                  )
              Just a ->
                if model.drawData.drawnPoints == a then
                  ( { model | draw = False}, Cmd.none )
                else
                  let
                    drawData =
                        model.drawData

                    newcurrentPoints = []

                    newDrawData =
                        { drawData
                          | drawnPoints = model.drawData.currentPoints :: model.drawData.drawnPoints
                          , currentPoints = newcurrentPoints
                          , previousDrawnPoints = model.drawData.drawnPoints :: model.drawData.previousDrawnPoints
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
                  model.drawData.currentPoints ++ [ point ]

                lineDrawOps =
                  List.concat
                    (List.map (\newPoints -> pointsToLineOperations newPoints)
                      (newPoints :: model.drawData.drawnPoints)
                    )

                newDrawOps =
                  concatDrawOps lineDrawOps

                drawData = model.drawData

                newDrawData =
                  { drawData
                      | currentPoints = newPoints
                      , drawOps = newDrawOps
                  }
              in
                ( { model | drawData = newDrawData }
                  , Cmd.none
                )

            False ->
              ( model, Cmd.none )

        Erase ->
          let
            drawData = model.drawData

            newPoints = erase point drawData.drawnPoints

            lineDrawOps =
              List.concat
                (List.map (\newPoints -> pointsToLineOperations newPoints)
                  newPoints
                )

            newDrawOps =
              concatDrawOps lineDrawOps

            newDrawData =
              { drawData
                | drawnPoints = newPoints
                , drawOps = newDrawOps
              }

          in
            ( { model | drawData = newDrawData }, Cmd.none )


    TouchDown event ->
      case event.points of
          [] ->
              ( model, Cmd.none )

        -- kanskje bare for [point] ?
          _ ->
            case model.mode of
              Draw ->
                ( { model | draw = True}, Cmd.none)
              Erase ->
                let
                  x = List.head model.drawData.previousDrawnPoints
                in
                  case x of
                    Nothing ->
                      ( { model | draw = False }, Cmd.none )
                    Just a ->
                      if model.drawData.drawnPoints == a then
                        ( { model | draw = True}, Cmd.none )
                      else
                        let
                          drawData =
                              model.drawData

                          newDrawData =
                              { drawData
                                | previousDrawnPoints =
                                    model.drawData.drawnPoints :: model.drawData.previousDrawnPoints }
                        in
                          ( { model
                              | draw = True
                              , drawData = newDrawData
                            }
                          , Cmd.none
                          )

    TouchUp event ->
      case event.points of
          [] ->
              ( model, Cmd.none )

          [point] ->
              case model.mode of
                Draw ->
                  let
                    x = List.head model.drawData.previousDrawnPoints
                  in
                    case x of
                      Nothing ->
                        let
                          drawData =
                              model.drawData

                          newcurrentPoints = []

                          newDrawData =
                              { drawData
                                | drawnPoints = model.drawData.currentPoints :: model.drawData.drawnPoints
                                , currentPoints = newcurrentPoints
                                , previousDrawnPoints = model.drawData.drawnPoints :: model.drawData.previousDrawnPoints
                              }
                        in
                          ( { model
                              | draw = False
                              , drawData = newDrawData
                            }
                          , Cmd.none
                          )
                      Just a ->
                        if model.drawData.drawnPoints == a then
                          ( { model | draw = False}, Cmd.none )
                        else
                          let
                            drawData =
                                model.drawData

                            newcurrentPoints = []

                            newDrawData =
                                { drawData
                                  | drawnPoints = model.drawData.currentPoints :: model.drawData.drawnPoints
                                  , currentPoints = newcurrentPoints
                                  , previousDrawnPoints = model.drawData.drawnPoints :: model.drawData.previousDrawnPoints
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

          point :: tl ->
              ( { model | draw = False }, Cmd.none )

    TouchMove event ->
      case event.points of
          [] ->
              ( model, Cmd.none )

          [point] ->
            case model.mode of
              Draw ->
                let
                  newPoints =
                      model.drawData.currentPoints ++ [ point ]

                  lineDrawOps =
                      List.concat
                          (List.map (\newPoints -> pointsToLineOperations newPoints)
                              (newPoints :: model.drawData.drawnPoints)
                          )

                  newDrawOps = concatDrawOps lineDrawOps

                  drawData = model.drawData

                  newDrawData =
                    { drawData
                        | currentPoints = newPoints
                        , drawOps = newDrawOps
                    }
                in
                  ( { model | drawData = newDrawData }
                  , Cmd.none
                  )

              Erase ->
                let
                  drawData = model.drawData

                  newPoints = erase point drawData.drawnPoints

                  lineDrawOps =
                    List.concat
                      (List.map (\newPoints -> pointsToLineOperations newPoints)
                        newPoints
                      )

                  newDrawOps = concatDrawOps lineDrawOps

                  newDrawData =
                    { drawData
                        | drawnPoints = newPoints
                        , drawOps = newDrawOps
                    }
                in
                  ( { model | drawData = newDrawData }
                  , Cmd.none
                  )

          point :: tl ->
            -- if model.mode == Erase then
            --   let
            --     drawData = model.drawData
            --
            --     newPoints = erase point drawData.drawnPoints
            --
            --     lineDrawOps =
            --       List.concat
            --         (List.map (\newPoints -> pointsToLineOperations newPoints)
            --           newPoints
            --         )
            --
            --     newDrawOps = concatDrawOps lineDrawOps
            --
            --     newDrawData =
            --       { drawData
            --           | drawnPoints = newPoints
            --           , drawOps = newDrawOps
            --       }
            --   in
            --     ( { model | drawData = newDrawData }
            --     , Cmd.none
            --     )
            -- else
            ( { model | draw = False }, Cmd.none )

    ClearClicked ->
      ( { model
            | drawData = initDrawData
            , mode = Draw
        }
      , Cmd.none
      )

    UndoClicked ->
      case model.drawData.previousDrawnPoints of
        [] ->
          ( { model | mode = Draw }
          , Cmd.none
          )

        x :: xs ->
          let
            drawData = model.drawData

            lineDrawOps =
              List.concat
                (List.map (\x -> pointsToLineOperations x)
                  x
                )

            newDrawOps =
              concatDrawOps lineDrawOps

            newDrawData =
              { drawData
                | drawnPoints = x
                , previousDrawnPoints = xs
                , drawOps = newDrawOps
              }

          in
            ( { model | drawData = newDrawData }
            , Cmd.none
            )

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


pointsToLineOperations : List Point -> List DrawOp
pointsToLineOperations points =
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
erase point drawnPoints  =
  let
    ( x, y ) = Point.toInts point
    -- Change based on screen size
    xPoints = List.range ( x-0 ) ( x+0 )
      |> List.map toFloat
    yPoints = List.range ( y-0 ) ( y+0 )
      |> List.map toFloat

    points = mapYPoints xPoints yPoints
  in
    removePoints drawnPoints points


removePoints : List (List Point) -> List (Float, Float) -> List (List Point)
removePoints drawnPoints pointsToErase =
  case pointsToErase of
    [] -> Debug.crash "Empty list"
    [x] ->
      let
        point = Point.fromFloats x
      in
        splitAndRemove drawnPoints point
    (x::xs) ->
      let
        point = Point.fromFloats x
      in
        removePoints (splitAndRemove drawnPoints point) xs


splitAndRemove : List (List Point) -> Point -> List (List Point)
splitAndRemove drawnPoints point =
  case drawnPoints of
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
