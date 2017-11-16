module DrawOld.State exposing (..)


import DrawOld.Types exposing (..)
import Canvas exposing (Size, Error, DrawOp(..), DrawImageParams(..), Canvas)
import Canvas.Point exposing (Point)
import Canvas.Point as Point
import List.Extra exposing (..)
import Color exposing (Color)


init : ( Model, Cmd Msg )
init =
  let
    model =
      { drawData = initDrawData
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
      let
          drawData =
              model.drawData

          newcurrentPoints = []

          newDrawData =
              { drawData
                  | drawnPoints = model.drawData.currentPoints :: model.drawData.drawnPoints
                  , currentPoints = newcurrentPoints
              }
      in
          ( { model
              | draw = False
              , drawData = newDrawData
            }
          , Cmd.none
          )

    MouseMove point ->
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


    TouchDown event ->
      case event.points of
          [] ->
              ( model, Cmd.none )
          point :: tl ->
              let
                  drawData =
                      model.drawData

                  newcurrentPoints =
                      Debug.log "newcurrentPoints" <|
                        []

                  newDrawData =
                      { drawData
                          | drawnPoints =
                              model.drawData.drawnPoints
                                  ++ [ model.drawData.currentPoints ]
                          , currentPoints = newcurrentPoints
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

          point :: tl ->
              ( { model | draw = False }, Cmd.none )


    ClearClicked ->
      ( { model | drawData = initDrawData }, Cmd.none )


    UndoClicked ->
      case model.drawData.drawnPoints of
        [] ->
          ( model, Cmd.none )

        x :: xs ->
          let
            lineDrawOps =
              List.concat
                (List.map (\points -> pointsToLineOperations points)
                  (xs)
                )

            newDrawOps = concatDrawOps lineDrawOps

          in
            ( { model
                | drawData =
                    { currentPoints = []
                    , drawOps = newDrawOps
                    , drawnPoints = xs
                    }
              }
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
