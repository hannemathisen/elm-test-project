module Main exposing (..)

import Types exposing (..)
import Draw.State exposing (..)
import Draw.View exposing (..)
import Html exposing (..)


main =
  Html.program
    { init = init
    , view = Draw.View.view
    , update = Draw.State.update
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
