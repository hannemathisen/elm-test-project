module Main exposing (..)

import Types exposing (..)
import State exposing (..)
import View exposing (..)
import Html exposing (..)


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
