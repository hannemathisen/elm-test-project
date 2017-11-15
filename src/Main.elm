module Main exposing (..)


import App.State exposing (..)
import App.View exposing (..)
import Html exposing (..)


main =
  Html.program
    { init = App.State.init
    , view = App.View.root
    , update = App.State.update
    , subscriptions = always Sub.none
    }
