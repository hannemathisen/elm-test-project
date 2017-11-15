module App.View exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import App.Types exposing (..)
import Draw.View
import Zoom.View


root : Model -> Html Msg
root model =
  div []
    [ viewHeader model
    , div []
      [ case model.globalMode of
        Main ->
          viewModeMenu
        Zoom ->
          div [] [ Html.map ZoomMsg (Zoom.View.view model.zoom) ]
        Draw ->
          div [] [ Html.map DrawMsg (Draw.View.view model.draw) ]
      ]
    ]


viewModeMenu : Html Msg
viewModeMenu =
  div []
    [ button [ class "btn", onClick (ChangeMode Draw) ] [ text "Draw" ]
    , button [ class "btn", onClick (ChangeMode Zoom) ] [ text "Zoom" ]
    ]


viewHeader : Model -> Html Msg
viewHeader model =
    header []
        [ nav []
            [ div []
              [ case model.globalMode of
                Main ->
                  text "MIIG test"
                _ ->
                  button [ class "btn", onClick (ChangeMode Main) ] [ text "Back to Menu" ]
              ]

            ]
        ]
