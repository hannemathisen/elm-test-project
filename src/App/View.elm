module App.View exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import App.Types exposing (..)
import DrawErase.View
import DrawOld.View
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
          div []
            [ Html.map ZoomMsg (Zoom.View.view model.zoom) ]
        DrawErase ->
          div [] [ Html.map DrawEraseMsg (DrawErase.View.view model.drawErase) ]
        DrawOld ->
          div [] [ Html.map DrawOldMsg (DrawOld.View.view model.drawOld)  ]
      ]
    ]


viewModeMenu : Html Msg
viewModeMenu =
  div []
    [ div []
      [ button [ class "btn", onClick (ChangeMode DrawErase) ] [ text "Draw 1" ]
      , button [ class "btn", onClick (ChangeMode DrawOld) ] [ text "Draw 2"]
      -- , button [ class "btn", onClick (ChangeMode Zoom) ] [ text "Zoom" ]
      ]
    , div []
      [ p []
        [ text "After you have tried both drawing functions, please answer this "
        , a [ href "https://goo.gl/forms/ixHuDnDevcuBH2LB3" ] [ text "survey." ]
        ]
      ]
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
