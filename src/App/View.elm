module App.View exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import App.Types exposing (..)
import Draw.View


root : Model -> Html Msg
root model =
  div []
    [ viewHeader model
    , div []
      [ case model.globalMode of
        Main ->
          viewModeMenu
        Zoom ->
          viewModeMenu
        Draw ->
          div [] [ Html.map DrawMsg (Draw.View.view model.draw) ]
      ]
    ]


viewModeMenu : Html Msg
viewModeMenu =
  -- div []
  --     [ h4
  --         [ class "center-align " ]
  --         [ span [] [ text "C" ]
  --         , span [] [ text "h" ]
  --         , span [] [ text "o" ]
  --         , span [] [ text "o" ]
  --         , span [] [ text "s" ]
  --         , span [] [ text "e" ]
  --         , span [] [ text " " ]
  --         , span [] [ text "g" ]
  --         , span [] [ text "a" ]
  --         , span [] [ text "m" ]
  --         , span [] [ text "e" ]
  --         , span [] [ text " " ]
  --         , span [] [ text "m" ]
  --         , span [] [ text "o" ]
  --         , span [] [ text "d" ]
  --         , span [] [ text "e" ]
  --         , span [] [ text "!" ]
  --         ]
  --     , hr [] []
  --     , div [ class "row" ] []
  --     , div [ class "row" ] []
  --     , div [ class "card hoverable main-button-color add-pointer", onClick (ChangeMode Draw) ]
  --         [ div [ class "card-content white-text" ] [ div [ class "card-title" ] [ p [ class "center-align" ] [ text "Draw" ] ] ] ]
  --     , div [ class "card hoverable main-button-color add-pointer", onClick (ChangeMode Zoom) ]
  --         [ div [ class "card-content white-text" ] [ div [ class "card-title" ] [ p [ class "center-align" ] [ text "Zoom" ] ] ] ]
  --     ]
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
