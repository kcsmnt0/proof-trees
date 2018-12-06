module Proof.View exposing (..)

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Util exposing (..)

import Prop exposing (..)
import Prop.View as Prop exposing (..)

import Proof exposing (..)
import Proof.Model as Proof exposing (..)


view : Model c msg -> Html msg
view model = viewAt [] model model.proof


{-| Render some subproof of a given `Proof`.
-}
viewAt :
  Proof.Index ->
  Model c msg ->
  Proof c ->
  Html msg
viewAt index model proof =
  case proof of
    Leaf premise ->
      div
        [align "center"]
        [ table
          [ style "padding-left" "0.75em"
          , style "padding-right" "0.75em"
          ]
          [ tr []
            [ td [style "vertical-align" "bottom"] [model.labelView.leaf index premise]
            , td [] [Prop.view model.varView model.conView premise]
            ]
          ]
        ]

    Branch name conclusion premises ->
      let premiseCount = List.length premises in
        table
          [ style "padding-left" "0.75em"
          , style "padding-right" "0.75em"
          ]
          [ tr []
            [ td
              [ style "vertical-align" "bottom"
              ]
              [ model.labelView.branch index name conclusion
              ]
            , td []
              [ table []
                [ tr [] <|
                    if premises == [] then
                      [td [] [div [style "width" "1.5em"] []]]
                    else
                      List.map
                        (\(j, p) ->
                          td
                            [style "vertical-align" "bottom"]
                            [viewAt (index ++ [j]) model p])
                        (indexed premises)
                , tr []
                  [ td
                    [ align "center"
                    , colspan premiseCount
                    , style "border-top" "1px solid black"
                    ]
                    [Prop.view model.varView model.conView conclusion]
                  ]
                ]
              ]
            ]
          ]
