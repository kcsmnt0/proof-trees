module GoalEditor.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Util exposing (..)

import Prop exposing (..)

import GoalEditor.Model exposing (..)
import GoalEditor.Update exposing (..)


view : Model c b -> Html Msg
view model =
  div
    [ style "margin-bottom" "3em"
    ]
    [ goalInput model.goalInput
    , updateGoalButton
    ]


{-| A text input element for entering a new goal.
-}
goalInput : String -> Html Msg
goalInput goalText =
  let
    placeholder = "type goal here"
  in
    growableInput goalText
      [ monospace
      , attribute "placeholder" placeholder
      , wideEnoughFor placeholder
      , onInput SetGoalInput
      , onKeyDown enterKey UpdateGoal
      ]


{-| A button element for setting a new goal from the goal text input.
-}
updateGoalButton : Html Msg
updateGoalButton =
  let
    buttonText = "set goal"
  in
    button
      [ onClick UpdateGoal
      , wideEnoughFor buttonText
      , monospace
      ]
      [ text buttonText
      ]


{-| A button element for setting a fixed goal.
-}
setGoalInputButton : String -> Html Msg
setGoalInputButton input = button [onClick (SetGoalInput input)] [text input]
