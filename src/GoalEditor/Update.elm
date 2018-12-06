module GoalEditor.Update exposing (..)

import Dict exposing (..)
import Util exposing (..)

import GoalEditor.Model exposing (..)

import Prop exposing (..)
import Prop.Syntax as Prop exposing (..)

import Proof exposing (..)


{-| - `SetGoalInput` sets the contents of the goal `<input>` field.
    - `UpdateGoal` parses the contents of the goal `<input>` field as a proposition and sets the current proof to be a new proof with the proposition as the conclusion.
-}
type Msg
  = SetGoalInput String
  | UpdateGoal

update : Msg -> Model c b -> Model c b
update msg model =
  Maybe.withDefault model <|
    case msg of
      SetGoalInput goal ->
        Just { model | goalInput = goal }

      UpdateGoal ->
        parseProp model.syntax model.goalInput |>
          Maybe.map (\p ->
            { model
            | proof = Leaf p
            , variableInputs = initialDict (Prop.free p) ""
            , goalInput = ""
            })
