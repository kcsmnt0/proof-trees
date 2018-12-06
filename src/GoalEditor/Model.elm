module GoalEditor.Model exposing (..)

import Dict exposing (..)

import Syntax exposing (..)

import Prop exposing (..)
import Prop.View as Prop exposing (..)

import Proof exposing (..)


{-| - `syntax` comes from the base theory.
    - `proof` is the proof being edited
    - `variableInputs` and `goalInput` hold the states of `<input>` elements in the view.
-}
type alias Model c b =
  { syntax : Syntax c b
  , proof : Proof c
  , variableInputs : Dict Name String
  , goalInput : String
  }
