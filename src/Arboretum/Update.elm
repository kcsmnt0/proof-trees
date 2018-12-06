module Arboretum.Update exposing (..)

{-| This is boilerplate to define an update function that can forward messages to the appropriate UI component.
-}

import Prop exposing (..)
import Proof exposing (..)
import Util exposing (..)

import Arboretum.Model exposing (..)

import GoalEditor.Model as GoalEditor
import GoalEditor.Update as GoalEditor

import ProofEditor.Model as ProofEditor
import ProofEditor.Update as ProofEditor

import ProofRenderer.Model as ProofRenderer


type Msg
  = GoalEditor GoalEditor.Msg
  | ProofEditor ProofEditor.Msg


update : Msg -> Model -> Model
update msg model =
  case msg of
    GoalEditor m -> modify goalEditorLens (GoalEditor.update m) model
    ProofEditor m -> modify proofEditorLens (ProofEditor.update m) model
