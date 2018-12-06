module Arboretum.Model exposing (..)

{-| This is boilerplate to define a model that contains enough information to generate the models of every UI component in the app. The `Lens` type is defined in `Util`.
-}

import Dict exposing (..)
import Prop exposing (..)
import Proof exposing (..)
import Syntax exposing (..)
import Util exposing (..)

import Arboretum.Theory.IPL as IPL

import GoalEditor.Model as GoalEditor

import ProofEditor.Model as ProofEditor
import ProofEditor.Update as ProofEditor

import ProofRenderer.Model as ProofRenderer


type alias Model =
  { variableInputs : Dict Name String
  , goalInput : String
  , proof : Proof IPL.Con
  }


goalEditorLens : Lens Model (GoalEditor.Model IPL.Con IPL.Bracket)
goalEditorLens =
  { get =
      \model ->
        { syntax = IPL.syntax
        , proof = model.proof
        , variableInputs = model.variableInputs
        , goalInput = model.goalInput
        }

  , set =
      \model goalEditorModel ->
        { model
        | proof = goalEditorModel.proof
        , variableInputs = goalEditorModel.variableInputs
        , goalInput = goalEditorModel.goalInput
        }
  }


proofEditorLens : Lens Model (ProofEditor.Model IPL.Con IPL.Bracket ProofEditor.Msg)
proofEditorLens =
  { get =
      \model ->
        { axioms = IPL.axioms
        , syntax = IPL.syntax
        , conView = IPL.conView
        , proof = model.proof
        , variableInputs = model.variableInputs
        }

  , set =
      \model proofEditorModel ->
        { model
        | proof = proofEditorModel.proof
        , variableInputs = proofEditorModel.variableInputs
        }
  }


axiomModel : Name -> Rule IPL.Con -> ProofRenderer.Model IPL.Con msg
axiomModel name rule =
  { conView = IPL.conView
  , proof = ruleProof name rule
  }
