module ProofEditor.Model exposing (..)

import Dict exposing (..)
import Syntax exposing (..)

import Prop exposing (..)
import Prop.View as Prop exposing (..)

import Proof exposing (..)
import Proof.Model as Proof


{-| - `variableInputs` holds the state of the `<input>` elements for the free variables in the proof.
    - `axioms` comes from the base theory and defines the set of inference rules listed in the axiom dropdown next to each rule in the proof.
    - `syntax` and `conView` come from the base theory and define how to parse and render propositions.
    - `proof` is the proof being edited.
-}
type alias Model c b msg =
  { variableInputs : Dict Name String
  , axioms : Dict Name (Rule c)
  , syntax : Syntax c b
  , conView : Prop.ConView c msg
  , proof : Proof c
  }
