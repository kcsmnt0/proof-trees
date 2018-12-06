module ProofRenderer.View exposing (..)

import Html exposing (..)

import Prop exposing (..)
import Prop.View as Prop

import Proof exposing (..)
import Proof.Model as Proof
import Proof.View as Proof

import ProofRenderer.Model exposing (..)


view : Model c msg -> Html msg
view = Proof.view << proofModel


{-| Variables are italicized and uneditable.
-}
varView : Prop.VarView msg
varView name =
  Html.i
    []
    [text name]


{-| Labels are just text.
-}
proofModel : Model c msg -> Proof.Model c msg
proofModel model =
  { conView = model.conView
  , varView = varView
  , labelView =
    { leaf = \_ _ -> span [] []
    , branch = \_ name _ -> text name
    }
  , proof = model.proof
  }
