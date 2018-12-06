module Proof.Model exposing (..)

import Dict exposing (..)
import Html exposing (..)

import Proof exposing (..)

import Prop exposing (..)
import Prop.View as Prop exposing (..)


{-| The data model for the proof UI component.
    - `conView` comes from the base theory and defines how to render connectives.
    - `varView` specifies how to render free variables.
    - `labelView` specifies how to render the rule labels to the left of horizontal lines in the proof.
-}
type alias Model c msg =
  { conView : Prop.ConView c msg
  , varView : VarView msg
  , labelView :
    { leaf : Proof.Index -> Prop c -> Html msg
    , branch : Proof.Index -> Name -> Prop c -> Html msg
    }
  , proof : Proof c
  }
