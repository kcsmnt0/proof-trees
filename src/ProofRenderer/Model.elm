module ProofRenderer.Model exposing (..)

import Proof exposing (..)

import Prop exposing (..)
import Prop.View as Prop exposing (..)


{-| - `conView` comes from the base theory and defines how to render connectives.
    - `proof` is the proof being rendered.
-}
type alias Model c msg =
  { conView : ConView c msg
  , proof : Proof c
  }
