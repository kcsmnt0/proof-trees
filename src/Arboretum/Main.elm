module Arboretum.Main exposing (..)

{-| An interactive proof editor for intuitionistic propositional logic.
-}

import Browser
import Dict
import Proof exposing (..)

import Arboretum.Model exposing (..)
import Arboretum.Update exposing (..)
import Arboretum.View exposing (..)

import Arboretum.Theory.IPL exposing (..)


init : Model
init =
  { variableInputs = Dict.empty
  , proof = Leaf (atom "")
  , goalInput = ""
  }


main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }
