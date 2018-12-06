module Prop.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Util exposing (..)

import Prop exposing (..)


{-| A UI component defines a `VarView` to describe how to render a propositional variable. Different components deal with variables in different ways - for example the proof editor displays them as interactive input boxes while the proof renderer displays them as italicized labels.
-}
type alias VarView msg = Name -> Html msg


{-| A base theory defines a `ConView` to describe how to combine a connective and HTML views for each of its arguments into a larger view. This allows base theories to define their own display rules for connectives while leaving the rendering of propositional variables up to the UI components.
-}
type alias ConView c msg = c -> List (Html msg) -> Html msg


view : VarView msg -> ConView c msg -> Prop c -> Html msg
view varView conView prop =
  span
    [ monospace
    ]
    [ case prop of
        (Var x) -> varView x
        (Con c ps) -> conView c (List.map (view varView conView) ps)
    ]
