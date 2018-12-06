module ProofEditor.View exposing (..)

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Util exposing (..)

import Proof exposing (..)
import Proof.Model as Proof
import Proof.View as Proof exposing (..)

import Prop exposing (..)
import Prop.View as Prop exposing (..)

import ProofEditor.Model as ProofEditor exposing (..)
import ProofEditor.Update as ProofEditor exposing (..)


view : Model c b Msg -> Html Msg
view = Proof.view << proofModel


{-| The text input for a free variable.
-}
proofVarView : Dict Name String -> VarView Msg
proofVarView variableInputs x =
  let
    varInput =
      Dict.get x variableInputs |>
        Maybe.withDefault "programming error!!! oh no!"
  in
    input
      [ type_ "text"
      , value varInput
      , style "width" "1em"
      , wideEnoughFor varInput
      , onInput (UpdateVariable x)
      , onKeyDown enterKey (GiveVariable x)
      , title x
      ]
      []


{-| The dropdown next to each rule for selecting an axiom (inference rule) to apply at that index in the proof.
-}
axiomSelect :
  { dropdownAxioms : Dict Name (Rule c)
  , index : Proof.Index
  , selection : Maybe Name
  , greyed : Bool
  } ->
  Html Msg
axiomSelect config =
  select
    [ align "center"
    , disabled config.greyed
    , onSelect <| \text ->
        SetRule config.index <|
          if text == "" then
            Nothing
          else
            Just text
    ]
    (option [selected (config.selection == Nothing)] [] ::
      (config.dropdownAxioms |>
        Dict.toList |>
          List.map (\(name, _) ->
            option
              [ selected (config.selection == Just name) ]
              [ text name ])))


proofModel : Model c b Msg -> Proof.Model c Msg
proofModel model =
  { conView = model.conView
  , varView = proofVarView model.variableInputs
  , labelView =
    { leaf =
      \index premise ->
        axiomSelect
          { dropdownAxioms = model.axioms
          , index = index
          , selection = Nothing
          , greyed = open premise
          }
    , branch =
      \index name conclusion ->
        axiomSelect
          { dropdownAxioms = model.axioms
          , index = index
          , selection = Just name
          , greyed = open conclusion
          }
    }
  , proof = model.proof
  }
