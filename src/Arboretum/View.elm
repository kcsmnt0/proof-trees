module Arboretum.View exposing (..)

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Util exposing (..)

import Arboretum.Model exposing (..)
import Arboretum.Update exposing (..)

import Arboretum.Theory.IPL as IPL exposing (..)

import ProofEditor.View as ProofEditor exposing (..)
import ProofRenderer.View as ProofRenderer exposing (..)
import GoalEditor.View as GoalEditor exposing (..)


view : Model -> Html Msg
view model =
  div
    [ align "center"
    , style "margin-left" "10%"
    , style "margin-right" "10%"
    ]
    [ goalEditor model
    , header
    , intro model
    , proofEditorInstructions
    , exampleGoals
    , syntax
    , axiomList
    ]


goalEditor : Model -> Html Msg
goalEditor model =
  div
    [ style "margin-bottom" "3em"
    , align "center"
    ]
    [ Html.map ProofEditor <|
        div
          [ style "overflow" "auto"
          , style "white-space" "nowrap"
          , style "font-family" "monospace"
          , style "margin-top" "3em"
          , style "margin-bottom" "3em"
          ]
          [ ProofEditor.view (proofEditorLens.get model) ]
    
    , Html.map GoalEditor <|
        GoalEditor.view (goalEditorLens.get model)
    ]


readmeSection : List (Html msg) -> Html msg
readmeSection = div [style "margin-bottom" "3em", align "left"]


header : Html msg
header =
  h1 [align "center"] [text "Arboretum"]


intro : Model -> Html Msg
intro model =
  readmeSection
    [ text "This is an environment for growing proof trees."
    , br [] [], br [] []
    , text "It currently supports intuitionistic propositional logic."
    , br [] [], br [] []
    , text "Type a proposition into "
    , Html.map GoalEditor <| goalInput model.goalInput
    , text " and click "
    , Html.map GoalEditor <| updateGoalButton
    , text " (or press Enter) to start a proof."
    ]


proofEditorInstructions : Html msg
proofEditorInstructions =
  readmeSection
    [ h2 [align "center"] [text "Growing proofs"]
    , text "Use the dropdown next to a proposition to choose an axiom to apply."
    , br [] []
    , text "(There's no error reporting yet - if you choose an axiom that doesn't work then nothing will happen.)"
    , br [] []
    , br [] []
    , text "You can undo applying an axiom branch selecting the blank space at the top of the dropdown list."
    , br [] []
    , br [] []
    , text "If you apply an axiom whose premises mention any unconstrained variables (ones that don't show up in the axiom's conclusion), an input box will show up for each one."
    , br [] []
    , br [] []
    , text "You'll have to give a value for each unconstrained variable before proceeding with the proof: type an expression into one of the input boxes and press Enter to give a value."
    , br [] []
    , br [] []
    , text "The same variable might occur in multiple different places in the proof, so typing in one input box might update the text of multiple input boxes."
    , br [] []
    , br [] []
    , text "A proof is finished when every branch ends with a horizontal line with nothing above it. In this theory, that means every branch must end with either \"Here\" or \"⊤-Intro\"."
    ]


exampleGoals : Html Msg
exampleGoals =
  Html.map GoalEditor <|
    readmeSection
      [ h2 [align "center"] [text "Example propositions"]
      , ul []
        [ li [] [setGoalInputButton "a, b, ε ⊢ a ∧ b"]
        , li [] [setGoalInputButton "nil |- (q | p) -> (p | q)"]
        , li [] [setGoalInputButton "anyContext entails (first and second implies third) implies (first implies second implies third)"]
        ]
      , br [] []
      ]


syntax : Html msg
syntax =
  readmeSection
    [ h2 [align "center"] [text "Syntax"]
    , text "There are a few equivalent spellings for each of the fancy logic symbols."
    , ul []
      [ li [] [text "\"∈\" = \"in\""]
      , li [] [text "\"⊢\" = \"|-\" = \"entails\""]
      , li [] [text "\"⊤\" = \"🔝\" = \"T\" = \"true\" = \"top\""]
      , li [] [text "\"⊥\" = \"_|_\" = \"F\" = \"false\" = \"bottom\""]
      , li [] [text "\"∧\" = \"/\\\" = \"&\" = \"&&\" = \"and\""]
      , li [] [text "\"∨\" = \"\\/\" = \"|\" = \"||\" = \"or\""]
      , li [] [text "\"→\" = \"->\" = \"implies\""]
      , li [] [text "\",\" = \"::\" = \";\" = \"cons\""]
      ]
    , text "\"Γ\" isn't special syntax - it's just a name. You can use any name in its place. A name is a letter (Greek letters included) followed branch any number of letters or digits."
    ]


axiomDiv : Html msg -> Html msg
axiomDiv axiomHtml =
  div
    [ style "margin-bottom" "3em"
    ]
    [ axiomHtml
    ]


axiomList : Html msg
axiomList =
  div []
    [ h2 [] [text "Inference rules"]

    , div [align "center"] <|
        List.map (axiomDiv << ProofRenderer.view << uncurry axiomModel) <|
          Dict.toList <|
            IPL.axioms
    ]
