module ProofEditor.Update exposing (..)

import Dict exposing (..)

import Prop exposing (..)
import Prop.Syntax as Prop exposing (..)
import Prop.View as Prop exposing (..)

import Proof exposing (..)

import ProofEditor.Model exposing (..)


{-| - `SetRule` applies an inference rule to the proposition at some index in a proof, replacing whatever subproof might have been there with a new proof with all premises unsatisfied.
    - `UpdateVariable` sets the contents of the text inputs for a free variable (they all stay in sync).
    - `GiveVariable` parses the contents of the text inputs for a free variable and substitutes the given proposition for that variable in the proof if parsing succeeds.
-}
type Msg
  = SetRule Proof.Index (Maybe String)
  | UpdateVariable Name String
  | GiveVariable Name

initialVarDict : List String -> Dict String String
initialVarDict = Dict.fromList << List.map (\x -> (x, ""))

update : Msg -> Model c b Msg -> Model c b Msg
update msg model =
  Maybe.withDefault model <|
    case msg of
      SetRule i axiom ->
        case axiom of
          Nothing ->
            trimProof i model.proof |>
              Maybe.map (\p ->
                { model
                | proof = p
                })

          Just ax ->
            Dict.get ax model.axioms |>
              Maybe.andThen (\ax1 ->
                stepProof i ax ax1 model.proof |>
                  Maybe.map (\p ->
                    { model
                    | proof = p
                    , variableInputs = initialVarDict (Proof.free p)
                    }))

      UpdateVariable name text ->
        if Dict.member name model.variableInputs then
          Just
            { model
            | variableInputs = Dict.insert name text model.variableInputs
            }
        else
          Nothing

      GiveVariable name ->
        Dict.get name model.variableInputs |>
          Maybe.andThen (\input ->
            parseProp model.syntax input |>
              Maybe.map (\p ->
                { model
                | proof = Proof.subst name p model.proof
                , variableInputs = Dict.remove name model.variableInputs
                }))
