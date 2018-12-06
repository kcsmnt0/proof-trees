module Proof exposing (..)

import List exposing (head, tail, take, drop)
import Util exposing (..)

import Prop exposing (..)


{-| A `Rule` is a list of premises (above the line) and a conclusion (below the line). The set of inference rules of a theory is defined as a `List` of `Rule`s.
-}
type alias Rule c =
  { premises : List (Prop c)
  , conclusion : Prop c
  }


{-| A proof is either an unsatisfied premise (`Leaf`) or an application of a rule to a conclusion and some subproofs. The `Name` argument to a `Branch` is the name of the rule being applied at that step.
-}
type Proof c
  = Leaf (Prop c)
  | Branch Name (Prop c) (List (Proof c))


{-| An `Index` identifies a subproof of some proof. `[]` is the whole proof, and `i :: j` is premise `i` (read left-to-right) of the subproof identified by `j`.
-}
type alias Index = List Int


{-| An axiom is a rule with no premises.
-}
axiom : Prop c -> Rule c
axiom p = { premises = [], conclusion = p }


{-| The free variables in a proof are all of the free variables in the propositions that occur in it.
-}
free : Proof c -> List Name
free proof =
  case proof of
    Leaf conclusion -> Prop.free conclusion

    Branch rule conclusion premises ->
      Prop.free conclusion ++
        List.concatMap free premises


{- Substitute a proposition for every occurrence of a free variable in a proof.
-}
subst : Name -> Prop c -> Proof c -> Proof c
subst name prop proof =
  case proof of
    Leaf conclusion ->
      Leaf (Prop.subst name prop conclusion)

    Branch rule conclusion premises ->
      Branch rule
        (Prop.subst name prop conclusion)
        (List.map (subst name prop) premises)


{-| Make a `Proof` out of a `Rule`. It's sometimes helpful for code reuse to treat axioms and inference rules as `Proof`s.
-}
ruleProof : Name -> Rule c -> Proof c
ruleProof name rule =
  Branch name rule.conclusion (List.map Leaf rule.premises)


{-| Unify the conclusion of a rule with a given proposition, then apply the resulting substitutions to the premises of the rule.
-}
stepProp : Rule c -> Prop c -> Maybe (List (Prop c))
stepProp r p =
  unify r.conclusion p |>
    Maybe.map (\s -> List.map (Prop.substs s) r.premises)


{-| Apply a `Rule` to a proposition with `stepProp` and create a `Proof` with the resulting premises. This is different from `ruleProof` in that it unifies the conclusion of the rule so that the constructed `Proof` is for the given `Prop`, not the `Rule` itself.
-}
startProof : Name -> Rule c -> Prop c -> Maybe (Proof c)
startProof name rule conclusion =
  Maybe.map (Branch name conclusion << List.map Leaf) <|
    stepProp rule conclusion


{-| Apply a rule to some proposition in a proof, replacing whatever subproof might be there with a fresh one with all premises unsatisfied.
-}
stepProof : Index -> Name -> Rule c -> Proof c -> Maybe (Proof c)
stepProof index name rule proof =
  case (index, proof) of
    ([], Leaf conclusion) -> startProof name rule conclusion
    ([], Branch _ conclusion _) -> startProof name rule conclusion
    (i :: j, Branch r q ps) ->
      lookup i ps |>
        Maybe.andThen (\p ->
          stepProof j name rule p |>
            Maybe.map (Branch r q << replace i ps))
    _ -> Nothing


{-| Remove a subproof from a proof, leaving an unsatisfied premise.
-}
trimProof : Index -> Proof c -> Maybe (Proof c)
trimProof index proof =
  case (index, proof) of
    ([], Branch _ conclusion _) -> Just (Leaf conclusion)
    (i :: j, Branch r q ps) ->
      lookup i ps |>
        Maybe.andThen (\p ->
          trimProof j p |>
            Maybe.map
              (Branch r q << replace i ps))
    _ -> Nothing
