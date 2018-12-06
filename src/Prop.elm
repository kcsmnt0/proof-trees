module Prop exposing (..)

import Util exposing (..)

import Syntax exposing (..)
import Syntax.Lexing as Lexing exposing (..)
import Syntax.Parsing as Parsing exposing (..)


type alias Name = String


{-| A `Prop` is either a variable or a connective applied to some `Prop`s. It represents the type of (not necessarily well-formed) expressions in some base theory. The `c` parameter is the type of connectives in the base theory.
-}
type Prop c
  = Var Name
  | Con c (List (Prop c))


{-| A convenience function for building propositions with nullary connectives (constants).
-}
nullary : c -> Prop c
nullary con = Con con []


{-| A convenience function for building propositions with unary connectives.
-}
unary : c -> Prop c -> Prop c
unary con p = Con con [p]


{-| A convenience function for building propositons with binary connectives.
-}
binary : c -> Prop c -> Prop c -> Prop c
binary con p1 p2 = Con con [p1,p2]


{-| The list of free variables in a proposition. Right now this is just the list of all variables, since there are no binders.
-}
free : Prop c -> List Name
free prop =
  case prop of
    Var x -> [x]
    Con c ps -> List.concatMap free ps


{-| A proposition is closed if it has no free variables.
-}
closed : Prop c -> Bool
closed prop = free prop == []


{-| A proposition is open if it has any free variables.
-}
open : Prop c -> Bool
open = not << closed


{-| Substitute every occurrence of a name in the second `Prop` with the first `Prop`. In traditional formal syntax:

    `subst x p1 p2` = p2[p1/x]
-}
subst : Name -> Prop c -> Prop c -> Prop c
subst x p1 p2 =
  case p2 of
    Var y -> if x == y then p1 else Var y
    Con c ps -> Con c (List.map (subst x p1) ps)


{-| Apply a list of substitutions in sequence.
-}
substs : List (Name, Prop c) -> Prop c -> Prop c
substs s p = List.foldl (uncurry subst) p s


{-| Attempt to unify a list of equalities given a partial assignment of variables, returning a list of variable assignments in the success case.
-}
unifyCtx : List (Prop c, Prop c) -> List (Name, Prop c) -> Maybe (List (Name, Prop c))
unifyCtx props theta =
  case props of
    [] -> Just theta
    ((p1,p2) :: ps) ->
      if p1 == p2 then
        unifyCtx ps theta
      else
        case (p1, p2) of
          (Con c qs, Con d rs) ->
            if c == d then
              unifyCtx (zip qs rs ++ ps) theta
            else
              Nothing
          (Var x, p) ->
            let
              occurs = elem x (free p)
              solved = List.any (Tuple.first >> ((==) x)) theta
            in
              if occurs || solved then
                Nothing
              else
                unifyCtx
                  (List.map (diagMap (subst x p)) ps)
                  ((x, p) :: List.map (Tuple.mapSecond (subst x p)) theta)
          (p, Var x) -> unifyCtx ((Var x, p) :: ps) theta


{-| Attempt to unify two propositions, returning a list of variable assignments in the success case.
-}
unify : Prop c -> Prop c -> Maybe (List (Name, Prop c))
unify p1 p2 = unifyCtx [(p1, p2)] []
