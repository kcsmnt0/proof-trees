module Syntax.Parsing exposing (..)

import List exposing (..)
import Util exposing (..)

import Syntax.Lexing as Lexing


{-| A `Parser` consumes some `token`s from a `List` and returns a value and the suffix of the list, or fails.
-}
type alias Parser token a = List token -> Maybe (List token, a)


{-| Run a `Parser` in sequence a fixed number of times.
-}
repeat : Int -> Parser token a -> Parser token (List a)
repeat n parse tokens =
  if n == 0 then
    Just (tokens, [])
  else
    parse tokens |>
      Maybe.andThen (\(ts1, x) ->
        repeat (n - 1) parse ts1 |>
          Maybe.map (\(ts2, xs) ->
            (ts2, x :: xs)))


{-| Parse an entire `List`, returning `Nothing` if there are any leftover `token`s.
-}
parseToEnd : Parser token a -> List token -> Maybe a
parseToEnd parser input =
  case parser input of
    Just ([], output) -> Just output
    _ -> Nothing


type alias Syntax token bracket = token -> Sort bracket


{-| Higher precedence operators bind tighter than lower precedence ones.
-}
type alias Precedence = Int


type Associativity = Left | Right


{-| The parsing syntax for a theory is defined branch classifying each connective as a constant, infix operator, prefix function, or bracket/parenthesis. Equality is defined over every type in Elm, but only actually works for some - `bracket` should usually be a simple enum type.
-}
type Sort bracket
  = Const
  | Op Precedence Associativity
  | Func Int
  | Open bracket
  | Close bracket


{-| Convert an expression with operators written in infix notation to the equivalent expression in prefix notation.
-}
prefixify : Syntax token bracket -> List token -> Maybe (List token)
prefixify sort tokens = shunt sort (List.reverse tokens) [] []


{-| A tail-recursive implementation of the shunting yard algorithm.
-}
shunt :
  Syntax token bracket ->
  List token ->
  List token ->
  List token ->
  Maybe (List token)
shunt sort tokens operators output =
  case tokens of
    [] ->
      let
        pop ops out =
          case ops of
            [] -> Just (ops, out)
            o :: os ->
              case sort o of
                Open _ -> Nothing
                Close _ -> Nothing
                _ -> pop os (o :: out)
      in
        Maybe.map Tuple.second (pop operators output)

    t :: ts ->
      case sort t of
        Const ->
          shunt sort ts operators (t :: output)

        Func _ ->
          shunt sort ts (t :: operators) output

        Op prec assoc ->
          let
            keepPopping tok =
              case sort tok of
                Func _ -> True
                Op otherPrec otherAssoc ->
                  otherPrec > prec ||
                    (otherPrec == prec && otherAssoc == Right)
                Open _ -> True
                _ -> False

            pop ops out =
              case ops of
                [] -> (ops, out)
                o :: os ->
                  if keepPopping o then
                    pop os (o :: out)
                  else
                    (ops, out)

            (ops1, out1) = pop operators output
          in
            shunt sort ts (t :: ops1) out1

        Close bracket ->
          shunt sort ts (t :: operators) output

        Open bracket ->
          let
            pop ops out =
              case ops of
                [] -> Nothing
                o :: os ->
                  case sort o of
                    Close otherBracket ->
                      if bracket == otherBracket then
                        Just (os, out)
                      else
                        pop os (o :: out)
                    _ -> pop os (o :: out)
          in
            pop operators output |>
              Maybe.andThen (uncurry (shunt sort ts))
