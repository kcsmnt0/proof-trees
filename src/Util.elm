module Util exposing (..)

{-| Utility functions that don't really belong anywhere else but aren't serious enough to pull in a dependency or create a module structure for.
-}

import Dict exposing (..)
import List exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Regex exposing (..)


{-| A `Lens` is a pair of a getter and setter, sometimes useful for defining "views" on complicated record types.
-}
type alias Lens a b =
  { get : a -> b
  , set : a -> b -> a
  }


{-| Apply a function to the target of some lens in a "larger" structure.
-}
modify : Lens a b -> (b -> b) -> a -> a
modify lens f x = lens.set x (f (lens.get x))


curry : ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)


uncurry : (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y


elem : a -> List a -> Bool
elem x = List.foldl (\a -> (||) (a == x)) False


diagMap : (a -> b) -> (a, a) -> (b, b)
diagMap f = Tuple.mapBoth f f


zip : List a -> List b -> List (a, b)
zip = List.map2 Tuple.pair


{-| Pair every element of a list with its index in the list, starting from 0.
-}
indexed : List a -> List (Int, a)
indexed xs = zip (range 0 (length xs - 1)) xs


lookup : Int -> List a -> Maybe a
lookup i = head << drop i


replace : Int -> List a -> a -> List a
replace i xs x = take i xs ++ x :: drop (i+1) xs


{-| Create a dictionary with some keys all initialized to the same default value.
-}
initialDict : List comparable -> a -> Dict comparable a
initialDict keys val = Dict.fromList (List.map (\k -> (k, val)) keys)


mapM2 : (a -> b -> Maybe c) -> List a -> List b -> Maybe (List c)
mapM2 f xs ys =
  case (xs, ys) of
    ([], _) -> Just []
    (_, []) -> Just []
    ((x::xs1), (y::ys1)) ->
      f x y |> Maybe.andThen (\z ->
      mapM2 f xs1 ys1 |> Maybe.map (\zs ->
      z :: zs))


assocKeyOf : v -> List (k, v) -> Maybe k
assocKeyOf val list =
  case list of
    [] -> Nothing
    (key, val1) :: kvs ->
      if val == val1 then
        Just key
      else
        assocKeyOf val kvs


{-| Search for the key of a value in a dictionary.
-}
keyOf : v -> Dict k v -> Maybe k
keyOf val = assocKeyOf val << Dict.toList


onSelect : (String -> msg) -> Attribute msg
onSelect f = on "change" (Json.map f targetValue)


onKeyDown : Int -> msg -> Attribute msg
onKeyDown key msg =
  on "keydown"
    (keyCode |>
      Json.andThen (\kc ->
        if kc == key then
          Json.succeed msg
        else
          Json.fail "not the right key"))


enterKey : Int
enterKey = 13


monospace : Html.Attribute msg
monospace = style "font-family" "monospace"


{-| Set the width of an element to be the size of some text in *monospace* font.
-}
wideEnoughFor : String -> Html.Attribute msg
wideEnoughFor text =
  style "min-width"
    (String.fromInt (String.length text) ++ "ch")


{-| An input box that sizes to the given string. Only works with monospace font.
-}
growableInput : String -> List (Html.Attribute msg) -> Html msg
growableInput text attrs =
  input
    (attrs ++
      [ type_ "input"
      , value text
      , wideEnoughFor text
      ])
  []


{-| I've checked my static regexes, I can vouch for them.
-}
uncheckedRegex : String -> Regex
uncheckedRegex = fromString >> Maybe.withDefault Regex.never
