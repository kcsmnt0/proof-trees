module Syntax.Lexing exposing (..)

import Regex exposing (..)


type alias Syntax token = List (TokenType token)


{-| A `LexAction` tells the lexer what to do with a lexeme it just matched - `Drop` says to throw it away (for e.g. whitespace and comments) and `Keep` says to keep the corresponding token in the output token list.
-}
type LexAction token = Drop | Keep token


{-| A token type is modeled as a `Regex` paired with a function that transforms a matching `String` into a token. The function can do additional validation after lexing, for e.g. the case of matching integer literals within a certain fixed range - a return value of `Nothing` indicates that the post-lexing validation failed.
-}
type alias TokenType token =
  { regex : Regex
  , toToken : String -> Maybe (LexAction token)
  }


mapLexAction : (a -> b) -> LexAction a -> LexAction b
mapLexAction f action =
  case action of
    Drop -> Drop
    Keep t -> Keep (f t)


{-| `List.drop` for `String`.
-}
stringDrop : Int -> String -> String
stringDrop n = String.fromList << List.drop n << String.toList


{-| Split a string into a prefix matching some regex, and the leftover suffix. Returns `Nothing` if the regex fails to match any prefix.
-}
splitAfter : Regex -> String -> Maybe (String, String)
splitAfter regex string =
  List.head (findAtMost 1 regex string) |>
    Maybe.andThen (\result ->
      if result.index == 0 then
        Just (result.match, stringDrop (String.length result.match) string)
      else
        Nothing)


{-| Try to lex one token from the start of a string, returning the token and the rest of the string after the token.
-}
lexOne : Syntax token -> String -> Maybe (String, LexAction token)
lexOne syntax string =
  case syntax of
    [] -> Nothing
    tokenType :: tokenTypes ->
      let
        result =
          splitAfter tokenType.regex string |>
            Maybe.andThen (\(prefix, suffix) ->
              tokenType.toToken prefix |>
                Maybe.map (Tuple.pair suffix))
      in
        case result of
          Just result1 -> Just result1
          Nothing -> lexOne tokenTypes string


{-| Lex some prefix of a string, stopping when no prefix matches any token type.
-}
lex : Syntax token -> String -> (String, List token)
lex syntax string =
  case lexOne syntax string of
    Nothing -> ("", [])
    Just (suffix1, action) ->
      let (suffix2, tokens) = lex syntax suffix1 in
        (suffix2,
          case action of
            Drop -> tokens
            Keep token -> token :: tokens)


{-| Lex an entire `String`, returning `Nothing` if there are any leftover characters.
-}
lexToEnd : Syntax token -> String -> Maybe (List token)
lexToEnd syntax string =
  let (suffix, tokens) = lex syntax string in
    if suffix == "" then
      Just tokens
    else
      Nothing
