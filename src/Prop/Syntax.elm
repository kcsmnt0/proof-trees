module Prop.Syntax exposing (..)

{-| Base theories define their own syntax using the constructs in the Syntax module tree; this module extends the syntax of an arbitrary theory to include propositional variables.
-}

import Util exposing (..)

import Prop exposing (..)

import Syntax exposing (..)
import Syntax.Lexing as Lexing exposing (LexAction(..), mapLexAction, lexToEnd)
import Syntax.Parsing as Parsing exposing (Parser, Sort(..), parseToEnd, prefixify, repeat)


{-| A token in the syntax of a proposition is either a propositional varible or a token in the syntax of the base theory.
-}
type PropToken token = PropVar Name | Token token


{-| Extend the lexical syntax of a base theory with syntax for propositional variables. A variable is written as a question mark followed branch a name (one or more word characters).
-}
propLexSyntax : Lexing.Syntax token -> Lexing.Syntax (PropToken token)
propLexSyntax lstx =
  varTokenType ::
    (lstx |>
      List.map (\tokenType ->
        { regex = tokenType.regex
        , toToken = Maybe.map (mapLexAction Token) << tokenType.toToken
        }))


varTokenType : Lexing.TokenType (PropToken token)
varTokenType =
  { regex = uncheckedRegex "\\?\\w+"
  , toToken = Just << Keep << PropVar
  }


{-| Extend the parsing syntax of a base theory with syntax for propositional variables. For the purposes of parsing, a variable is treated syntactically as an atomic proposition.
-}
propParseSyntax :
  Parsing.Syntax token bracket ->
  Parsing.Syntax (PropToken token) bracket
propParseSyntax pstx token =
  case token of
    PropVar x -> Const
    Token t -> pstx t


propParser :
  Parsing.Syntax token bracket ->
  Parser (PropToken token) (Prop token)
propParser pstx input =
  case input of
    [] -> Nothing

    token :: ts ->
      case token of
        PropVar x -> Just (ts, Var x)
        Token t ->
          case pstx t of
            Const ->
              Just (ts, Con t [])

            Op _ _ ->
              propParser pstx ts |>
                Maybe.andThen (\(ts1, p) ->
                  propParser pstx ts1 |>
                    Maybe.map (\(ts2, q) ->
                      (ts2, Con t [p, q])))

            Func arity ->
              repeat arity (propParser pstx) ts |>
                Maybe.map (\(ts1, ps) -> (ts1, Con t ps))

            _ ->
              Nothing


{-| Try to parse an entire string to a proposition, using the syntax of the base theory extended with propositional variables.
-}
parseProp :
  Syntax token bracket ->
  String ->
  Maybe (Prop token)
parseProp syntax input =
  lexToEnd (propLexSyntax syntax.lexing) input |>
    Maybe.andThen (\tokens ->
      prefixify (propParseSyntax syntax.parsing) tokens |>
        Maybe.andThen (\tokens1 ->
          parseToEnd (propParser syntax.parsing) tokens1))
