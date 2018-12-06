module Arboretum.Theory.IPL exposing (..)

{-| A proof theory for intuitionistic propositional logic.
-}

import Dict exposing (..)
import Html exposing (..)
import Regex exposing (..)
import Util exposing (..)

import Proof exposing (..)

import Prop exposing (..)
import Prop.Syntax as Prop exposing (..)
import Prop.View as Prop exposing (..)

import Syntax exposing (..)
import Syntax.Lexing as Lexing exposing (LexAction(..))
import Syntax.Parsing as Parsing exposing (Sort(..), Associativity(..))


{-| All bracket sorts are semantically identical, but a left bracket will only match with a right bracket of the same sort.
-}
type Bracket = Paren | Square | Curly


{-| - `Atom` is an atomic proposition.
    - `T` and `F` are the true and false propositions.
    - `And`, `Or`, and `Implies` are the traditional connectives.
    - `Entails` is the turnstile (⊢) in judgements involving contexts.
    - `In` is the set inclusion operator (∈) in judgements about context membership.
    - The `Paren`/`Square`/`Curly` constructors are brackets.
-}
type Con
  = Atom Name
  | T | F
  | And | Or | Implies
  | Cons
  | Entails | In
  | ParenL | ParenR
  | SquareL | SquareR
  | CurlyL | CurlyR


parsingSyntax : Parsing.Syntax Con Bracket
parsingSyntax token =
  case token of
    Atom x -> Const
    T -> Const
    F -> Const
    And -> Op 8 Left
    Or -> Op 7 Left
    Implies -> Op 6 Right
    Cons -> Op 5 Right
    Entails -> Op 4 Right
    In -> Op 4 Right
    ParenL -> Open Paren
    ParenR -> Close Paren
    SquareL -> Open Square
    SquareR -> Close Square
    CurlyL -> Open Curly
    CurlyR -> Close Curly


conView : ConView Con msg
conView connective arguments =
  case (connective, arguments) of
    (Atom x, []) -> Html.text x

    (T, []) -> Html.text "⊤"
    (F, []) -> Html.text "⊥"

    (And, [p1, p2]) ->
      Html.span []
        [ Html.text "("
        , p1
        , Html.text " ∧ "
        , p2
        , Html.text ")"
        ]

    (Or, [p1, p2]) ->
      Html.span []
        [ Html.text "("
        , p1
        , Html.text " ∨ "
        , p2
        , Html.text ")"
        ]

    (Implies, [p1, p2]) ->
      Html.span []
        [ Html.text "("
        , p1
        , Html.text " → "
        , p2
        , Html.text ")"
        ]

    (Cons, [p1, p2]) ->
      Html.span []
      [ p1
      , Html.text ", "
      , p2
      ]

    (Entails, [p1, p2]) ->
      Html.span []
      [ p1
      , Html.text " ⊢ "
      , p2
      ]

    (In, [p1, p2]) ->
      Html.span []
      [ p1
      , Html.text " ∈ "
      , p2
      ]

    _ -> Html.text "error press any key to continue"


var  = Prop.Var
atom = nullary << Atom
top  = nullary T
bot  = nullary F
and  = binary  And
or   = binary  Or
imp  = binary  Implies
cons = binary  Cons
ent  = binary  Entails
in_  = binary  In


lexingSyntax : Lexing.Syntax Con
lexingSyntax =
  [ { regex = uncheckedRegex "\\s+"
    , toToken = always <| Just Drop
    }
  , { regex = uncheckedRegex "\\("
    , toToken = always <| Just <| Keep ParenL
    }
  , { regex = uncheckedRegex "\\)"
    , toToken = always <| Just <| Keep ParenR
    }
  , { regex = uncheckedRegex "\\["
    , toToken = always <| Just <| Keep SquareL
    }
  , { regex = uncheckedRegex "\\]"
    , toToken = always <| Just <| Keep SquareR
    }
  , { regex = uncheckedRegex "{"
    , toToken = always <| Just <| Keep CurlyL
    }
  , { regex = uncheckedRegex "}"
    , toToken = always <| Just <| Keep CurlyR
    }
  , { regex = uncheckedRegex "⊤|🔝|T\\b|true\\b|top\\b"
    , toToken = always <| Just <| Keep T
    }
  , { regex = uncheckedRegex "⊥|_\\|_|F\\b|false\\b|bottom\\b"
    , toToken = always <| Just <| Keep F
    }
  , { regex = uncheckedRegex "\\|-|⊢|entails\\b"
    , toToken = always <| Just <| Keep Entails
    }
  , { regex = uncheckedRegex "&&|&|/\\\\|∧|and\\b"
    , toToken = always <| Just <| Keep And
    }
  , { regex = uncheckedRegex "\\|\\||\\||\\\\/|∨|or\\b"
    , toToken = always <| Just <| Keep Or
    }
  , { regex = uncheckedRegex "→|->|implies\\b"
    , toToken = always <| Just <| Keep Implies
    }
  , { regex = uncheckedRegex ",|;|::|cons\\b"
    , toToken = always <| Just <| Keep Cons
    }
  , { regex = uncheckedRegex "∈|in\\b"
    , toToken = always <| Just <| Keep In
    }
  , { regex = uncheckedRegex "[a-zA-Zα-ωΑ-Ω][0-9a-zA-Zα-ωΑ-Ω]*"
    , toToken = Just << Keep << Atom
    }
  ]

syntax : Syntax Con Bracket
syntax = { lexing = lexingSyntax, parsing = parsingSyntax }

lex = Lexing.lexToEnd lexingSyntax
parse = parseProp syntax

here : Rule Con
here = axiom (in_ (var "a") (cons (var "a") (var "Γ")))

there : Rule Con
there =
  { premises = [in_ (var "a") (var "Γ")]
  , conclusion = in_ (var "a") (cons (var "b") (var "Γ"))
  }

ass : Rule Con
ass =
  { premises = [in_ (var "a") (var "Γ")]
  , conclusion = ent (var "Γ") (var "a")
  }

topIntro : Rule Con
topIntro =
  { premises =
    [
    ]
  , conclusion =
      top
  }

botElim : Rule Con
botElim =
  { premises =
    [ ent (var "Γ") bot
    ]
  , conclusion =
      ent (var "Γ") (var "a")
  }

andIntro : Rule Con
andIntro =
  { premises =
    [ ent (var "Γ") (var "a")
    , ent (var "Γ") (var "b")
    ]
  , conclusion =
      ent (var "Γ") (and (var "a") (var "b"))
  }

andElim : Rule Con
andElim =
  { premises =
    [ ent (var "Γ") (and (var "a") (var "b"))
    , ent (cons (var "a") (cons (var "b") (var "Γ"))) (var "c")
    ]
  , conclusion =
      ent (var "Γ") (var "c")
  }

orLeftIntro : Rule Con
orLeftIntro =
  { premises =
    [ ent (var "Γ") (var "a")
    ]
  , conclusion =
      ent (var "Γ") (or (var "a") (var "b"))
  }

orRightIntro : Rule Con
orRightIntro =
  { premises =
    [ ent (var "Γ") (var "b")
    ]
  , conclusion =
      ent (var "Γ") (or (var "a") (var "b"))
  }

orElim : Rule Con
orElim =
  { premises =
    [ ent (var "Γ") (or (var "a") (var "b"))
    , ent (cons (var "a") (var "Γ")) (var "c")
    , ent (cons (var "b") (var "Γ")) (var "c")
    ]
  , conclusion =
      ent (var "Γ") (var "c")
  }

impIntro : Rule Con
impIntro =
  { premises =
    [ ent (cons (var "a") (var "Γ")) (var "b")
    ]
  , conclusion =
      ent (var "Γ") (imp (var "a") (var "b"))
  }

impElim : Rule Con
impElim =
  { premises =
    [ ent (var "Γ") (imp (var "a") (var "b"))
    , ent (var "Γ") (var "a")
    ]
  , conclusion =
      ent (var "Γ") (var "b")
  }

axioms : Dict Name (Rule Con)
axioms =
  Dict.fromList
    [ ("Here", here)
    , ("There", there)
    , ("Assumed", ass)
    , ("⊤-Intro", topIntro)
    , ("⊥-Elim", botElim)
    , ("∧-Intro", andIntro)
    , ("∧-Elim", andElim)
    , ("∨L-Intro", orLeftIntro)
    , ("∨R-Intro", orRightIntro)
    , ("∨-Elim", orElim)
    , ("→-Intro", impIntro)
    , ("→-Elim", impElim)
    ]
