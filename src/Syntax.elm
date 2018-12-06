module Syntax exposing (..)

import Syntax.Lexing as Lexing
import Syntax.Parsing as Parsing

type alias Syntax c b =
  { lexing : Lexing.Syntax c
  , parsing : Parsing.Syntax c b
  }
