%{
[@@@warning "-42-33"]

(* A pre parser for the JsLIGO parser.

   The following syntax causes shift/reduce conflicts in the JsLIGO
   parser:

   let a = (b, c);

   with

   let a = (b, c) =>

   and the different versions between them.

   To solve this in a manageable way the JsLIGO PreParser inserts a
   [ES6FUN] virtual token based on a set of heuristics.

   In certain cases we are sure that a [ES6FUN] token needs to be
   inserted or not.  These cases can be considered [Solved]. In some
   cases we need more information and either request to [Inject] an
   [ES6FUN] token or make a [Suggestion] of where to place the
   [ES6FUN] token. *)

(* Vendors dependencies *)

open Simple_utils.Region

(* LIGO dependencies *)

module Region = Simple_utils.Region
module Wrap   = Lexing_shared.Wrap

open State
open Token

let es6fun = Token.ghost_ES6FUN

let solve = function
  Solved t     -> t
| Inject t     -> es6fun :: t
| Suggestion f -> f (Some es6fun)

(*
  Add token [token] to list [list].

  A list can either be
    * Solved: no ES6FUN token needs to be inserted for this code
    * Inject: an ES6FUN token might need to be inserted
    * Suggestion: an ES6FUN token might need to be inserted at the
      suggested location *)

let cons token list =
  let open Token in
  match token, list with
    (* let foo = a:int => a *)
  | EQ _, Suggestion f ->
      Solved (token :: es6fun :: f None)

  | COLON _, Suggestion f ->
      Suggestion (function
                    Some c -> token :: c :: f None
                  | None   -> token :: f None)

    (* let foo:int => a *)
  | (Let _  | Const _), Suggestion f ->
      Solved (token :: f (Some es6fun))

    (* | Foo, y =>  *)
  | VBAR _, Suggestion f -> Solved (token :: f None)

    (* let foo = () => 2 *)
  | EQ _, Inject tokens -> Solved (token :: es6fun :: tokens)

    (* Bar => *)
    (* let x = (x: int, b: int => int) => b(x) *)
  | COMMA _, Inject tokens ->
      Suggestion (function
                    Some c -> token :: c :: tokens
                  | None   -> token :: tokens)

    (* , foo => bar *)
  | COMMA _, Suggestion f -> Solved (token :: f (Some es6fun))

    (* | [] => *)
  | VBAR _, Inject tokens -> Solved (token :: tokens)

    (* : bar

      It's not clear yet what needs to happen here, so we make a suggestion.
      See also the above cases that use Suggestion.
    *)
  | COLON _, Inject tokens ->
      Suggestion (function
                    Some t -> token :: t :: tokens
                  | None   -> token :: tokens)

  | _, Inject tokens -> Inject (token :: tokens)
  | _, Solved tokens -> Solved (token :: tokens)
  | _, Suggestion f  -> Suggestion (fun c -> token :: f c)


(* Concatenation of lists *)

let concat a b =
  match a, b with
    Solved a,     Solved b -> Solved (a @ b)
  | Solved a,     Inject b
  | Inject a,     Solved b -> Inject (a @ b)
  | Inject a,     Inject b -> Inject (a @ es6fun :: b)
  | Solved a, Suggestion f ->
      Suggestion (function
                    Some c -> c :: a @ f None
                  | None   ->      a @ f None)
  | Inject a,     Suggestion f -> Inject (a @ solve (Inject (f None)))
  | Suggestion a, Suggestion b -> Suggestion (fun c -> a c @ b None)
  | Suggestion a, Inject b
  | Suggestion a, Solved b     -> Suggestion (fun c -> a c @ b)
%}

(* See [../../02-parsing/reasonligo/ParToken.mly] for the definition
   of the tokens. *)

(* Entry points *)

%start self_pass
%type <Token.t list> self_pass

%%

everything_else:
  "<block_comment>" { BlockCom $1  }
| "<line_comment>"  { LineCom $1   }
| "<directive>"     { Directive $1 }
| "<string>"        { String $1    }
| "<verbatim>"      { Verbatim $1  }
| "<bytes>"         { Bytes $1     }
| "<int>"           { Int $1       }
| "<ident>"         { Ident $1     }
| "<uident>"        { UIdent $1    }
| "[@attr]"         { Attr $1      }
| "-"               { MINUS $1     }
| "+"               { PLUS $1      }
| "/"               { SLASH $1     }
| "*"               { TIMES $1     }
| "%"               { REM $1       }
| ","               { COMMA $1     }
| ";"               { SEMI $1      }
| ":"               { COLON $1     }
| "."               { DOT $1       }
| "..."             { ELLIPSIS $1  }
| "||"              { BOOL_OR $1   }
| "&&"              { BOOL_AND $1  }
| "!"               { BOOL_NOT $1  }
| "="               { EQ $1        }
| "=="              { EQ2 $1       }
| "!="              { NE $1        }
| "<"               { LT $1        }
| ">"               { GT $1        }
| "<="              { LE $1        }
| ">="              { GE $1        }
| "+="              { PLUS_EQ $1   }
| "-="              { MINUS_EQ $1  }
| "*="              { MULT_EQ $1   }
| "%="              { REM_EQ $1    }
| "/="              { DIV_EQ $1    }
| "|"               { VBAR $1      }
| "_"               { WILD $1      }
| "break"           { Break $1     }
| "case"            { Case $1      }
| "const"           { Const $1     }
| "default"         { Default $1   }
| "else"            { Else $1      }
| "export"          { Export $1    }
| "for"             { For $1       }
| "if"              { If $1        }
| "import"          { Import $1    }
| "let"             { Let $1       }
| "of"              { Of $1        }
| "return"          { Return $1    }
| "switch"          { Switch $1    }
| "while"           { While $1     }
| "as"              { As $1        }
| "namespace"       { Namespace $1 }
| "type"            { Type $1      }
| ZWSP              { ZWSP $1      }

inner:
  /* nothing */ { Solved [] }
| everything_else inner {
    cons $1 $2
  }
| everything_else "=>" inner {
    Inject ($1 :: ARROW $2 :: solve $3)
  }
| parenthesized inner
| braces inner
| brackets inner {
    concat $1 $2 }

parenthesized:
  "(" inner ")" {
    Solved ((LPAR $1 :: solve $2) @ (RPAR $3 :: []))
  }
| "(" inner ")" "=>" {
    Solved ((ES6FUN $1 :: LPAR $1 :: solve $2) @ (RPAR $3 :: ARROW $4 :: []))
}

braces:
  "{" inner "}" {
  Solved ((LBRACE $1 :: solve $2) @ (RBRACE $3 :: []))
  }
| "{" inner "}" "=>" {
    Inject ((LBRACE $1 :: solve $2) @ (RBRACE $3 :: ARROW $4 :: []))
  }

brackets:
  "[" inner "]" {
  Solved ((LBRACKET $1 :: solve $2) @ (RBRACKET $3 :: []))
}
| "[" inner "]" "=>" {
  Inject ((LBRACKET $1 :: solve $2) @ (RBRACKET $3 :: ARROW $4 :: []))
}

self_pass_inner:
  everything_else self_pass_inner {
    cons $1 $2
  }
| everything_else "=>" self_pass_inner {
   Inject ($1 :: ARROW $2 :: (solve $3))
}
| parenthesized self_pass_inner
| braces self_pass_inner
| brackets self_pass_inner {
  concat $1 $2
}
| EOF { Solved [EOF $1] }

self_pass:
  self_pass_inner {
    match $1 with
      Solved s     -> s
    | Inject s     -> s      (* let it fail in the parser *)
    | Suggestion f -> f None (* let it fail in the parser *)
  }

%%
