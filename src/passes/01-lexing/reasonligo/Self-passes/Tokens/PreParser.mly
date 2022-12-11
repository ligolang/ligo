%{
[@@@warning "-42-33"]

(*
 A pre parser for the ReasonLIGO parser.

 ReasonLIGO has the following challenges which cannot be solved with
 LR1 parsing without doing big sacrifices:

 ```reasonligo
 let foo = (x, y);  // a tuple
 let foo = (x, y) => x; // a function
 ```

 and variations of this.

 To solve this, an ES6FUN token is inserted where we know it's a arrow function.

 This parser can be separated in two parts:
 - grammar, which is only concerned with arrows, braces, blocks and
            parentheses. This is expected to be well-formed, otherwise an error
            will be given from es6fun_error.msg.in. Note that a challenge for
            these error messages is over-approximation. This means that the
            error messages are _very_ general.
 - solving, here it's decided what needs to happen for an arrow "=>". The
            functions 'cons' and 'concat' are important here.
*)

(* Vendors dependencies *)

open Simple_utils.Region

(* LIGO dependencies *)

module Wrap = Lexing_shared.Wrap
module Region = Simple_utils.Region

open Token
open State


let es6fun_token = ES6FUN (Wrap.wrap "=>" Region.ghost)

let solve = function
  Solved t     -> t
| Inject t     -> es6fun_token :: t
| Suggestion f -> f (Some es6fun_token)

(*
  Add token a to list b.

  A list can be either:
   -     Solved: no ES6FUN token needs to be inserted for this code
   -     Inject: an ES6FUN token might need to be inserted
   - Suggestion: an ES6FUN token might need to be inserted at the suggested location
 *)

let cons a b =
  match a, b with
    (* let foo = a:int => a *)
  | (EQ _ | COLON _  as s), Suggestion f  -> Solved (s :: es6fun_token :: (f None))
    (* let foo:int => a *)
  | (Let _ | Rec _ as l),   Suggestion f  -> Solved (l :: (f (Some es6fun_token)))
    (* | Foo, y =>  *)
  | VBAR _ as v,            Suggestion f  -> Solved (v :: (f None))
    (* let foo = () => 2 *)
  | EQ _ as s,              Inject tokens -> Solved (s :: es6fun_token :: tokens)
    (* Bar => *)
  | UIdent _ as u,          Inject tokens -> Solved (u :: tokens)
    (* let x = (x: int, b: int => int) => b(x) *)
  | COMMA _ as s,           Inject tokens -> Suggestion (fun c ->
    match c with
     Some c ->
      s :: c :: tokens
    | None -> s :: tokens)
    (* | [] => *)
  | VBAR _ as v,            Inject tokens -> Solved (v :: tokens)
    (* : bar

      It's not clear yet what needs to happen here, so we make a suggestion.
      See also the above cases that use Suggestion.
    *)
  | COLON _ as c,         Inject tokens -> Suggestion (fun t ->
      match t with
        Some t ->
          c :: t :: tokens
      | None ->
          c :: tokens
      )
  | s,                      Inject tokens -> Inject (s :: tokens)
  | _,                      Solved s      -> Solved (a :: s)
  | _,                      Suggestion f  -> Suggestion (fun c -> a :: f c)


(* Concatenate lists *)

let concat a b =
  match a, b with
    Solved a,     Solved b                  -> Solved (a @ b)
  | Solved a,     Inject b
  | Inject a,     Solved b                  -> Inject (a @ b)
  | Inject a,     Inject b                  -> Inject (a @ es6fun_token :: b)
  | Solved a, Suggestion f                  -> Suggestion (fun c ->
    match c with
      Some c ->
        c :: a @ f None
    | None ->
        a @ f None
    )
  | Inject a,     Suggestion f              -> Inject (a @ solve (Inject (f None)))
  | Suggestion a, Suggestion b              -> Suggestion (fun c -> a c @ b None)
  | Suggestion a, Inject b
  | Suggestion a, Solved b                  -> Suggestion (fun c -> a c @ b)
%}

(* See [../../02-parsing/reasonligo/ParToken.mly] for the definition of tokens. *)

(* Entry points *)

%start self_pass
%type <Token.t list> self_pass

%%

everything_else:
  "<directive>" { Directive $1 }
| "<string>"    { String $1   }
| "<verbatim>"  { Verbatim $1 }
| "<bytes>"     { Bytes $1    }
| "<int>"       { Int $1      }
| "<nat>"       { Nat $1      }
| "<mutez>"     { Mutez $1    }
| "<ident>"     { Ident $1    }
| "<uident>"    { UIdent $1   }
| "[@attr]"     { Attr $1     }
| "-"           { MINUS $1    }
| "+"           { PLUS $1     }
| "/"           { SLASH $1    }
| "*"           { TIMES $1    }
| "++"          { PLUS2 $1    }
| "."           { DOT $1      }
| "..."         { ELLIPSIS $1 }
| ","           { COMMA $1    }
| ";"           { SEMI $1     }
| "|"           { VBAR $1     }
| "_"           { WILD $1     }
| "="           { EQ $1       }
| "=="          { EQ2 $1      }
| "!="          { NE $1       }
| "<"           { LT $1       }
| ">"           { GT $1       }
| "<="          { LE $1       }
| "!"           { NOT $1      }
| "||"          { BOOL_OR $1  }
| "&&"          { BOOL_AND $1 }
| "'"           { QUOTE $1    }
| "else"        { Else $1     }
| "if"          { If $1       }
| "let"         { Let $1      }
| "rec"         { Rec $1      }
| "switch"      { Switch $1   }
| "mod"         { Mod $1      }
| "land"        { Land $1     }
| "lor"         { Lor $1      }
| "lxor"        { Lxor $1     }
| "lsl"         { Lsl $1      }
| "lsr"         { Lsr $1      }
| "or"          { Or $1       }
| "type"        { Type $1     }
| "module"      { Module $1   }
| ":"           { COLON $1    }
| ZWSP          { ZWSP $1     }

inner:
  /* nothing */ { Solved [] }
| everything_else inner {
    cons $1 $2
  }
| everything_else "=>" inner {
  Inject ($1 :: (ARROW $2) :: solve $3)
}
| parenthesized inner
| braces inner
| brackets inner {
  concat $1 $2
}

parenthesized:
  "(" inner ")" {
    Solved ((LPAR $1 :: solve $2) @ (RPAR $3 :: []))
  }
| "(" inner ")" "=>" {
    Inject ((LPAR $1 :: solve $2) @ (RPAR $3 :: ARROW $4 :: []))
}

braces:
  "{" inner "}" {
  Solved ((LBRACE $1 :: solve $2) @ (RBRACE $3 :: []))
  }
| "{" inner "}" "=>" {
    Inject ((LBRACE $1 :: solve $2) @ (RBRACE $3 :: ARROW $4 :: []))
  }

brackets:
  "[%lang" inner "]" {
  Solved ((Lang $1 :: solve $2) @ (RBRACKET $3 :: []))
}
| "[" inner "]" {
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
