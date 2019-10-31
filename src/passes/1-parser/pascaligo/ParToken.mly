%{
%}

(* Tokens (mirroring thise defined in module LexToken) *)

  (* Literals *)

%token           <LexToken.lexeme Region.reg> String
%token <(LexToken.lexeme * Hex.t) Region.reg> Bytes
%token   <(LexToken.lexeme * Z.t) Region.reg> Int
%token   <(LexToken.lexeme * Z.t) Region.reg> Nat
%token   <(LexToken.lexeme * Z.t) Region.reg> Mutez
%token           <LexToken.lexeme Region.reg> Ident
%token           <LexToken.lexeme Region.reg> Constr

  (* Symbols *)

%token <Region.t> SEMI        (* ";"   *)
%token <Region.t> COMMA       (* ","   *)
%token <Region.t> LPAR        (* "("   *)
%token <Region.t> RPAR        (* ")"   *)
%token <Region.t> LBRACE      (* "{"   *)
%token <Region.t> RBRACE      (* "}"   *)
%token <Region.t> LBRACKET    (* "["   *)
%token <Region.t> RBRACKET    (* "]"   *)
%token <Region.t> CONS        (* "#"   *)
%token <Region.t> VBAR        (* "|"   *)
%token <Region.t> ARROW       (* "->"  *)
%token <Region.t> ASS         (* ":="  *)
%token <Region.t> EQ          (* "="   *)
%token <Region.t> COLON       (* ":"   *)
%token <Region.t> LT          (* "<"   *)
%token <Region.t> LE          (* "<="  *)
%token <Region.t> GT          (* ">"   *)
%token <Region.t> GE          (* ">="  *)
%token <Region.t> NE          (* "=/=" *)
%token <Region.t> PLUS        (* "+"   *)
%token <Region.t> MINUS       (* "-"   *)
%token <Region.t> SLASH       (* "/"   *)
%token <Region.t> TIMES       (* "*"   *)
%token <Region.t> DOT         (* "."   *)
%token <Region.t> WILD        (* "_"   *)
%token <Region.t> CAT         (* "^"   *)

  (* Keywords *)

%token <Region.t> And         (* "and"        *)
%token <Region.t> Begin       (* "begin"      *)
%token <Region.t> BigMap      (* "big_map"    *)
%token <Region.t> Block       (* "block"      *)
%token <Region.t> Case        (* "case"       *)
%token <Region.t> Const       (* "const"      *)
%token <Region.t> Contains    (* "contains"   *)
%token <Region.t> Else        (* "else"       *)
%token <Region.t> End         (* "end"        *)
%token <Region.t> For         (* "for"        *)
%token <Region.t> Function    (* "function"   *)
%token <Region.t> From        (* "from"       *)
%token <Region.t> If          (* "if"         *)
%token <Region.t> In          (* "in"         *)
%token <Region.t> Is          (* "is"         *)
%token <Region.t> List        (* "list"       *)
%token <Region.t> Map         (* "map"        *)
%token <Region.t> Mod         (* "mod"        *)
%token <Region.t> Nil         (* "nil"        *)
%token <Region.t> Not         (* "not"        *)
%token <Region.t> Of          (* "of"         *)
%token <Region.t> Or          (* "or"         *)
%token <Region.t> Patch       (* "patch"      *)
%token <Region.t> Record      (* "record"     *)
%token <Region.t> Remove      (* "remove"     *)
%token <Region.t> Set         (* "set"        *)
%token <Region.t> Skip        (* "skip"       *)
%token <Region.t> Then        (* "then"       *)
%token <Region.t> To          (* "to"         *)
%token <Region.t> Type        (* "type"       *)
%token <Region.t> Var         (* "var"        *)
%token <Region.t> While       (* "while"      *)
%token <Region.t> With        (* "with"       *)

  (* Data constructors *)

%token <Region.t> C_False     (* "False" *)
%token <Region.t> C_None      (* "None"  *)
%token <Region.t> C_Some      (* "Some"  *)
%token <Region.t> C_True      (* "True"  *)
%token <Region.t> C_Unit      (* "Unit"  *)

  (* Virtual tokens *)

%token <Region.t> EOF

%%
