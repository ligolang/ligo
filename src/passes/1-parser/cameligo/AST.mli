(* Abstract Syntax Tree (AST) for Cameligo *)

[@@@warning "-30"]

open Utils

(* Regions

   The AST carries all the regions where tokens have been found by the
   lexer, plus additional regions corresponding to whole subtrees
   (like entire expressions, patterns etc.). These regions are needed
   for error reporting and source-to-source transformations. To make
   these pervasive regions more legible, we define singleton types for
   the symbols, keywords etc. with suggestive names like "kwd_and"
   denoting the _region_ of the occurrence of the keyword "and".
*)

type 'a reg = 'a Region.reg

val last : ('a -> Region.t) -> 'a list -> Region.t
val nsepseq_to_region : ('a -> Region.t) -> ('a,'sep) nsepseq -> Region.t

(* Some keywords of OCaml *)

type keyword   = Region.t
type kwd_and   = Region.t
type kwd_begin = Region.t
type kwd_else  = Region.t
type kwd_end   = Region.t
type kwd_false = Region.t
type kwd_fun   = Region.t
type kwd_if    = Region.t
type kwd_in    = Region.t
type kwd_let   = Region.t
type kwd_let_entry = Region.t
type kwd_match = Region.t
type kwd_mod   = Region.t
type kwd_not   = Region.t
type kwd_of    = Region.t
type kwd_or    = Region.t
type kwd_then  = Region.t
type kwd_true  = Region.t
type kwd_type  = Region.t
type kwd_with  = Region.t

(* Data constructors *)

type c_None  = Region.t
type c_Some  = Region.t

(* Symbols *)

type arrow  = Region.t                                               (* "->" *)
type cons   = Region.t                                               (* "::" *)
type cat    = Region.t                                               (* "^"  *)
type append = Region.t                                               (* "@"  *)
type dot    = Region.t                                               (* "."  *)

(* Arithmetic operators *)

type minus = Region.t                                                 (* "-" *)
type plus  = Region.t                                                 (* "+" *)
type slash   = Region.t                                                 (* "/" *)
type times  = Region.t                                                 (* "*" *)

(* Boolean operators *)

type bool_or  = Region.t                                             (* "||" *)
type bool_and = Region.t                                             (* "&&" *)

(* Comparisons *)

type equal = Region.t                                                   (* "="  *)
type neq = Region.t                                                   (* "<>" *)
type lt = Region.t                                                   (* "<"  *)
type gt = Region.t                                                   (* ">"  *)
type leq = Region.t                                                   (* "=<" *)
type geq = Region.t                                                   (* ">=" *)

(* Compounds *)

type lpar = Region.t                                                  (* "(" *)
type rpar = Region.t                                                  (* ")" *)
type lbracket = Region.t                                                  (* "[" *)
type rbracket = Region.t                                                  (* "]" *)
type lbrace = Region.t (* "{" *)
type rbrace = Region.t (* "}" *)

(* Separators *)

type comma = Region.t                                                 (* "," *)
type semi  = Region.t                                                 (* ";" *)
type vbar  = Region.t                                                 (* "|" *)
type colon = Region.t

(* Wildcard *)

type wild = Region.t                                                  (* "_" *)

(* Literals *)

type variable    = string reg
type fun_name    = string reg
type type_name   = string reg
type field_name  = string reg
type type_constr = string reg
type constr      = string reg

(* Parentheses *)

type 'a par = {
  lpar   : lpar;
  inside : 'a;
  rpar   : rpar
}

type the_unit = lpar * rpar

(* The Abstract Syntax Tree (finally) *)

type t = {
  decl : declaration nseq;
  eof  : eof
}

and ast = t

and eof = Region.t

and declaration =
  Let      of (kwd_let * let_binding) reg              (* let x = e       *)
| TypeDecl of type_decl reg                            (* type ...        *)

(* Non-recursive values *)

and let_binding = {                                  (* p = e   p : t = e *)
  binders  : pattern nseq;
  lhs_type : (colon * type_expr) option;
  eq       : equal;
  let_rhs  : expr
}

(* Recursive types *)

and type_decl = {
  kwd_type   : kwd_type;
  name       : type_name;
  eq         : equal;
  type_expr  : type_expr
}

and type_expr =
  TProd   of cartesian
| TSum    of (variant reg, vbar) nsepseq reg
| TRecord of field_decl reg ne_injection reg
| TApp    of (type_constr * type_tuple) reg
| TFun    of (type_expr * arrow  * type_expr) reg
| TPar    of type_expr par reg
| TVar    of variable

and cartesian = (type_expr, times) nsepseq reg

and variant = {
  constr : constr;
  arg    : (kwd_of * type_expr) option
}

and field_decl = {
  field_name : field_name;
  colon      : colon;
  field_type : type_expr
}

and type_tuple = (type_expr, comma) nsepseq par reg

and pattern =
  PConstr of constr_pattern                        (* True () None A B(3,"") *)
| PUnit   of the_unit reg                                   (*            () *)
| PFalse  of kwd_false                                      (*         false *)
| PTrue   of kwd_true                                       (*          true *)
| PVar    of variable                                       (*             x *)
| PInt    of (Lexer.lexeme * Z.t) reg                       (*             7 *)
| PNat    of (Lexer.lexeme * Z.t) reg                       (*         7p 7n *)
| PBytes  of (Lexer.lexeme * Hex.t) reg                     (*        0xAA0F *)
| PString of string reg                                     (*         "foo" *)
| PWild   of wild                                           (*             _ *)
| PList   of list_pattern
| PTuple  of (pattern, comma) nsepseq reg                   (* p1, p2, ...   *)
| PPar    of pattern par reg                                (*           (p) *)
| PRecord of field_pattern reg ne_injection reg             (*  {a=...; ...} *)
| PTyped  of typed_pattern reg                              (*     (x : int) *)

and constr_pattern =
| PNone      of c_None
| PSomeApp   of (c_Some * pattern) reg
| PConstrApp of (constr * pattern option) reg

and list_pattern =
  PListComp of pattern injection reg                       (* [p1; p2; ...] *)
| PCons     of (pattern * cons * pattern) reg              (* p1 :: p2      *)

and typed_pattern = {
  pattern   : pattern;
  colon     : colon;
  type_expr : type_expr
}

and field_pattern = {
  field_name : field_name;
  eq         : equal;
  pattern    : pattern
}

and expr =
  ECase   of expr case reg                   (* p1 -> e1 | p2 -> e2 | ... *)
| ECond   of cond_expr   reg      (* if e1 then e2 else e3                *)
| EAnnot  of (expr * type_expr) reg                              (* e : t *)
| ELogic  of logic_expr
| EArith  of arith_expr
| EString of string_expr
| EList   of list_expr                                 (* x::y::l [1;2;3] *)
| EConstr of constr_expr                               (* A  B(1,A) (C A) *)
| ERecord of field_assign reg ne_injection reg           (* {f1=e1; ... } *)
| EProj   of projection reg                               (* x.y.z  M.x.y *)
| EVar    of variable                                                (* x *)
| ECall   of (expr * expr nseq) reg                        (* e e1 ... en *)
| EBytes  of (string * Hex.t) reg                               (* 0xAEFF *)
| EUnit   of the_unit reg                                           (* () *)
| ETuple  of (expr, comma) nsepseq reg                     (* e1, e2, ... *)
| EPar    of expr par reg                                          (* (e) *)
| ELetIn  of let_in reg           (* let p1 = e1 and p2 = e2 and ... in e *)
| EFun    of fun_expr reg         (*                           fun x -> e *)
| ESeq    of expr injection reg   (*           begin e1; e2; ... ; en end *)

and 'a injection = {
  compound   : compound;
  elements   : ('a, semi) sepseq;
  terminator : semi option
}

and 'a ne_injection = {
  compound    : compound;
  ne_elements : ('a, semi) nsepseq;
  terminator  : semi option
}

and compound =
  BeginEnd of kwd_begin * kwd_end
| Braces   of lbrace * rbrace
| Brackets of lbracket * rbracket

and list_expr =
  ECons     of cat bin_op reg                            (* e1 :: e3      *)
| EListComp of expr injection reg                        (* [e1; e2; ...] *)
(*| Append of (expr * append * expr) reg *)              (* e1  @ e2      *)

and string_expr =
  Cat    of cat bin_op reg                               (* e1  ^ e2      *)
| String of string reg                                   (* "foo"         *)

and constr_expr =
  ENone      of c_None
| ESomeApp   of (c_Some * expr) reg
| EConstrApp of (constr * expr option) reg

and arith_expr =
  Add   of plus bin_op reg                                  (* e1  + e2   *)
| Sub   of minus bin_op reg                                 (* e1  - e2   *)
| Mult  of times bin_op reg                                 (* e1  *  e2  *)
| Div   of slash bin_op reg                                 (* e1  /  e2  *)
| Mod   of kwd_mod bin_op reg                               (* e1 mod e2  *)
| Neg   of minus un_op reg                                  (* -e         *)
| Int   of (string * Z.t) reg                               (* 12345      *)
| Nat   of (string * Z.t) reg                               (* 3n         *)
| Mutez of (string * Z.t) reg                      (* 1.00tz 3tz 233mutez *)

and logic_expr =
  BoolExpr of bool_expr
| CompExpr of comp_expr

and bool_expr =
  Or    of kwd_or bin_op reg
| And   of kwd_and bin_op reg
| Not   of kwd_not un_op reg
| True  of kwd_true
| False of kwd_false

and 'a bin_op = {
  op   : 'a;
  arg1 : expr;
  arg2 : expr
}

and 'a un_op = {
  op  : 'a;
  arg : expr
}

and comp_expr =
  Lt    of lt    bin_op reg
| Leq   of leq   bin_op reg
| Gt    of gt    bin_op reg
| Geq   of geq   bin_op reg
| Equal of equal bin_op reg
| Neq   of neq   bin_op reg

and projection = {
  struct_name : variable;
  selector    : dot;
  field_path  : (selection, dot) nsepseq
}

and selection =
  FieldName of variable
| Component of (string * Z.t) reg

and field_assign = {
  field_name : field_name;
  assignment : equal;
  field_expr : expr
}

and 'a case = {
  kwd_match : kwd_match;
  expr      : expr;
  kwd_with  : kwd_with;
  lead_vbar : vbar option;
  cases     : ('a case_clause reg, vbar) nsepseq reg
}

and 'a case_clause = {
  pattern : pattern;
  arrow   : arrow;
  rhs     : 'a
}

and let_in = {
  kwd_let : kwd_let;
  binding : let_binding;
  kwd_in  : kwd_in;
  body    : expr
}

and fun_expr = {
  kwd_fun  : kwd_fun;
  binders  : pattern nseq;
  lhs_type : (colon * type_expr) option;
  arrow    : arrow;
  body     : expr
}

and cond_expr = {
  kwd_if   : kwd_if;
  test     : expr;
  kwd_then : kwd_then;
  ifso     : expr;
  kwd_else : kwd_else;
  ifnot    : expr
}

(* Projecting regions from sundry nodes of the AST. See the first
   comment at the beginning of this file. *)

val pattern_to_region   : pattern -> Region.t
val expr_to_region      : expr -> Region.t
val type_expr_to_region : type_expr -> Region.t
val selection_to_region : selection -> Region.t
