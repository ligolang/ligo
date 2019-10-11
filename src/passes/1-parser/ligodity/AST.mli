(* Abstract Syntax Tree (AST) for Ligodity *)

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
  decl : declaration Utils.nseq;
  eof  : eof
}

and ast = t

and eof = Region.t

and declaration =
  Let      of (kwd_let * let_binding) reg        (* let x = e       *)
| LetEntry of (kwd_let_entry * let_binding) reg  (* let%entry x = e *)
| TypeDecl of type_decl reg                      (* type ...        *)

(* Non-recursive values *)

and let_binding = {                                  (* p = e   p : t = e *)
  bindings : pattern list;
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
| TSum    of (variant reg, vbar) Utils.nsepseq reg
| TRecord of record_type
| TApp    of (type_constr * type_tuple) reg
| TFun    of (type_expr * arrow  * type_expr) reg
| TPar    of type_expr par reg
| TAlias  of variable

and cartesian = (type_expr, times) Utils.nsepseq reg

and variant = {
  constr : constr;
  args   : (kwd_of * cartesian) option
}

and record_type = field_decl reg injection reg

and field_decl = {
  field_name : field_name;
  colon      : colon;
  field_type : type_expr
}

and type_tuple = (type_expr, comma) Utils.nsepseq par reg

and pattern =
  PTuple  of (pattern, comma) Utils.nsepseq reg             (* p1, p2, ...   *)
| PList   of list_pattern
| PVar    of variable                                       (*             x *)
| PUnit   of the_unit reg                                   (*            () *)
| PInt    of (string * Z.t) reg                             (*             7 *)
| PTrue   of kwd_true                                       (*          true *)
| PFalse  of kwd_false                                      (*         false *)
| PString of string reg                                     (*         "foo" *)
| PWild   of wild                                           (*             _ *)
| PPar    of pattern par reg                                (*           (p) *)
| PConstr of (constr * pattern option) reg                  (*    A B(3,"")  *)
| PRecord of record_pattern                                 (*  {a=...; ...} *)
| PTyped  of typed_pattern reg                              (*     (x : int) *)

and list_pattern =
  Sugar of pattern injection reg                            (* [p1; p2; ...] *)
| PCons of (pattern * cons * pattern) reg                   (* p1 :: p2      *)

and typed_pattern = {
  pattern   : pattern;
  colon     : colon;
  type_expr : type_expr
}

and record_pattern = field_pattern reg injection reg

and field_pattern = {
  field_name : field_name;
  eq         : equal;
  pattern    : pattern
}

and expr =
  ECase   of expr case reg                     (* p1 -> e1 | p2 -> e2 | ... *)
| EAnnot  of annot_expr reg                                        (* e : t *)
| ELogic  of logic_expr
| EArith  of arith_expr
| EString of string_expr
| EList   of list_expr
| EConstr of constr_expr reg
| ERecord of record_expr                                   (* {f1=e1; ... } *)
| EProj   of projection reg                                 (* x.y.z  M.x.y *)
| EVar    of variable                                                  (* x *)
| ECall   of (expr * expr Utils.nseq) reg                    (* e e1 ... en *)
| EBytes  of (string * Hex.t) reg                                 (* 0xAEFF *)
| EUnit   of the_unit reg                                             (* () *)
| ETuple  of (expr, comma) Utils.nsepseq reg                 (* e1, e2, ... *)
| EPar    of expr par reg                                            (* (e) *)
| ELetIn  of let_in reg             (* let p1 = e1 and p2 = e2 and ... in e *)
| EFun    of fun_expr reg           (* fun x -> e                           *)
| ECond   of conditional reg        (* if e1 then e2 else e3                *)
| ESeq    of sequence               (* begin e1; e2; ... ; en end           *)

and constr_expr = constr * expr option

and annot_expr = expr * type_expr

and 'a injection = {
  opening    : opening;
  elements   : ('a, semi) Utils.sepseq;
  terminator : semi option;
  closing    : closing
}

and opening =
  Begin    of kwd_begin
| With     of kwd_with
| LBrace   of lbrace
| LBracket of lbracket

and closing =
  End      of kwd_end
| RBrace   of rbrace
| RBracket of rbracket

and list_expr =
  Cons   of cat bin_op reg                                 (* e1 :: e3      *)
| List   of expr injection reg                             (* [e1; e2; ...] *)
(*| Append of (expr * append * expr) reg *)                (* e1  @ e2      *)

and string_expr =
  Cat    of cat bin_op reg                                 (* e1  ^ e2      *)
| String of string reg                                     (* "foo"         *)

and arith_expr =
  Add  of plus bin_op reg                                     (* e1  + e2   *)
| Sub  of minus bin_op reg                                    (* e1  - e2   *)
| Mult of times bin_op reg                                    (* e1  *  e2  *)
| Div  of slash bin_op reg                                    (* e1  /  e2  *)
| Mod  of kwd_mod bin_op reg                                  (* e1 mod e2  *)
| Neg  of minus un_op reg                                     (* -e         *)
| Int  of (string * Z.t) reg                                  (* 12345      *)
| Nat  of (string * Z.t) reg                                  (* 3p         *)
| Mtz  of (string * Z.t) reg                                  (* 1.00tz 3tz *)

and logic_expr =
  BoolExpr of bool_expr
| CompExpr of comp_expr

and bool_expr =
  Or       of kwd_or bin_op reg
| And      of kwd_and bin_op reg
| Not      of kwd_not un_op reg
| True     of kwd_true
| False    of kwd_false

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
  field_path  : (selection, dot) Utils.nsepseq
}

and selection =
  FieldName of variable
| Component of (string * Z.t) reg par reg

and record_expr = field_assign reg injection reg

and field_assign = {
  field_name : field_name;
  assignment : equal;
  field_expr : expr
}

and sequence = expr injection reg

and 'a case = {
  kwd_match : kwd_match;
  expr      : expr;
  opening   : opening;
  lead_vbar : vbar option;
  cases     : ('a case_clause reg, vbar) Utils.nsepseq reg;
  closing   : closing
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
  kwd_fun : kwd_fun;
  params  : pattern list;
  p_annot : (colon * type_expr) option;
  arrow   : arrow;
  body    : expr
}

and conditional = {
  kwd_if     : kwd_if;
  test       : expr;
  kwd_then   : kwd_then;
  ifso       : expr;
  kwd_else   : kwd_else;
  ifnot      : expr
}

(* Normalising nodes of the AST so the interpreter is more uniform and
   no source regions are lost in order to enable all manner of
   source-to-source transformations from the rewritten AST and the
   initial source.

   The first kind of expressions to be normalised is lambdas, like:

     fun a -> fun b -> a
     fun a b -> a
     fun a (b,c) -> a

   to become

     fun a -> fun b -> a
     fun a -> fun b -> a
     fun a -> fun x -> let (b,c) = x in a

   The second kind is let-bindings introducing functions without the
   "fun" keyword, like

     let g a b = a
     let h a (b,c) = a

   which become

     let g = fun a -> fun b -> a
     let h = fun a -> fun x -> let (b,c) = x in a

   The former is actually a subcase of the latter. Indeed, the general
   shape of the former is

     fun <patterns> -> <expr>

   and the latter is

     let <ident> <patterns> = <expr>

   The isomorphic parts are "<patterns> -> <expr>" and "<patterns> =
   <expr>".

     The call [norm patterns sep expr], where [sep] is a region either
   of an "->" or a "=", evaluates in a function expression (lambda),
   as expected. In order to get the regions right in the case of
   lambdas, additional regions are passed: [norm ~reg:(total,kwd_fun)
   patterns sep expr], where [total] is the region for the whole
   lambda (even if the resulting lambda is actually longer: we want to
   keep the region of the original), and the region of the original
   "fun" keyword.
*)
(*
type sep = Region.t

val norm : ?reg:(Region.t * kwd_fun) -> pattern Utils.nseq -> sep -> expr -> fun_expr
*)
(* Undoing the above rewritings (for debugging by comparison with the
   lexer, and to feed the source-to-source transformations with only
   tokens that originated from the original input.

     Unparsing is performed on an expression which is expected to be a
   series "fun ... -> fun ... -> ...". Either this expression is the
   right-hand side of a let, or it is not. These two cases are
   distinguished by the function [unparse], depending on the first
   keyword "fun" being concrete or ghostly (virtual). In the former
   case, we are unparsing an expression which was originally starting
   with "fun"; in the latter, we are unparsing an expression that was
   parsed on the right-hand side of a let construct. In other words,
   in the former case, we expect to reconstruct

                    let f p_1 ... p_n = e

   whereas, in the second case, we want to obtain

                    fun p_1 ... p_n -> e

     In any case, the heart of the unparsing is the same, and this is
   why the data constructors [`Fun] and [`Let] of the type [unparsed]
   share a common type: [pattern * Region.t * expr], the region can
   either actually denote the alias type [arrow] or [eq]. Let us
   assume a value of this triple [patterns, separator_region,
   expression]. Then the context (handled by [unparse]) decides if
   [separator_region] is the region of a "=" sign or "->".

   There are two forms to be unparsed:

     fun x_1 -> let p_1 = x_1 in ... fun x_n -> let p_n = x_n in e

   or

     fun p_1 -> ... fun p_n -> e

   in the first case, the rightmost "=" becomes [separator_region]
   above, whereas, in the second case, it is the rightmost "->".

   Here are some example covering all cases:

   let rec f = fun a -> fun b -> a
   let rec g = fun a b -> a
   let rec h = fun a (b,c) -> a
   let rec fst = fun (x,_) -> x

   let rec g a b = a
   let rec h (b,c) a (d,e) = a
   let len = (fun n _ -> n)
   let f l = let n = l in n
*)

(* Projecting regions from sundry nodes of the AST. See the first
   comment at the beginning of this file. *)

val pattern_to_region   : pattern -> Region.t
val expr_to_region      : expr -> Region.t
val type_expr_to_region : type_expr -> Region.t

(* Simplifications *)

(* The call [unpar e] is the expression [e] if [e] is not
   parenthesised, otherwise it is the non-parenthesised expressions it
   contains. *)

val unpar : expr -> expr
