(* Concrete Syntax Tree (CST) for CameLIGO *)

(* To disable warning about multiply-defined record labels. *)

[@@@warning "-30"] (* multiply-defined record labels *)

module Types = Cst_shared.Types

(* Vendor dependencies *)

module Directive = Types.Directive
module Utils     = Types.Utils
module Region    = Types.Region

(* Local dependencies *)

module Wrap = Types.Wrap
module Attr = Types.Attr

(* Utilities *)

type 'a reg = 'a Types.reg
type 'payload wrap = 'payload Types.wrap

open Utils

(* Lexemes *)

type lexeme = Types.lexeme

(* Keywords of CameLIGO *)

open Types

(* IMPORTANT: The types are sorted alphabetically. If you add or
   modify some, please make sure they remain in order. *)

type kwd_begin     = lexeme wrap
type kwd_do        = lexeme wrap
type kwd_done      = lexeme wrap
type kwd_downto    = lexeme wrap [@@deriving yojson_of]
type kwd_else      = lexeme wrap
type kwd_end       = lexeme wrap
type kwd_false     = lexeme wrap [@@deriving yojson_of]
type kwd_for       = lexeme wrap
type kwd_fun       = lexeme wrap
type kwd_if        = lexeme wrap
type kwd_in        = lexeme wrap
type kwd_include   = lexeme wrap
type kwd_let       = lexeme wrap [@@deriving yojson_of]
type kwd_land      = lexeme wrap [@@deriving yojson_of]
type kwd_lor       = lexeme wrap [@@deriving yojson_of]
type kwd_lxor      = lexeme wrap [@@deriving yojson_of]
type kwd_lsl       = lexeme wrap [@@deriving yojson_of]
type kwd_lsr       = lexeme wrap [@@deriving yojson_of]
type kwd_match     = lexeme wrap
type kwd_mod       = lexeme wrap [@@deriving yojson_of]
type kwd_module    = lexeme wrap
type kwd_mut       = lexeme wrap
type kwd_not       = lexeme wrap [@@deriving yojson_of]
type kwd_of        = lexeme wrap
type kwd_or        = lexeme wrap [@@deriving yojson_of]
type kwd_rec       = lexeme wrap
type kwd_signature = lexeme wrap
type kwd_sig       = lexeme wrap
type kwd_struct    = lexeme wrap
type kwd_then      = lexeme wrap
type kwd_true      = lexeme wrap [@@deriving yojson_of]
type kwd_type      = lexeme wrap
type kwd_upto      = lexeme wrap [@@deriving yojson_of]
type kwd_val       = lexeme wrap
type kwd_while     = lexeme wrap
type kwd_with      = lexeme wrap

(* Symbols *)

(* IMPORTANT: The types are sorted alphabetically. If you add or
   modify some, please make sure they remain in order. *)

type arrow    = lexeme wrap                         (* -> *)
type ass      = lexeme wrap [@@deriving yojson_of]  (* := *)
type append   = lexeme wrap                         (* @  *)
type bool_or  = lexeme wrap                         (* || *)
type bool_and = lexeme wrap [@@deriving yojson_of]  (* && *)
type caret    = lexeme wrap [@@deriving yojson_of]  (* ^  *)
type colon    = lexeme wrap                         (* :  *)
type comma    = lexeme wrap                         (* ,  *)
type cons     = lexeme wrap [@@deriving yojson_of]  (* :: *)
type dot      = lexeme wrap                         (* .  *)
type equal    = lexeme wrap [@@deriving yojson_of]  (* =  *)
type geq      = lexeme wrap [@@deriving yojson_of]  (* >= *)
type gt       = lexeme wrap [@@deriving yojson_of]  (* >  *)
type lbrace   = lexeme wrap                         (* {  *)
type lbracket = lexeme wrap                         (* [  *)
type leq      = lexeme wrap [@@deriving yojson_of]  (* =< *)
type lpar     = lexeme wrap                         (* (  *)
type lt       = lexeme wrap [@@deriving yojson_of]  (* <  *)
type minus    = lexeme wrap [@@deriving yojson_of]  (* -  *)
type neq      = lexeme wrap [@@deriving yojson_of]  (* <> *)
type plus     = lexeme wrap [@@deriving yojson_of]  (* +  *)
type quote    = lexeme wrap                         (* '  *)
type rbracket = lexeme wrap                         (* ]  *)
type rbrace   = lexeme wrap                         (* }  *)
type rev_app  = lexeme wrap [@@deriving yojson_of]  (* |> *)
type rpar     = lexeme wrap                         (* )  *)
type semi     = lexeme wrap                         (* ;  *)
type slash    = lexeme wrap [@@deriving yojson_of]  (* /  *)
type times    = lexeme wrap [@@deriving yojson_of]  (* *  *)
type vbar     = lexeme wrap                         (* |  *)
type plus_eq  = lexeme wrap                         (* += *)
type minus_eq = lexeme wrap                         (* -= *)
type times_eq = lexeme wrap                         (* *= *)
type slash_eq = lexeme wrap                         (* /= *)
type vbar_eq  = lexeme wrap                         (* |= *)

(* End-of-File *)

type eof = lexeme wrap
[@@deriving yojson_of]

(* Literals *)

type variable =
  Var of lexeme wrap (* foo  *)
| Esc of lexeme wrap (* @foo without the @ *)

let yojson_of_variable : variable -> Yojson.Safe.t = function
  Var wrapped_lexeme -> yojson_of_wrap yojson_of_lexeme wrapped_lexeme
| Esc wrapped_lexeme -> yojson_of_wrap yojson_of_lexeme wrapped_lexeme

type field_name    = variable [@@deriving yojson_of]
type type_name     = variable [@@deriving yojson_of]
type type_variable = variable [@@deriving yojson_of]

type language      = lexeme reg wrap [@@deriving yojson_of]
type ctor          = lexeme wrap [@@deriving yojson_of]
type module_name   = lexeme wrap [@@deriving yojson_of]
type attribute     = Attr.t wrap

type bytes_literal =
  (lexeme * (Hex.t [@yojson.opaque])) wrap [@@deriving yojson_of]

type int_literal =
  (lexeme * (Z.t [@yojson.opaque])) wrap [@@deriving yojson_of]

type mutez_literal =
  (lexeme * (Int64.t [@yojson.opaque])) wrap [@@deriving yojson_of]

type nat_literal      = int_literal [@@deriving yojson_of]
type string_literal   = lexeme wrap [@@deriving yojson_of]
type verbatim_literal = lexeme wrap [@@deriving yojson_of]

(* Parentheses, braces, brackets *)

type 'a par'= {
  lpar   : (lpar [@yojson.opaque]);
  inside : 'a;
  rpar   : (rpar [@yojson.opaque])
} [@@deriving yojson_of]

type 'a par = 'a par' reg [@@deriving yojson_of]

type 'a braces'= {
  lbrace : (lbrace [@yojson.opaque]);
  inside : 'a;
  rbrace : (rbrace [@yojson.opaque])
} [@@deriving yojson_of]

type 'a braces = 'a braces' reg [@@deriving yojson_of]

type 'a brackets' = {
  lbracket : (lbracket [@yojson.opaque]);
  inside   : 'a;
  rbracket : (rbracket [@yojson.opaque])
} [@@deriving yojson_of]

type 'a brackets = 'a brackets' reg [@@deriving yojson_of]

(* The Abstract Syntax Tree *)

type t = {
  decl : declaration nseq;
  eof  : eof
}

and cst = t [@@deriving yojson_of]

(* DECLARATIONS (top-level) *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and declaration =
  D_Attr      of ((attribute [@yojson.opaque]) * declaration) reg
| D_Directive of (Directive.t [@yojson.opaque])
| D_Let       of let_decl reg
| D_Module    of module_decl reg
| D_Include   of module_include reg
| D_Signature of signature_decl reg
| D_Type      of type_decl reg

(* Non-recursive, top-level values *)

and let_decl =
  (kwd_let [@yojson.opaque])
  * (kwd_rec [@yojson.opaque]) option
  * let_binding

and let_binding = {
  binders     : pattern nseq;
  type_params : type_params par option;
  rhs_type    : type_annotation option;
  eq          : equal [@yojson.opaque];
  let_rhs     : expr
}

(* Type parameters *)

and type_params = (kwd_type [@yojson.opaque]) * type_variable nseq

(* Module declaration *)

and module_decl = {
  kwd_module  : (kwd_module [@yojson.opaque]);
  name        : module_name;
  annotation  : ((colon [@yojson.opaque]) * signature_expr) option;
  eq          : equal [@yojson.opaque];
  module_expr : module_expr
}

and module_expr =
  M_Body of module_body reg             (* Structure definition *)
| M_Path of module_name module_path reg (* Module selection     *)
| M_Var  of module_name                 (* Module aliasing      *)

and module_body = {
  kwd_struct   : (kwd_struct [@yojson.opaque]);
  declarations : declaration list;
  kwd_end      : (kwd_end [@yojson.opaque])
}

and module_include = {
  kwd_include : (kwd_include [@yojson.opaque]);
  module_expr : module_expr
}

(* Signature declaration *)

and signature_decl = {
  kwd_module     : (kwd_module [@yojson.opaque]);
  kwd_type       : (kwd_type [@yojson.opaque]);
  name           : module_name;
  eq             : equal [@yojson.opaque];
  signature_expr : signature_expr
}

and signature_expr =
  S_Sig  of signature_body reg
| S_Path of module_name module_path reg
| S_Var  of module_name

and signature_body = {
  kwd_sig   : (kwd_sig [@yojson.opaque]);
  sig_items : sig_item list;
  kwd_end   : (kwd_end [@yojson.opaque])
}

and sig_item =
  S_Attr    of sig_attr reg
| S_Include of sig_include reg
| S_Type    of sig_type reg
| S_Value   of sig_value reg

and sig_attr = (attribute [@yojson.opaque]) * sig_item

and sig_include = (kwd_include [@yojson.opaque]) * signature_expr

and sig_type = {
  kwd_type  : (kwd_type [@yojson.opaque]);
  type_vars : type_vars option;
  type_name : type_name;
  type_rhs  : ((equal [@yojson.opaque]) * type_expr) option
}

and sig_value = {
  kwd_val  : (kwd_val [@yojson.opaque]);
  var      : variable;
  colon    : (colon [@yojson.opaque]);
  val_type : type_expr
}

(* Module paths *)

and 'a module_path = {
  module_path : (module_name, (dot [@yojson.opaque])) nsepseq;
  selector    : (dot [@yojson.opaque]);
  field       : 'a
}

(* Type declaration *)

and type_decl = {
  kwd_type  : (kwd_type [@yojson.opaque]);
  params    : type_vars option;
  name      : type_name;
  eq        : equal [@yojson.opaque];
  type_expr : type_expr
}

and type_vars =
  TV_Single of type_var
| TV_Tuple  of type_var tuple par

and type_var =
  ((quote option [@yojson.opaque]) * type_variable) reg  (* 'a or ' a or _ *)

and 'a tuple = ('a, (comma [@yojson.opaque])) nsepseq

(* TYPE EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and type_expr =
  T_App         of (type_expr * type_ctor_arg) reg (* M.t (x,y,z)     *)
| T_Arg         of type_var                        (* 'a              *)
| T_Attr        of type_attr                       (* [@a]            *)
| T_Cart        of cartesian reg                   (* x * (y * z)     *)
| T_ForAll      of for_all reg                     (* 'a.'a -> 'a     *)
| T_Fun         of fun_type reg                    (* x -> y          *)
| T_Int         of int_literal                     (* 42              *)
| T_ModPath     of type_expr module_path reg       (* A.B.(x * y)     *)
| T_Par         of type_expr par                   (* (t)             *)
| T_ParameterOf of parameter_of reg                (* parameter_of M  *)
| T_Record      of field_decl reg record           (* {a; [@x] b: t}  *)
| T_String      of string_literal                  (* "x"             *)
| T_Var         of type_variable                   (* x   @x          *)
| T_Variant     of variant_type reg                (* [@a] A | B of t *)

(* Type application *)

and type_ctor_arg =
  TC_Single of type_expr
| TC_Tuple  of type_expr tuple par

(* Type attribute *)

and type_attr = (attribute [@yojson.opaque]) * type_expr

(* Cartesian type *)

and cartesian =
  type_expr
  * (times [@yojson.opaque])
  * (type_expr,(times [@yojson.opaque])) nsepseq

(* Parametric type *)

and for_all = type_var nseq * (dot [@yojson.opaque]) * type_expr

(* Functional type *)

and fun_type = type_expr * (arrow [@yojson.opaque]) * type_expr

(* Parameter-of *)

and parameter_of = (module_name, (dot [@yojson.opaque])) nsepseq

(* Record type *)

and 'a record = ('a, (semi [@yojson.opaque])) sepseq braces

and field_decl = {
  attributes : (attribute list [@yojson.opaque]);
  field_name : field_name;
  field_type : type_annotation option (* Type punning if [None] *)
}

and type_annotation = (colon [@yojson.opaque]) * type_expr

(* Variant type *)

and variant_type = {
  lead_vbar : (vbar option [@yojson.opaque]);
  variants  : (variant reg, (vbar [@yojson.opaque])) nsepseq
}

and variant = {
  attributes : (attribute list [@yojson.opaque]);
  ctor       : ctor;
  ctor_args  : ((kwd_of [@yojson.opaque]) * type_expr) option
}

(* PATTERNS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and pattern =
  P_App      of (pattern * pattern option) reg  (* M.C (x,y) *)
| P_Attr     of attr_pattern                    (* [@var] x  *)
| P_Bytes    of bytes_literal                   (* 0xFFFA    *)
| P_Cons     of cons_pattern reg                (* x :: y    *)
| P_Ctor     of ctor                            (* C         *)
| P_False    of kwd_false                       (* false     *)
| P_Int      of int_literal                     (* 42        *)
| P_List     of pattern list_                   (* [x; 4]    *)
| P_ModPath  of pattern module_path reg         (* M.N.x     *)
| P_Mutez    of mutez_literal                   (* 5mutez    *)
| P_Nat      of nat_literal                     (* 4n        *)
| P_Par      of pattern par                     (* (C, 4)    *)
| P_Record   of record_pattern                  (* {x=y; z}  *)
| P_String   of string_literal                  (* "string"  *)
| P_True     of kwd_true                        (* true      *)
| P_Tuple    of pattern tuple reg               (* 1, x      *)
| P_Typed    of typed_pattern reg               (* (x : int) *)
| P_Unit     of the_unit reg                    (* ()        *)
| P_Var      of variable                        (* x  @x     *)
| P_Verbatim of verbatim_literal                (* {|foo|}   *)

(* Attributed pattern *)

and attr_pattern = (attribute [@yojson.opaque]) * pattern

(* List consing pattern *)

and cons_pattern = pattern * (cons [@yojson.opaque]) * pattern

(* List pattern *)

and 'a list_ = ('a, (semi [@yojson.opaque])) sepseq brackets

(* Record pattern *)

and record_pattern = (field_name, equal, pattern) field record

and ('lhs, 'lens, 'rhs) field =
  Punned   of 'lhs punned reg
| Complete of ('lhs, 'lens, 'rhs) full_field reg

and 'lhs punned = {
  attributes : (attribute list [@yojson.opaque]);
  pun        : 'lhs;
}

and ('lhs, 'lens, 'rhs) full_field = {
  attributes : (attribute list [@yojson.opaque]);
  field_lhs  : 'lhs;
  field_lens : ('lens [@yojson.opaque]);
  field_rhs  : 'rhs
}

(* Typed pattern *)

and typed_pattern = pattern * type_annotation

(* Unit pattern *)

and the_unit = lpar * rpar [@yojson.opaque]

(* EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and expr =
  E_Add        of plus bin_op reg        (* x + y                         *)
| E_And        of bool_and bin_op reg    (* x && y                        *)
| E_App        of (expr * expr nseq) reg (* f x y     C (x,y)             *)
| E_Assign     of assign reg             (* x := e                        *)
| E_Attr       of attr_expr              (* [@a] e                        *)
| E_Bytes      of bytes_literal          (* 0xFFFA                        *)
| E_Cat        of caret bin_op reg       (* "Hello" ^ world               *)
| E_CodeInj    of code_inj reg
| E_Cond       of cond_expr reg          (* if x then y                   *)
| E_Cons       of cons bin_op reg        (* head :: tail                  *)
| E_ContractOf of contract_of            (* contract_of M                 *)
| E_Ctor       of ctor                   (* C                             *)
| E_Div        of slash bin_op reg       (* x / y                         *)
| E_Equal      of equal bin_op reg       (* x = y                         *)
| E_False      of kwd_false              (* false                         *)
| E_For        of for_loop reg           (* for x = e1 upto e2 do e3 done *)
| E_ForIn      of for_in_loop reg        (* for x in e1 do e2 done        *)
| E_Fun        of fun_expr reg           (* fun x -> x                    *)
| E_Geq        of geq bin_op reg         (* x >= y                        *)
| E_Gt         of gt bin_op reg          (* x > y                         *)
| E_Int        of int_literal            (* 42                            *)
| E_Land       of kwd_land bin_op reg    (* x land y                      *)
| E_Leq        of leq bin_op reg         (* x <= y                        *)
| E_LetIn      of let_in reg             (* let x = e1 in e2              *)
| E_LetMutIn   of let_mut_in reg         (* let mut x = e1 in e2          *)
| E_List       of expr list_             (* [f x; 5]                      *)
| E_Lor        of kwd_lor bin_op reg     (* x lor y                       *)
| E_Lsl        of kwd_lsl bin_op reg     (* x lsl y                       *)
| E_Lsr        of kwd_lsr bin_op reg     (* x lsr y                       *)
| E_Lt         of lt bin_op reg          (* x < y                         *)
| E_Lxor       of kwd_lxor bin_op reg    (* x lxor y                      *)
| E_Match      of match_expr reg         (* match e with p -> i           *)
| E_Mod        of kwd_mod bin_op reg     (* x mod n                       *)
| E_ModIn      of module_in reg          (* module M = N in e             *)
| E_ModPath    of expr module_path reg   (* M.N.x.0                       *)
| E_Mult       of times bin_op reg       (* x * y                         *)
| E_Mutez      of mutez_literal          (* 5mutez                        *)
| E_Nat        of nat_literal            (* 4n                            *)
| E_Neg        of minus un_op reg        (* -a                            *)
| E_Neq        of neq bin_op reg         (* x <> y                        *)
| E_Not        of kwd_not un_op reg      (* not x                         *)
| E_Or         of kwd_or bin_op reg      (* x or y                        *)
| E_Par        of expr par               (* (x - M.y)                     *)
| E_Proj       of projection reg         (* e.x.1                         *)
| E_Record     of record_expr            (* {x=y; z}                      *)
| E_RevApp     of rev_app bin_op reg     (* y |> f |> g x                 *)
| E_Seq        of sequence_expr reg      (* x; 3                          *)
| E_String     of string_literal         (* "string"                      *)
| E_Sub        of minus bin_op reg       (* a - b                         *)
| E_True       of kwd_true               (* true                          *)
| E_Tuple      of expr tuple reg         (* (1, x)                        *)
| E_Typed      of typed_expr par         (* (x : int)                     *)
| E_TypeIn     of type_in reg            (* type t = u in e               *)
| E_Unit       of the_unit reg           (* ()                            *)
| E_Update     of update_expr braces     (* {x with y=z}                  *)
| E_Var        of variable               (* x  @x                         *)
| E_Verbatim   of verbatim_literal       (* {|foo|}                       *)
| E_While      of while_loop reg         (* while e1 do e2 done           *)

(* Binary and unary arithmetic operators *)

and 'a bin_op = {arg1: expr; op: 'a; arg2: expr}
and 'a  un_op = {op: 'a; arg: expr}

(* Typed expression *)

and typed_expr = expr * type_annotation

(* Sequence expression *)

and sequence_expr = {
  compound : (compound option [@yojson.opaque]);
  elements : (expr, (semi [@yojson.opaque])) sepseq
}

and compound =
  BeginEnd of (kwd_begin * kwd_end [@yojson.opaque])
| Parens   of (lpar * rpar [@yojson.opaque])          (* TODO in parser *)

(* Projection *)

and projection = {
  record_or_tuple : expr;
  selector        : (dot [@yojson.opaque]);
  field_path      : (selection, (dot [@yojson.opaque])) nsepseq
}

and selection =
  FieldName of field_name
| Component of int_literal

(* Record expression *)

and record_expr = (field_name, equal, expr) field record

(* Functional update of records *)

and update_expr = {
  record   : expr;
  kwd_with : (kwd_with [@yojson.opaque]);
  updates  : ((path, lens, expr) field, (semi [@yojson.opaque])) nsepseq
}

and path =
  Name of variable
| Path of projection reg

and lens =
  Lens_Id   of (equal [@yojson.opaque])
| Lens_Add  of (plus_eq [@yojson.opaque])
| Lens_Sub  of (minus_eq [@yojson.opaque])
| Lens_Mult of (times_eq [@yojson.opaque])
| Lens_Div  of (slash_eq [@yojson.opaque])
| Lens_Fun  of (vbar_eq [@yojson.opaque])

(* Pattern matching *)

and match_expr = {
  kwd_match : (kwd_match [@yojson.opaque]);
  subject   : expr;
  kwd_with  : (kwd_with [@yojson.opaque]);
  lead_vbar : (vbar option [@yojson.opaque]);
  clauses   : (match_clause reg, (vbar [@yojson.opaque])) nsepseq reg
}

and match_clause = {
  pattern : pattern;
  arrow   : (arrow [@yojson.opaque]);
  rhs     : expr
}

(* Local value definition *)

and let_in = {
  kwd_let : kwd_let;
  kwd_rec : (kwd_rec [@yojson.opaque]) option;
  binding : let_binding reg;
  kwd_in  : (kwd_in [@yojson.opaque]);
  body    : expr
}

(* Mutable value definition *)

and let_mut_in = {
  kwd_let : kwd_let;
  kwd_mut : (kwd_mut [@yojson.opaque]);
  binding : let_binding reg;
  kwd_in  : (kwd_in [@yojson.opaque]);
  body    : expr
}

(* Mutable value assignement *)

and assign = {
  binder : variable;
  ass    : ass;
  expr   : expr
}

(* Attributed expression *)

and attr_expr = (attribute [@yojson.opaque]) * expr

(* Local type definition *)

and type_in = {
  type_decl : type_decl reg;
  kwd_in    : (kwd_in [@yojson.opaque]);
  body      : expr
}

(* Local module definition *)

and module_in = {
  mod_decl : module_decl reg;
  kwd_in   : (kwd_in [@yojson.opaque]);
  body     : expr
}

(* Functional expression (a.k.a. lambda) *)

and fun_expr = {
  kwd_fun     : (kwd_fun [@yojson.opaque]);
  type_params : type_params par option;
  binders     : pattern nseq;
  rhs_type    : type_annotation option;
  arrow       : (arrow [@yojson.opaque]);
  body        : expr
}

(* Conditional expression *)

and cond_expr = {
  kwd_if   : (kwd_if [@yojson.opaque]);
  test     : expr;
  kwd_then : (kwd_then [@yojson.opaque]);
  if_so    : expr;
  if_not   : ((kwd_else [@yojson.opaque]) * expr) option
}

(* Code injection. Note how the field [language] wraps a region in
   another: the outermost region covers the header "[%<language>" and
   the innermost covers the <language>. *)

and code_inj = {
  language : language;
  code     : expr;
  rbracket : (rbracket [@yojson.opaque])
}

(* Contract-of *)

and contract_of = (module_name, (dot [@yojson.opaque])) nsepseq reg

(* Loops *)

and for_loop = {
  kwd_for   : (kwd_for [@yojson.opaque]);
  index     : variable;
  equal     : equal;
  bound1    : expr;
  direction : direction;
  bound2    : expr;
  body      : loop_body reg
}

and while_loop = {
  kwd_while : (kwd_while [@yojson.opaque]);
  cond      : expr;
  body      : loop_body reg
}

and for_in_loop = {
  kwd_for    : (kwd_for [@yojson.opaque]);
  pattern    : pattern;
  kwd_in     : (kwd_in [@yojson.opaque]);
  collection : expr;
  body       : loop_body reg
}

and direction =
  Upto   of kwd_upto
| Downto of kwd_downto

and loop_body = {
  kwd_do   : (kwd_do [@yojson.opaque]);
  seq_expr : (expr, (semi [@yojson.opaque])) sepseq;
  kwd_done : (kwd_done [@yojson.opaque])
}

(* PROJECTING REGIONS *)

let rec last to_region = function
    [] -> Region.ghost
|  [x] -> to_region x
| _::t -> last to_region t

let nseq_to_region to_region (hd, tl) =
  Region.cover (to_region hd) (last to_region tl)

let nsepseq_to_region to_region (hd, tl) =
  Region.cover (to_region hd) (last (to_region <@ snd) tl)

let sepseq_to_region to_region = function
      None -> Region.ghost
| Some seq -> nsepseq_to_region to_region seq

let variable_to_region = function
  Var w | Esc w -> w#region

let rec type_expr_to_region = function
  T_Arg     {region; _}
| T_App     {region; _} -> region
| T_Attr    (_, e) -> type_expr_to_region e
| T_Cart    {region; _}
| T_ForAll  {region; _}
| T_Fun     {region; _} -> region
| T_Int     w -> w#region
| T_ModPath {region; _}
| T_Par     {region; _}
| T_ParameterOf {region; _}
| T_Record  {region; _} -> region
| T_String  w -> w#region
| T_Variant {region; _} -> region
| T_Var     w -> variable_to_region w

let rec pattern_to_region = function
  P_App      {region; _} -> region
| P_Attr     (_, p) -> pattern_to_region p
| P_Bytes    p -> p#region
| P_Cons     {region; _} -> region
| P_Ctor     p -> p#region
| P_False    p -> p#region
| P_Int      p -> p#region
| P_List     {region; _}
| P_ModPath  {region; _} -> region
| P_Mutez    p -> p#region
| P_Nat      p -> p#region
| P_Par      {region; _}
| P_Record   {region; _} -> region
| P_String   p -> p#region
| P_True     p -> p#region
| P_Tuple    {region; _}
| P_Typed    {region; _} -> region
| P_Var      p -> variable_to_region p
| P_Verbatim p -> p#region
| P_Unit     {region; _} -> region

let rec expr_to_region = function
  E_Add        {region; _}
| E_And        {region; _}
| E_App        {region; _} -> region
| E_Attr       (_, e) -> expr_to_region e
| E_Bytes      e -> e#region
| E_Cat        {region; _}
| E_CodeInj    {region; _}
| E_Cond       {region; _} -> region
| E_Ctor       e -> e#region
| E_Cons       {region; _}
| E_ContractOf {region; _}
| E_Div        {region; _}
| E_Equal      {region; _} -> region
| E_False      e -> e#region
| E_Fun        {region; _}
| E_Geq        {region; _}
| E_Gt         {region; _} -> region
| E_Int        e -> e#region
| E_Land       {region; _}
| E_LetIn      {region; _}
| E_LetMutIn   {region; _}
| E_Assign     {region; _}
| E_Leq        {region; _}
| E_List       {region; _}
| E_Lor        {region; _}
| E_Lsl        {region; _}
| E_Lsr        {region; _}
| E_Lt         {region; _}
| E_Lxor       {region; _}
| E_Match      {region; _}
| E_Mod        {region; _}
| E_ModIn      {region; _}
| E_ModPath    {region; _}
| E_Mult       {region; _} -> region
| E_Mutez      e -> e#region
| E_Nat        e -> e#region
| E_Neg        {region; _}
| E_Neq        {region; _}
| E_Not        {region; _}
| E_Or         {region; _}
| E_Par        {region; _}
| E_Proj       {region; _}
| E_Record     {region; _} -> region
| E_String     e -> e#region
| E_Sub        {region; _} -> region
| E_True       e -> e#region
| E_Tuple      {region; _}
| E_Typed      {region; _}
| E_TypeIn     {region; _}
| E_Unit       {region; _}
| E_Update     {region; _} -> region
| E_Var        e -> variable_to_region e
| E_Verbatim   e -> e#region
| E_Seq        {region; _}
| E_RevApp     {region; _} -> region
| E_While      {region; _}
| E_For        {region; _}
| E_ForIn      {region; _} -> region

let selection_to_region = function
  FieldName v -> variable_to_region v
| Component c -> c#region

let path_to_region = function
  Name v -> variable_to_region v
| Path p -> p.region

let type_ctor_arg_to_region = function
  TC_Single e -> type_expr_to_region e
| TC_Tuple  p -> p.region

let module_expr_to_region = function
  M_Body {region; _}
| M_Path {region; _} -> region
| M_Var e -> e#region

let declaration_to_region = function
  D_Attr      {region; _} -> region
| D_Directive dir         -> Directive.to_region dir
| D_Let       {region; _}
| D_Module    {region; _}
| D_Include   {region; _}
| D_Signature {region; _}
| D_Type      {region; _} -> region

let sig_item_to_region = function
  S_Attr    {region; _}
| S_Value   {region; _}
| S_Type    {region; _}
| S_Include {region; _} -> region

let signature_expr_to_region = function
  S_Sig  {region; _}
| S_Path {region; _} -> region
| S_Var e -> e#region
