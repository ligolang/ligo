(* Concrete Syntax Tree (CST) for CameLIGO *)

(* To disable warning about multiply-defined record labels. *)

[@@@warning "-30"] (* multiply-defined record labels *)

(* Vendor dependencies *)

module Directive = Preprocessor.Directive
module Utils     = Simple_utils.Utils
module Region    = Simple_utils.Region

(* Local dependencies *)

module Wrap = Lexing_shared.Wrap
module Attr = Lexing_shared.Attr

(* Utilities *)

type 'a reg = 'a Region.reg
type 'payload wrap = 'payload Wrap.t

open Utils

(* Lexemes *)

type lexeme = string

(* Keywords of CameLIGO *)

(* IMPORTANT: The types are sorted alphabetically. If you add or
   modify some, please make sure they remain in order. *)

type kwd_begin  = lexeme wrap
type kwd_else   = lexeme wrap
type kwd_end    = lexeme wrap
type kwd_false  = lexeme wrap
type kwd_fun    = lexeme wrap
type kwd_if     = lexeme wrap
type kwd_in     = lexeme wrap
type kwd_let    = lexeme wrap
type kwd_land   = lexeme wrap
type kwd_lor    = lexeme wrap
type kwd_lxor   = lexeme wrap
type kwd_lsl    = lexeme wrap
type kwd_lsr    = lexeme wrap
type kwd_match  = lexeme wrap
type kwd_mod    = lexeme wrap
type kwd_module = lexeme wrap
type kwd_not    = lexeme wrap
type kwd_of     = lexeme wrap
type kwd_or     = lexeme wrap
type kwd_rec    = lexeme wrap
type kwd_struct = lexeme wrap
type kwd_then   = lexeme wrap
type kwd_true   = lexeme wrap
type kwd_type   = lexeme wrap
type kwd_with   = lexeme wrap
type kwd_mut    = lexeme wrap
type kwd_for    = lexeme wrap
type kwd_while  = lexeme wrap
type kwd_upto   = lexeme wrap
type kwd_downto = lexeme wrap
type kwd_do     = lexeme wrap
type kwd_done   = lexeme wrap

(* Symbols *)

(* IMPORTANT: The types are sorted alphabetically. If you add or
   modify some, please make sure they remain in order. *)

type arrow    = lexeme wrap  (* -> *)
type ass      = lexeme wrap  (* := *)
type append   = lexeme wrap  (* @  *)
type bool_or  = lexeme wrap  (* || *)
type bool_and = lexeme wrap  (* && *)
type caret    = lexeme wrap  (* ^  *)
type colon    = lexeme wrap  (* :  *)
type comma    = lexeme wrap  (* ,  *)
type cons     = lexeme wrap  (* :: *)
type dot      = lexeme wrap  (* .  *)
type equal    = lexeme wrap  (* =  *)
type geq      = lexeme wrap  (* >= *)
type gt       = lexeme wrap  (* >  *)
type lbrace   = lexeme wrap  (* {  *)
type lbracket = lexeme wrap  (* [  *)
type leq      = lexeme wrap  (* =< *)
type lpar     = lexeme wrap  (* (  *)
type lt       = lexeme wrap  (* <  *)
type minus    = lexeme wrap  (* -  *)
type neq      = lexeme wrap  (* <> *)
type plus     = lexeme wrap  (* +  *)
type quote    = lexeme wrap  (* '  *)
type rbracket = lexeme wrap  (* ]  *)
type rbrace   = lexeme wrap  (* }  *)
type rev_app  = lexeme wrap  (* |> *)
type rpar     = lexeme wrap  (* )  *)
type semi     = lexeme wrap  (* ;  *)
type slash    = lexeme wrap  (* /  *)
type times    = lexeme wrap  (* *  *)
type vbar     = lexeme wrap  (* |  *)
type plus_eq  = lexeme wrap  (* += *)
type minus_eq = lexeme wrap  (* -= *)
type times_eq = lexeme wrap  (* *= *)
type slash_eq = lexeme wrap  (* /= *)
type vbar_eq  = lexeme wrap  (* |= *)

(* End-of-File *)

type eof = lexeme wrap

(* Literals *)

type variable    = lexeme wrap
type module_name = lexeme wrap
type field_name  = lexeme wrap
type ctor        = lexeme wrap
type language    = lexeme Region.reg wrap
type attribute   = Attr.t wrap

(* Parentheses, braces, brackets *)

type 'a par'      = {lpar: lpar; inside: 'a; rpar: rpar}
type 'a par       = 'a par' reg
type 'a braces'   = {lbrace: lbrace; inside: 'a; rbrace: rbrace}
type 'a braces    = 'a braces' reg
type 'a brackets' = {lbracket: lbracket; inside: 'a; rbracket: rbracket}
type 'a brackets  = 'a brackets' reg

(* The Abstract Syntax Tree *)

type t = {
  decl : declaration nseq;
  eof  : eof
}

and cst = t

(* DECLARATIONS (top-level) *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and declaration =
  D_Attr      of (attribute * declaration) reg
| D_Directive of Directive.t
| D_Let       of let_decl reg
| D_Module    of module_decl reg
| D_Type      of type_decl reg

(* Non-recursive, top-level values *)

and let_decl = kwd_let * kwd_rec option * let_binding

and let_binding = {
  binders     : pattern nseq;
  type_params : type_params par option;
  rhs_type    : type_annotation option;
  eq          : equal;
  let_rhs     : expr
}

(* Type parameters *)

and type_params = kwd_type * variable nseq

(* Module declaration *)

and module_decl = {
  kwd_module  : kwd_module;
  name        : module_name;
  eq          : equal;
  module_expr : module_expr
}

and module_expr =
  M_Body of module_body reg             (* Structure definition *)
| M_Path of module_name module_path reg (* Module selection     *)
| M_Var  of module_name                 (* Module aliasing      *)

and module_body = {
  kwd_struct   : kwd_struct;
  declarations : declaration list;
  kwd_end      : kwd_end
}

(* Module paths *)

and 'a module_path = {
  module_path : (module_name, dot) nsepseq;
  selector    : dot;
  field       : 'a
}

(* Type declaration *)

and type_decl = {
  kwd_type  : kwd_type;
  params    : type_vars option;
  name      : variable;
  eq        : equal;
  type_expr : type_expr
}

and type_vars =
  TV_Single of type_var
| TV_Tuple  of type_var tuple par

and type_var = (quote option * variable) reg  (* 'a or ' a or _ *)

and 'a tuple = ('a, comma) nsepseq

(* TYPE EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and type_expr =
  T_App       of (type_expr * type_ctor_arg) reg     (* M.t (x,y,z)     *)
| T_Arg       of type_var                            (* 'a              *)
| T_Attr      of (attribute * type_expr)             (* [@a] x          *)
| T_Cart      of cartesian                           (* x * (y * z)     *)
| T_Fun       of (type_expr * arrow * type_expr) reg (* x -> y          *)
| T_Int       of (lexeme * Z.t) wrap                 (* 42              *)
| T_ModPath   of type_expr module_path reg           (* A.B.(x * y)     *)
| T_Par       of type_expr par                       (* (t)             *)
| T_Record    of field_decl reg record               (* {a; [@x] b: t}  *)
| T_String    of lexeme wrap                         (* "x"             *)
| T_Variant   of variant_type reg                    (* [@a] A | B of t *)
| T_Var       of variable                            (* x               *)
| T_Parameter of (module_name, dot) nsepseq reg      (* parameter_of m  *)

(* Type application *)

and type_ctor_arg =
  TC_Single of type_expr
| TC_Tuple  of type_expr tuple par

(* Cartesian type *)

and cartesian = (type_expr * times * (type_expr,times) nsepseq) reg

(* Record type *)

and 'a record = ('a, semi) sepseq braces

and field_decl = {
  attributes : attribute list;
  field_name : field_name;
  field_type : type_annotation option (* Type punning if [None] *)
}

and type_annotation = colon * type_expr

(* Variant type *)

and variant_type = {
  lead_vbar : vbar option;
  variants  : (variant reg, vbar) nsepseq
}

and variant = {
  attributes : attribute list;
  ctor       : ctor;
  ctor_args  : (kwd_of * type_expr) option
}

(* PATTERNS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and pattern =
  P_App      of (pattern * pattern option) reg  (* M.C (x,y) *)
| P_Attr     of (attribute * pattern)           (* [@var] x  *)
| P_Bytes    of (lexeme * Hex.t) wrap           (* 0xFFFA    *)
| P_Cons     of (pattern * cons * pattern) reg  (* x :: y    *)
| P_Ctor     of ctor                            (* C         *)
| P_Int      of (lexeme * Z.t) wrap             (* 42        *)
| P_List     of pattern list_                   (* [x; 4]    *)
| P_ModPath  of pattern module_path reg         (* M.N.x     *)
| P_Mutez    of (lexeme * Int64.t) wrap         (* 5mutez    *)
| P_Nat      of (lexeme * Z.t) wrap             (* 4n        *)
| P_Par      of pattern par                     (* (C, 4)    *)
| P_Record   of record_pattern                  (* {x=y; z}  *)
| P_String   of lexeme wrap                     (* "string"  *)
| P_Tuple    of pattern tuple reg               (* (1, x)    *)
| P_Typed    of typed_pattern reg               (* (x : int) *)
| P_Var      of variable                        (* x         *)
| P_Verbatim of lexeme wrap                     (* {|foo|}   *)
| P_Unit     of the_unit reg                    (* ()        *)

(* List pattern *)

and 'a list_ = ('a, semi) sepseq brackets

(* Record pattern *)

and record_pattern = (field_name, equal, pattern) field record

and ('lhs, 'lens, 'rhs) field =
  Punned   of 'lhs punned reg
| Complete of ('lhs, 'lens, 'rhs) full_field reg

and 'lhs punned = {
  attributes : attribute list;
  pun        : 'lhs;
}

and ('lhs, 'lens, 'rhs) full_field = {
  attributes : attribute list;
  field_lhs  : 'lhs;
  field_lens : 'lens;
  field_rhs  : 'rhs
}

(* Typed pattern *)

and typed_pattern = pattern * type_annotation

(* Unit pattern *)

and the_unit = lpar * rpar

(* EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and expr =
  E_Add      of plus bin_op reg          (* x + y               *)
| E_And      of bool_and bin_op reg      (* x && y              *)
| E_App      of (expr * expr nseq) reg   (* f x y     C (x,y)   *)
| E_Assign   of assign reg               (* x := e *)
| E_Attr     of (attribute * expr)       (* [@a] e              *)
| E_Bytes    of (lexeme * Hex.t) wrap    (* 0xFFFA              *)
| E_Cat      of caret bin_op reg         (* "Hello" ^ world     *)
| E_CodeInj  of code_inj reg
| E_Cond     of cond_expr reg            (* if x then y         *)
| E_Cons     of cons bin_op reg          (* head :: tail        *)
| E_Contract of (module_name, dot) nsepseq reg (* contract_of M *)
| E_Ctor     of ctor                     (* C                   *)
| E_Div      of slash bin_op reg         (* x / y               *)
| E_Equal    of equal bin_op reg         (* x = y               *)
| E_For      of for_loop reg             (* for x = e1 upto e2 do e3 done *)
| E_ForIn    of for_in_loop reg          (* for x in e1 do e2 done *)
| E_Fun      of fun_expr reg             (* fun x -> x          *)
| E_Geq      of geq bin_op reg           (* x >= y              *)
| E_Gt       of gt bin_op reg            (* x > y               *)
| E_Int      of (lexeme * Z.t) wrap      (* 42                  *)
| E_Land     of kwd_land bin_op reg      (* x land y            *)
| E_Leq      of leq bin_op reg           (* x <= y              *)
| E_LetIn    of let_in reg               (* let x = e1 in e2    *)
| E_LetMutIn of let_mut_in reg           (* let mut x = e1 in e2 *)
| E_List     of expr list_               (* [f x; 5]            *)
| E_Lor      of kwd_lor bin_op reg       (* x lor y             *)
| E_Lsl      of kwd_lsl bin_op reg       (* x lsl y             *)
| E_Lsr      of kwd_lsr bin_op reg       (* x lsr y             *)
| E_Lt       of lt bin_op reg            (* x < y               *)
| E_Lxor     of kwd_lxor bin_op reg      (* x lxor y            *)
| E_Match    of match_expr reg           (* match e with p -> i *)
| E_Mod      of kwd_mod bin_op reg       (* x mod n             *)
| E_ModIn    of module_in reg            (* module M = N in e   *)
| E_ModPath  of expr module_path reg     (* M.N.x.0             *)
| E_Mult     of times bin_op reg         (* x * y               *)
| E_Mutez    of (lexeme * Int64.t) wrap  (* 5mutez              *)
| E_Nat      of (lexeme * Z.t) wrap      (* 4n                  *)
| E_Neg      of minus un_op reg          (* -a                  *)
| E_Neq      of neq bin_op reg           (* x <> y              *)
| E_Not      of kwd_not un_op reg        (* not x               *)
| E_Or       of kwd_or bin_op reg        (* x or y              *)
| E_Par      of expr par                 (* (x - M.y)           *)
| E_Proj     of projection reg           (* e.x.1               *)
| E_Record   of record_expr              (* {x=y; z}            *)
| E_String   of lexeme wrap              (* "string"            *)
| E_Sub      of minus bin_op reg         (* a - b               *)
| E_Tuple    of expr tuple reg           (* (1, x)              *)
| E_Typed    of typed_expr par           (* (x : int)           *)
| E_TypeIn   of type_in reg              (* type t = u in e     *)
| E_Unit     of the_unit reg             (* ()                  *)
| E_Update   of update_expr braces       (* {x with y=z}        *)
| E_Var      of variable                 (* x                   *)
| E_Verbatim of lexeme wrap              (* {|foo|}             *)
| E_Seq      of sequence_expr reg        (* x; 3                *)
| E_RevApp   of rev_app bin_op reg       (* y |> f |> g x       *)
| E_While    of while_loop reg           (* while e1 do e2 done *)

(* Binary and unary arithmetic operators *)

and 'a bin_op = {arg1: expr; op: 'a; arg2: expr}
and 'a  un_op = {op: 'a; arg: expr}

(* Typed expression *)

and typed_expr = expr * type_annotation

(* Sequence expression *)

and sequence_expr = {
  compound : compound option;
  elements : (expr, semi) sepseq
}

and compound =
  BeginEnd of kwd_begin * kwd_end
| Parens   of lpar * rpar          (* TODO in parser *)

(* Projection *)

and projection = {
  record_or_tuple : expr;
  selector        : dot;
  field_path      : (selection, dot) nsepseq
}

and selection =
  FieldName of field_name
| Component of (lexeme * Z.t) wrap

(* Record expression *)

and record_expr = (field_name, equal, expr) field record

(* Functional update of records *)

and update_expr = {
  record   : expr;
  kwd_with : kwd_with;
  updates  : ((path, lens, expr) field, semi) nsepseq
}

and path =
  Name of variable
| Path of projection reg

and lens =
  Lens_Id   of equal
| Lens_Add  of plus_eq
| Lens_Sub  of minus_eq
| Lens_Mult of times_eq
| Lens_Div  of slash_eq
| Lens_Fun  of vbar_eq

(* Pattern matching *)

and match_expr = {
  kwd_match : kwd_match;
  subject   : expr;
  kwd_with  : kwd_with;
  lead_vbar : vbar option;
  clauses   : (match_clause reg, vbar) nsepseq reg
}

and match_clause = {
  pattern : pattern;
  arrow   : arrow;
  rhs     : expr
}

(* Local value definition *)

and let_in = {
  kwd_let : kwd_let;
  kwd_rec : kwd_rec option;
  binding : let_binding;
  kwd_in  : kwd_in;
  body    : expr
}

(* Mutable value definition *)

and let_mut_in = {
  kwd_let : kwd_let;
  kwd_mut : kwd_mut;
  binding : let_binding;
  kwd_in  : kwd_in;
  body    : expr
}

(* Mutable value assignement *)

and assign = {
  binder : variable;
  ass    : ass;
  expr   : expr
}

(* Local type definition *)

and type_in = {
  type_decl : type_decl;
  kwd_in    : kwd_in;
  body      : expr
}

(* Local module definition *)

and module_in = {
  mod_decl : module_decl;
  kwd_in   : kwd_in;
  body     : expr
}

(* Functional expression (a.k.a. lambda) *)

and fun_expr = {
  kwd_fun     : kwd_fun;
  type_params : type_params par option;
  binders     : pattern nseq;
  rhs_type    : type_annotation option;
  arrow       : arrow;
  body        : expr
}

(* Conditional expression *)

and cond_expr = {
  kwd_if   : kwd_if;
  test     : expr;
  kwd_then : kwd_then;
  if_so    : expr;
  if_not   : (kwd_else * expr) option
}

(* Code injection. Note how the field [language] wraps a region in
   another: the outermost region covers the header "[%<language>" and
   the innermost covers the <language>. *)

and code_inj = {
  language : language;
  code     : expr;
  rbracket : rbracket
}

and for_loop = {
  kwd_for   : kwd_for;
  index     : variable;
  equal     : equal;
  bound1    : expr;
  direction : direction;
  bound2    : expr;
  body      : loop_body reg
}

and while_loop = {
  kwd_while : kwd_while;
  cond      : expr;
  body      : loop_body reg
}

and for_in_loop = {
  kwd_for     : kwd_for;
  pattern     : pattern;
  kwd_in      : kwd_in;
  collection  : expr;
  body        : loop_body reg
}

and direction = 
  | Upto of kwd_upto
  | Downto of kwd_downto

and loop_body = {
  kwd_do    : kwd_do;
  seq_expr  : (expr, semi) nsepseq option;
  kwd_done  : kwd_done
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

let rec type_expr_to_region = function
  T_Arg     {region; _}
| T_App     {region; _} -> region
| T_Attr    (_, e) -> type_expr_to_region e
| T_Cart    {region; _}
| T_Fun     {region; _} -> region
| T_Int     w -> w#region
| T_ModPath {region; _}
| T_Par     {region; _}
| T_Record  {region; _} -> region
| T_String  w -> w#region
| T_Variant {region; _} -> region
| T_Var     w -> w#region
| T_Parameter {region; _} -> region

let rec pattern_to_region = function
  P_App      {region; _} -> region
| P_Attr     (_, p) -> pattern_to_region p
| P_Bytes    p -> p#region
| P_Cons     {region; _} -> region
| P_Ctor     p -> p#region
| P_Int      p -> p#region
| P_List     {region; _}
| P_ModPath  {region; _} -> region
| P_Mutez    p -> p#region
| P_Nat      p -> p#region
| P_Par      {region; _}
| P_Record   {region; _} -> region
| P_String   p -> p#region
| P_Tuple    {region; _}
| P_Typed    {region; _} -> region
| P_Var      p -> p#region
| P_Verbatim p -> p#region
| P_Unit     {region; _} -> region

let rec expr_to_region = function
  E_Add      {region; _}
| E_And      {region; _}
| E_App      {region; _} -> region
| E_Attr     (_, e) -> expr_to_region e
| E_Bytes    e -> e#region
| E_Cat      {region; _}
| E_CodeInj  {region; _}
| E_Cond     {region; _} -> region
| E_Ctor     e -> e#region
| E_Cons     {region; _}
| E_Contract {region; _}
| E_Div      {region; _}
| E_Equal    {region; _}
| E_Fun      {region; _}
| E_Geq      {region; _}
| E_Gt       {region; _} -> region
| E_Int      e -> e#region
| E_Land     {region; _}
| E_LetIn    {region; _}
| E_LetMutIn {region; _}
| E_Assign   {region; _}
| E_Leq      {region; _}
| E_List     {region; _}
| E_Lor      {region; _}
| E_Lsl      {region; _}
| E_Lsr      {region; _}
| E_Lt       {region; _}
| E_Lxor     {region; _}
| E_Match    {region; _}
| E_Mod      {region; _}
| E_ModIn    {region; _}
| E_ModPath  {region; _}
| E_Mult     {region; _} -> region
| E_Mutez    e -> e#region
| E_Nat      e -> e#region
| E_Neg      {region; _}
| E_Neq      {region; _}
| E_Not      {region; _}
| E_Or       {region; _}
| E_Par      {region; _}
| E_Proj     {region; _}
| E_Record   {region; _} -> region
| E_String   e -> e#region
| E_Sub      {region; _}
| E_Tuple    {region; _}
| E_Typed    {region; _}
| E_TypeIn   {region; _}
| E_Unit     {region; _}
| E_Update   {region; _} -> region
| E_Var      e -> e#region
| E_Verbatim e -> e#region
| E_Seq      {region; _}
| E_RevApp   {region; _} -> region
| E_While    {region; _}
| E_For      {region; _}
| E_ForIn    {region; _} -> region

let selection_to_region = function
  FieldName v -> v#region
| Component c -> c#region

let path_to_region = function
  Name v -> v#region
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
| D_Type      {region; _} -> region
