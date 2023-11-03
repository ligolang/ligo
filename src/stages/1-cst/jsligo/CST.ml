(* Concrete Syntax Tree (CST) for JsLIGO *)

(* Disabling warnings *)

[@@@warning "-30"] (* multiply-defined record labels *)

module Types = Cst_shared.Types

(* Vendor dependencies *)

module Directive = Types.Directive
module Utils     = Types.Utils
module Region    = Types.Region
module Token     = Lexing_jsligo.Token

(* Local dependencies *)

module Wrap  = Types.Wrap
module Attr  = Types.Attr
module Nodes = Cst_shared.Nodes

(* Utilities *)

type 'a reg = 'a Region.reg
type 'payload wrap = 'payload Types.wrap

open Utils

type lexeme = string

(* Keywords of JsLIGO *)

open Types

(* IMPORTANT: The keywords are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

type kwd_as           = lexeme wrap
type kwd_break        = lexeme wrap [@@deriving yojson_of]
type kwd_case         = lexeme wrap
type kwd_const        = lexeme wrap
type kwd_continue     = lexeme wrap [@@deriving yojson_of]
type kwd_contract_of  = lexeme wrap
type kwd_default      = lexeme wrap
type kwd_do           = lexeme wrap
type kwd_else         = lexeme wrap
type kwd_export       = lexeme wrap
type kwd_extends      = lexeme wrap
type kwd_false        = lexeme wrap [@@deriving yojson_of]
type kwd_for          = lexeme wrap
type kwd_from         = lexeme wrap
type kwd_function     = lexeme wrap
type kwd_if           = lexeme wrap
type kwd_implements   = lexeme wrap
type kwd_import       = lexeme wrap
type kwd_interface    = lexeme wrap
type kwd_let          = lexeme wrap
type kwd_match        = lexeme wrap
type kwd_namespace    = lexeme wrap
type kwd_of           = lexeme wrap
type kwd_parameter_of = lexeme wrap
type kwd_return       = lexeme wrap
type kwd_switch       = lexeme wrap
type kwd_true         = lexeme wrap [@@deriving yojson_of]
type kwd_type         = lexeme wrap
type kwd_when         = lexeme wrap
type kwd_while        = lexeme wrap

(* Symbols *)

type arrow      = lexeme wrap  (* =>  *)
type bit_and_eq = lexeme wrap [@@deriving yojson_of]  (* &=  *)
type bit_and    = lexeme wrap [@@deriving yojson_of]  (* &   *)
type bit_neg    = lexeme wrap [@@deriving yojson_of]  (* ~   *)
type bit_or_eq  = lexeme wrap [@@deriving yojson_of]  (* |=  *)
type bit_or     = lexeme wrap [@@deriving yojson_of]  (* |   *)
type bit_sl_eq  = lexeme wrap [@@deriving yojson_of]  (* <<= *)
type bit_sl     = lexeme wrap [@@deriving yojson_of]  (* <<  *)
type bit_sr_eq  = lexeme wrap [@@deriving yojson_of]  (* >>= *)
type bit_sr     = lexeme wrap [@@deriving yojson_of]  (* >>  *)
type bit_xor_eq = lexeme wrap [@@deriving yojson_of]  (* ^=  *)
type bit_xor    = lexeme wrap [@@deriving yojson_of]  (* ^   *)
type bool_and   = lexeme wrap [@@deriving yojson_of]  (* &&  *)
type bool_neg   = lexeme wrap [@@deriving yojson_of]  (* !   *)
type bool_or    = lexeme wrap [@@deriving yojson_of]  (* ||  *)
type bool_xor   = lexeme wrap [@@deriving yojson_of]  (* ^^  *)
type colon      = lexeme wrap  (* :   *)
type comma      = lexeme wrap  (* ,   *)
type decrement  = lexeme wrap [@@deriving yojson_of]  (* --  *)
type div_eq     = lexeme wrap [@@deriving yojson_of]  (* /=  *)
type dot        = lexeme wrap  (* .   *)
type ellipsis   = lexeme wrap  (* ... *)
type equal_cmp  = lexeme wrap [@@deriving yojson_of]  (* ==  *)
type equal      = lexeme wrap [@@deriving yojson_of]  (* =   *)
type geq        = lexeme wrap [@@deriving yojson_of]  (* >=  *)
type gt         = lexeme wrap [@@deriving yojson_of]  (* >   *)
type increment  = lexeme wrap [@@deriving yojson_of]  (* ++  *)
type lbrace     = lexeme wrap  (* {   *)
type lbracket   = lexeme wrap  (* [   *)
type leq        = lexeme wrap [@@deriving yojson_of]  (* <=  *)
type lpar       = lexeme wrap  (* (   *)
type lt         = lexeme wrap [@@deriving yojson_of]  (* <   *)
type minus_eq   = lexeme wrap [@@deriving yojson_of]  (* -=  *)
type minus      = lexeme wrap [@@deriving yojson_of]  (* -   *)
type neq        = lexeme wrap [@@deriving yojson_of]  (* !=  *)
type plus_eq    = lexeme wrap [@@deriving yojson_of]  (* +=  *)
type plus       = lexeme wrap [@@deriving yojson_of]  (* +   *)
type qmark      = lexeme wrap [@@deriving yojson_of]  (* ?   *)
type rbrace     = lexeme wrap  (* }   *)
type rbracket   = lexeme wrap  (* ]   *)
type remainder  = lexeme wrap [@@deriving yojson_of]  (* %   *)
type rem_eq     = lexeme wrap [@@deriving yojson_of]  (* %=  *)
type rpar       = lexeme wrap  (* )   *)
type semi       = lexeme wrap  (* ;   *)
type sharp      = lexeme wrap  (* #   *)
type slash      = lexeme wrap [@@deriving yojson_of]  (* /   *)
type times_eq   = lexeme wrap [@@deriving yojson_of]  (* *=  *)
type times      = lexeme wrap [@@deriving yojson_of]  (* *   *)
type vbar       = lexeme wrap  (* |   *)
type wild       = lexeme wrap  (* _   *)

type property_sep = lexeme wrap  (* , ; *)

(* End-Of-File *)

type eof = lexeme wrap [@@deriving yojson_of]

(* Literals *)

type variable =
  Var of lexeme wrap (* foo  *)
| Esc of lexeme wrap (* @foo without the @ *)

let yojson_of_variable : variable -> Yojson.Safe.t = function
  Var wrapped_lexeme -> yojson_of_wrap yojson_of_lexeme wrapped_lexeme
| Esc wrapped_lexeme -> yojson_of_wrap yojson_of_lexeme wrapped_lexeme

type fun_name       = variable [@@deriving yojson_of]
type type_name      = variable [@@deriving yojson_of]
type type_var       = variable [@@deriving yojson_of]
type type_ctor      = variable [@@deriving yojson_of]
type property_name  = variable [@@deriving yojson_of]
type generic        = variable [@@deriving yojson_of]

type language       = lexeme wrap [@@deriving yojson_of]
type ctor           = lexeme wrap [@@deriving yojson_of]
type namespace_name = lexeme wrap [@@deriving yojson_of] (* N *)
type intf_name      = lexeme wrap [@@deriving yojson_of] (* I *)
type file_path      = lexeme wrap [@@deriving yojson_of]
type attribute      = Attr.t wrap

type bytes_literal    = (lexeme * (Hex.t [@yojson.opaque])) wrap [@@deriving yojson_of]
type int_literal      = (lexeme * (Z.t [@yojson.opaque])) wrap [@@deriving yojson_of]
type mutez_literal    = (lexeme * (Int64.t [@yojson.opaque])) wrap [@@deriving yojson_of]
type nat_literal      = int_literal [@@deriving yojson_of]
type string_literal   = lexeme wrap [@@deriving yojson_of]
type verbatim_literal = lexeme wrap [@@deriving yojson_of]

(* Parentheses, braces, brackets *)

type 'a par'      = {lpar: (lpar [@yojson.opaque]); inside: 'a; rpar: (rpar [@yojson.opaque])} [@@deriving yojson_of]
type 'a par       = 'a par' reg [@@deriving yojson_of]
type 'a braces'   = {lbrace: (lbrace [@yojson.opaque]); inside: 'a; rbrace: (rbrace [@yojson.opaque])} [@@deriving yojson_of]
type 'a braces    = 'a braces' reg [@@deriving yojson_of]
type 'a brackets' = {lbracket: (lbracket [@yojson.opaque]); inside: 'a; rbracket: (rbracket [@yojson.opaque])} [@@deriving yojson_of]
type 'a brackets  = 'a brackets' reg [@@deriving yojson_of]
type 'a chevrons' = {lchevron: (lt [@yojson.opaque]); inside: 'a; rchevron: (gt [@yojson.opaque])} [@@deriving yojson_of]
type 'a chevrons  = 'a chevrons' reg [@@deriving yojson_of]

(* The Abstract Syntax Tree *)

type t = {statements : statements; eof : eof}

and cst = t
[@@deriving yojson_of]

(* DECLARATIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and declaration =
  D_Fun       of fun_decl reg
| D_Import    of import_decl
| D_Interface of interface_decl reg
| D_Namespace of namespace_decl reg
| D_Type      of type_decl reg
| D_Value     of value_decl reg

(* Function declaration *)

and fun_decl = {
  kwd_function : (kwd_function [@yojson.opaque]);
  fun_name     : fun_name;
  generics     : generics option;
  parameters   : fun_params;
  rhs_type     : type_annotation option;
  fun_body     : statements braces
}

and fun_params = (pattern, (comma [@yojson.opaque])) sep_or_term par

(* Import declaration *)

and import_decl =
  ImportAlias of import_alias reg
| ImportAllAs of import_all_as reg
| ImportFrom  of import_from reg

and import_alias = {
  kwd_import     : (kwd_import [@yojson.opaque]);
  alias          : namespace_name;
  equal          : (equal [@yojson.opaque]);
  namespace_path : namespace_selection
}

and namespace_selection =
  M_Path  of namespace_name namespace_path reg
| M_Alias of namespace_name

and import_all_as = {
  kwd_import : (kwd_import [@yojson.opaque]);
  times      : (times [@yojson.opaque]);
  kwd_as     : (kwd_as [@yojson.opaque]);
  alias      : namespace_name;
  kwd_from   : (kwd_from [@yojson.opaque]);
  file_path  : file_path
}

and import_from = {
  kwd_import : (kwd_import [@yojson.opaque]);
  imported   : (variable, (comma [@yojson.opaque])) sep_or_term braces;
  kwd_from   : (kwd_from [@yojson.opaque]);
  file_path  : file_path
}

(* Namespace paths *)

and 'a namespace_path = {
  namespace_path : (namespace_name, (dot [@yojson.opaque])) nsepseq;
  selector       : (dot [@yojson.opaque]);
  property       : 'a
}

(* Interfaces *)

and interface_decl = {
  kwd_interface : (kwd_interface [@yojson.opaque]);
  intf_name     : intf_name;
  intf_extends  : extends option;
  intf_body     : intf_body
}

and intf_body = intf_entries braces

and intf_entries = (intf_entry, (semi [@yojson.opaque])) sep_or_term

and intf_entry =
  I_Attr  of ((attribute [@yojson.opaque]) * intf_entry)
| I_Type  of intf_type reg
| I_Const of intf_const reg

and intf_type = {
  kwd_type  : (kwd_type [@yojson.opaque]);
  type_name : type_name;
  type_rhs  : ((equal [@yojson.opaque]) * type_expr) option
}

and intf_const = {
  kwd_const  : (kwd_const [@yojson.opaque]);
  const_name : variable;
  const_optional : (qmark option [@yojson.opaque]);
  const_type : type_annotation
}

and extends = ((kwd_extends [@yojson.opaque]) * (intf_expr, (comma [@yojson.opaque])) nsepseq) reg

(* Namespace declaration *)

and namespace_decl = {
  kwd_namespace  : (kwd_namespace [@yojson.opaque]);
  namespace_name : namespace_name;
  namespace_type : interface option;
  namespace_body : statements braces
}

and interface = ((kwd_implements [@yojson.opaque]) * (intf_expr, (comma [@yojson.opaque])) nsepseq) reg

and intf_expr =
  I_Body of intf_body
| I_Path of namespace_selection

(* Type declarations *)

and type_decl = {
  kwd_type  : (kwd_type [@yojson.opaque]);
  name      : type_name;
  generics  : generics option;
  eq        : (equal [@yojson.opaque]);
  type_expr : type_expr
}

(* Value declaration *)

and value_decl = {
  kind     : var_kind;
  bindings : (val_binding reg, (comma [@yojson.opaque])) nsepseq
}

and var_kind = [
  `Let   of (kwd_let [@yojson.opaque])
| `Const of (kwd_const [@yojson.opaque])
]

and val_binding = {
  pattern   : pattern;
  generics  : generics option;
  rhs_type  : type_annotation option;
  eq        : (equal [@yojson.opaque]);
  rhs_expr  : expr
}

and generics = (generic, (comma [@yojson.opaque])) sep_or_term chevrons

and type_annotation = (colon [@yojson.opaque]) * type_expr

(* TYPE EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and type_expr =
  T_App         of (type_expr * type_ctor_args) reg           (* M.t<u,v>       *)
| T_Attr        of ((attribute [@yojson.opaque]) * type_expr) (* @a e           *)
| T_Array       of array_type                                 (* [t, [u, v]]    *)
| T_Fun         of fun_type                                   (* (a : t) => u   *)
| T_Int         of int_literal                                (* 42             *)
| T_NamePath    of type_expr namespace_path reg               (* A.B.list<u>    *)
| T_Object      of type_expr _object                          (* {x; @a y : t}  *)
| T_Par         of type_expr par                              (* (t)            *)
| T_ParameterOf of parameter_of_type reg                      (* parameter_of m *)
| T_String      of string_literal                             (* "x"            *)
| T_Union       of union_type                              (* {kind: "C", x: t} *)
| T_Var         of type_var                                (* t                 *)
| T_Variant     of variant_type                            (* ["A"] | ["B", t]  *)

(* Type application *)

and type_ctor_args = (type_expr, (comma [@yojson.opaque])) nsep_or_term chevrons

(* Array type type *)

and array_type = (type_expr, (comma [@yojson.opaque])) nsep_or_term brackets

(* Functional type *)

and fun_type        = (fun_type_params * (arrow [@yojson.opaque]) * type_expr) reg
and fun_type_params = (fun_type_param reg, (comma [@yojson.opaque])) sep_or_term par
and fun_type_param  = pattern * type_annotation

(* Parameter of type *)

and parameter_of_type = {
  kwd_parameter_of : (kwd_parameter_of [@yojson.opaque]);
  namespace_path   : namespace_selection
}

(* Object type *)

and 'a _object = ('a property reg, (property_sep [@yojson.opaque])) sep_or_term braces

and 'a property = {
  attributes   : (attribute list [@yojson.opaque]);
  property_id  : property_id;
  property_rhs : ((colon [@yojson.opaque]) * 'a) option (* [None] means punning *)
}

and property_id =
  F_Int  of int_literal
| F_Name of property_name
| F_Str  of string_literal

(* Discriminated unions *)

and union_type = (type_expr _object, (vbar [@yojson.opaque])) nsep_or_pref reg

(* Variant type *)

and variant_type = (type_expr variant_kind, (vbar [@yojson.opaque])) nsep_or_pref reg

and 'a variant_kind =
  Variant   of 'a variant reg
| Bracketed of 'a bracketed_variant reg
| Legacy    of 'a legacy_variant reg

and 'a variant = {
  attributes : (attribute list [@yojson.opaque]);
  tuple      : 'a ctor_app
}

and 'a bracketed_variant = {
  attributes : (attribute list [@yojson.opaque]);
  sharp      : (sharp [@yojson.opaque]);
  tuple      : 'a bracketed_variant_args brackets
}

and 'a bracketed_variant_args = {
  ctor : 'a;
  args : ((comma [@yojson.opaque]) * ('a, (comma [@yojson.opaque])) sep_or_term) option
}

and 'a legacy_variant = {
  attributes : (attribute list [@yojson.opaque]);
  tuple      : 'a legacy_variant_args brackets
}

and 'a legacy_variant_args = {
  ctor : string_literal;
  args : ((comma [@yojson.opaque]) * 'a) list
}

(* Data constructor applications *)

and 'a ctor_app = (sharp option [@yojson.opaque]) * 'a app

and ctor_app_kind =
  CtorStr  of string_literal
| CtorName of ctor

and 'a app =
  ZeroArg of ctor_app_kind
| MultArg of (ctor_app_kind * ('a, (comma [@yojson.opaque])) nsep_or_term par)

(* Do-expressions *)

and do_expr = {
  kwd_do     : (kwd_do [@yojson.opaque]);
  statements : statements braces
}

(* PATTERNS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and pattern =
  P_Array    of pattern _array                           (* [x, ...y, z] [] *)
| P_Attr     of ((attribute [@yojson.opaque]) * pattern) (* @a [x, _]       *)
| P_Bytes    of bytes_literal                            (* 0xFFFA          *)
| P_CtorApp  of pattern variant_kind                     (* #["C",4]        *)
| P_False    of kwd_false                                (* false           *)
| P_Int      of int_literal                              (* 42              *)
| P_Mutez    of mutez_literal                            (* 5mutez          *)
| P_NamePath of pattern namespace_path reg               (* M.N.{x, y : 0}  *)
| P_Nat      of nat_literal                              (* 4n              *)
| P_Object   of pattern _object                          (* {x, y : 0}      *)
| P_String   of string_literal                           (* "string"        *)
| P_True     of kwd_true                                 (* true            *)
| P_Typed    of typed_pattern reg                        (* [x,y] : t       *)
| P_Var      of variable                                 (* x  @x           *)
| P_Verbatim of verbatim_literal                         (* {|foo|}         *)

(* Array pattern *)

and 'a _array = ('a element, (comma [@yojson.opaque])) sep_or_term brackets

and 'a element = (ellipsis option [@yojson.opaque]) * 'a

(* Typed patterns (function parameters) *)

and typed_pattern = pattern * type_annotation

(* STATEMENTS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and statement =
  S_Attr      of ((attribute [@yojson.opaque]) * statement)
| S_Block     of statements braces
| S_Break     of kwd_break
| S_Continue  of kwd_continue
| S_Decl      of declaration
| S_Directive of (Directive.t [@yojson.opaque])
| S_Export    of export_stmt reg
| S_Expr      of expr
| S_For       of for_stmt reg
| S_ForOf     of for_of_stmt reg
| S_If        of if_stmt reg
| S_Return    of return_stmt reg
| S_Switch    of switch_stmt reg
| S_While     of while_stmt reg

and statements = (statement * (semi option [@yojson.opaque])) nseq

(* Export statement *)

and export_stmt = (kwd_export [@yojson.opaque]) * declaration

(* Conditional statement *)

and if_stmt = {
  kwd_if : (kwd_if [@yojson.opaque]);
  test   : expr par;
  if_so  : statement * (semi option [@yojson.opaque]);
  if_not : ((kwd_else [@yojson.opaque]) * statement) option
}

(* For-loops *)

and for_stmt = {
  kwd_for  : (kwd_for [@yojson.opaque]);
  range    : range_for par;
  for_body : statement option
}

and range_for = {
  initialiser  : statement option;
  semi1        : (semi [@yojson.opaque]);
  condition    : expr option;
  semi2        : (semi [@yojson.opaque]);
  afterthought : (expr, (comma [@yojson.opaque])) nsepseq option
}

(* For-of loops *)

and for_of_stmt = {
  kwd_for     : (kwd_for [@yojson.opaque]);
  range       : range_of par;
  for_of_body : statement
}

and range_of = {
  index_kind : (var_kind [@yojson.opaque]);
  index      : variable;
  kwd_of     : (kwd_of [@yojson.opaque]);
  expr       : expr
}

(* Return statement *)

and return_stmt = (kwd_return [@yojson.opaque]) * expr option

(* Switch statement *)

and switch_stmt = {
  kwd_switch : (kwd_switch [@yojson.opaque]);
  subject    : expr par;
  cases      : cases braces
}

and cases =
  AllCases of all_cases
| Default  of switch_default reg

and all_cases = switch_case reg nseq * switch_default reg option

and switch_case = {
  kwd_case  : (kwd_case [@yojson.opaque]);
  expr      : expr;
  colon     : (colon [@yojson.opaque]);
  case_body : statements option
}

and switch_default = {
  kwd_default  : (kwd_default [@yojson.opaque]);
  colon        : (colon [@yojson.opaque]);
  default_body : statements option
}

(* While-loop *)

and while_stmt = {
  kwd_while  : (kwd_while [@yojson.opaque]);
  invariant  : expr par;
  while_body : statement
}

(* EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and expr =
  E_Add        of plus bin_op reg                       (* x + y             *)
| E_AddEq      of plus_eq bin_op reg                    (* x += y            *)
| E_And        of bool_and bin_op reg                   (* x && y            *)
| E_App        of (expr * arguments) reg                (* f(x,y)  foo()     *)
| E_Array      of expr _array                           (* [x, ...y, z]  []  *)
| E_ArrowFun   of arrow_fun_expr reg                    (* (x : int) => e    *)
| E_Assign     of equal bin_op reg                      (* x = y             *)
| E_Attr       of ((attribute [@yojson.opaque]) * expr) (* @a [x, y]         *)
| E_BitAnd     of bit_and bin_op reg                    (* x & y             *)
| E_BitAndEq   of bit_and_eq bin_op reg                 (* x &= y            *)
| E_BitNeg     of bit_neg un_op reg                     (* ~x                *)
| E_BitOr      of bit_or bin_op reg                     (* x | y             *)
| E_BitOrEq    of bit_or_eq bin_op reg                  (* x |= y            *)
| E_BitSl      of bit_sl bin_op reg                     (* x << y            *)
| E_BitSlEq    of bit_sl_eq bin_op reg                  (* x <<= y           *)
| E_BitSr      of bit_sr bin_op reg                     (* x >> y            *)
| E_BitSrEq    of bit_sr_eq bin_op reg                  (* x >>= y           *)
| E_BitXor     of bit_xor bin_op reg                    (* x ^ y             *)
| E_BitXorEq   of bit_xor_eq bin_op reg                 (* x ^= y            *)
| E_Bytes      of bytes_literal                         (* 0xFFFA            *)
| E_CodeInj    of code_inj reg
| E_ContractOf of contract_of_expr reg                  (* contract_of (M.N) *)
| E_CtorApp    of expr variant_kind                     (* #["C",4]          *)
| E_Div        of slash bin_op reg                      (* x / y             *)
| E_DivEq      of div_eq bin_op reg                     (* x /= y            *)
| E_Do         of do_expr reg                           (* do { return 4 }   *)
| E_Equal      of equal_cmp bin_op reg                  (* x == y            *)
| E_False      of kwd_false                             (* false             *)
| E_Function   of function_expr reg                     (* function (x) {...} *)
| E_Geq        of geq bin_op reg                        (* x >= y            *)
| E_Gt         of gt bin_op reg                         (* x > y             *)
| E_Int        of int_literal                           (* 42                *)
| E_Leq        of leq bin_op reg                        (* x <= y            *)
| E_Lt         of lt bin_op reg                         (* x < y             *)
| E_Match      of match_expr reg                        (* match (e) { ... } *)
| E_Mult       of times bin_op reg                      (* x * y             *)
| E_MultEq     of times_eq bin_op reg                   (* x *= y            *)
| E_Mutez      of mutez_literal                         (* 5mutez            *)
| E_NamePath   of expr namespace_path reg               (* M.N.x.0           *)
| E_Nat        of nat_literal                           (* 42n               *)
| E_Neg        of minus un_op reg                       (* -x                *)
| E_Neq        of neq bin_op reg                        (* x != y            *)
| E_Not        of bool_neg un_op reg                    (* !x                *)
| E_Object     of expr _object                          (* {x : e, y}        *)
| E_Or         of bool_or bin_op reg                    (* x || y            *)
| E_Par        of expr par                              (* (x + y)           *)
| E_PostDecr   of decrement un_op reg                   (* x--               *)
| E_PostIncr   of increment un_op reg                   (* x++               *)
| E_PreDecr    of decrement un_op reg                   (* --x               *)
| E_PreIncr    of increment un_op reg                   (* ++x               *)
| E_Proj       of projection reg                        (* e.x.1             *)
| E_Rem        of remainder bin_op reg                  (* x % n             *)
| E_RemEq      of rem_eq bin_op reg                     (* x %= y            *)
| E_String     of string_literal                        (* "abcdef"          *)
| E_Sub        of minus bin_op reg                      (* x - y             *)
| E_SubEq      of minus_eq bin_op reg                   (* x -= y            *)
| E_Ternary    of ternary reg                           (* x ? y : z         *)
| E_True       of kwd_true                              (* true              *)
| E_Typed      of typed_expr reg                        (* e as t            *)
| E_Update     of update_expr braces                    (* {...x, y : z}     *)
| E_Var        of variable                              (* x                 *)
| E_Verbatim   of verbatim_literal                      (* {|foo|}           *)
| E_Xor        of bool_xor bin_op reg                   (* x ^^ y            *)

(* Arguments of function calls *)

and arguments = (expr, (comma [@yojson.opaque])) sepseq par

(* Functional expressions *)

and arrow_fun_expr = {
  generics   : generics option;
  parameters : arrow_fun_params;
  rhs_type   : type_annotation option;
  arrow      : (arrow [@yojson.opaque]);
  fun_body   : fun_body
}

and function_expr = {
  kwd_function : (kwd_function [@yojson.opaque]);
  generics     : generics option;
  parameters   : arrow_fun_params;
  rhs_type     : type_annotation option;
  fun_body     : fun_body
}

and arrow_fun_params =
  ParParams  of fun_params
| NakedParam of pattern

and fun_body =
  StmtBody of statements braces
| ExprBody of expr

(* Pattern matching *)

and match_expr = {
  kwd_match : (kwd_match [@yojson.opaque]);
  subject   : expr par;
  clauses   : match_clauses braces
}

and match_clauses =
  AllClauses    of all_match_clauses
| DefaultClause of match_default reg

and all_match_clauses = match_clause reg nseq * match_default reg option

and match_clause = {
  kwd_when    : (kwd_when [@yojson.opaque]);
  filter      : pattern par;
  colon       : (colon [@yojson.opaque]);
  clause_expr : expr
}

and match_default = {
  kwd_default  : (kwd_default [@yojson.opaque]);
  colon        : (colon [@yojson.opaque]);
  default_expr : expr
}

(* Contract of expression *)

and contract_of_expr = {
  kwd_contract_of : (kwd_contract_of [@yojson.opaque]);
  namespace_path  : namespace_selection par
}

(* Functional update of object expressions *)

and update_expr = {
  ellipsis : (ellipsis [@yojson.opaque]);
  _object  : expr;
  sep      : (property_sep [@yojson.opaque]);
  updates  : (expr property reg, (property_sep [@yojson.opaque])) sep_or_term
}

(* Ternary conditional *)

and ternary = {
  condition : expr;
  qmark     : (qmark [@yojson.opaque]);
  truthy    : expr;
  colon     : (colon [@yojson.opaque]);
  falsy     : expr
}

(* Typed expression *)

and typed_expr = expr * (kwd_as [@yojson.opaque]) * type_expr

(* Binary and unary arithmetic operators *)

and 'a bin_op = {arg1: expr; op: 'a; arg2: expr}
and 'a  un_op = {op: 'a; arg: expr}

(* Projections *)

and projection = {
  object_or_array : expr;
  property_path   : selection nseq
}

and selection =
  PropertyName of ((dot [@yojson.opaque]) * property_name) (* Objects *)
| PropertyStr  of string_literal brackets                  (* Objects *)
| Component    of int_literal brackets                     (* Arrays  *)

(* Code injection.  Note how the field [language] wraps a region in
   another: the outermost region covers the header "[%<language>" and
   the innermost covers the <language>. *)

and code_inj = {
  language : language;
  code     : expr
}

(* PROJECTIONS *)

(* Projecting regions from some nodes of the AST *)

let variant_kind_to_region = function
  Variant   {region; _}
| Bracketed {region; _}
| Legacy    {region; _} -> region

let import_decl_to_region = function
  ImportAlias {region; _}
| ImportAllAs {region; _}
| ImportFrom  {region; _} -> region

let declaration_to_region = function
  D_Fun       {region; _} -> region
| D_Import    d -> import_decl_to_region d
| D_Interface {region; _}
| D_Namespace {region; _}
| D_Type      {region; _} -> region
| D_Value     {region; _} -> region

let variable_to_region = function
  Var w | Esc w -> w#region

let rec type_expr_to_region = function
  T_App         {region; _}
| T_Array       {region; _} -> region
| T_Attr        (_, t) -> type_expr_to_region t
| T_Fun         {region; _} -> region
| T_Int         w -> w#region
| T_NamePath    {region; _}
| T_Object      {region; _}
| T_Par         {region; _}
| T_ParameterOf {region; _} -> region
| T_String      w -> w#region
| T_Union       {region; _} -> region
| T_Var         v -> variable_to_region v
| T_Variant     {region; _} -> region

let rec pattern_to_region = function
  P_Array    {region; _} -> region
| P_Attr     (_, p) -> pattern_to_region p
| P_Bytes    w -> w#region
| P_CtorApp  w -> variant_kind_to_region w
| P_False    w -> w#region
| P_Int      w -> w#region
| P_Mutez    w -> w#region
| P_NamePath {region; _} -> region
| P_Nat      w -> w#region
| P_Object   {region; _} -> region
| P_String   w-> w#region
| P_True     w -> w#region
| P_Typed    {region; _} -> region
| P_Var      v -> variable_to_region v
| P_Verbatim w -> w#region

let rec expr_to_region = function
  E_Add        {region; _}
| E_AddEq      {region; _}
| E_And        {region; _}
| E_App        {region; _}
| E_Array      {region; _}
| E_ArrowFun   {region; _}
| E_Assign     {region; _} -> region
| E_Attr       (_, e) -> expr_to_region e
| E_BitAnd     {region; _}
| E_BitAndEq   {region; _}
| E_BitNeg     {region; _}
| E_BitOr      {region; _}
| E_BitOrEq    {region; _}
| E_BitSl      {region; _}
| E_BitSlEq    {region; _}
| E_BitSr      {region; _}
| E_BitSrEq    {region; _}
| E_BitXor     {region; _}
| E_BitXorEq   {region; _} -> region
| E_Bytes      w -> w#region
| E_CodeInj    {region; _}
| E_ContractOf {region; _} -> region
| E_CtorApp    w -> variant_kind_to_region w
| E_Do         {region; _}
| E_Div        {region; _}
| E_DivEq      {region; _}
| E_Equal      {region; _} -> region
| E_False      w -> w#region
| E_Function   {region; _}
| E_Geq        {region; _}
| E_Gt         {region; _} -> region
| E_Int        w -> w#region
| E_Leq        {region; _}
| E_Lt         {region; _}
| E_Match      {region; _}
| E_Mult       {region; _}
| E_MultEq    {region; _} -> region
| E_Mutez      w -> w#region
| E_NamePath   {region; _} -> region
| E_Nat        w -> w#region
| E_Neg        {region; _}
| E_Neq        {region; _}
| E_Not        {region; _}
| E_Object     {region; _}
| E_Or         {region; _}
| E_Par        {region; _}
| E_PostDecr   {region; _}
| E_PostIncr   {region; _}
| E_PreDecr    {region; _}
| E_PreIncr    {region; _}
| E_Proj       {region; _}
| E_Rem        {region; _}
| E_RemEq      {region; _} -> region
| E_String     w -> w#region
| E_Sub        {region; _}
| E_SubEq      {region; _}
| E_Ternary    {region; _} -> region
| E_True       w -> w#region
| E_Typed      {region; _}
| E_Update     {region; _} -> region
| E_Var        v -> variable_to_region v
| E_Verbatim   w -> w#region
| E_Xor        {region; _} -> region

let rec statement_to_region = function
  S_Attr      (_, s) -> statement_to_region s
| S_Block     {region; _} -> region
| S_Break     w -> w#region
| S_Continue  w -> w#region
| S_Decl      d -> declaration_to_region d
| S_Directive d -> Directive.to_region d
| S_Export    {region; _} -> region
| S_Expr      e -> expr_to_region e
| S_For       {region; _}
| S_ForOf     {region; _}
| S_If        {region; _} -> region
| S_Return    {region; _}
| S_Switch    {region; _}
| S_While     {region; _} -> region

let var_kind_to_region = function
  `Let w | `Const w -> w#region

let property_id_to_region = function
  F_Name i -> variable_to_region i
| F_Int  i -> i#region
| F_Str  i -> i#region

let fun_body_to_region = function
  StmtBody {region; _} -> region
| ExprBody e -> expr_to_region e

let selection_to_region = function
  PropertyName (dot, property_name) ->
    Region.cover dot#region (variable_to_region property_name)
| PropertyStr brackets -> brackets.region
| Component brackets -> brackets.region

let namespace_selection_to_region = function
  M_Path {region; _} -> region
| M_Alias w -> w#region

let intf_expr_to_region = function
  I_Body {region; _} -> region
| I_Path path -> namespace_selection_to_region path

let parameters_to_region = function
  ParParams {region; _} -> region
| NakedParam p -> pattern_to_region p


(* exposing types so that the Parsing_shared.Common.MakePretty functor works *)
type signature_expr = intf_expr