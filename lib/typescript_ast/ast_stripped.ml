(* Abstract Syntax Tree (AST) for JsLIGO *)

(* Disabling warnings *)

[@@@warning "-30"] (* multiply-defined record labels *)

(* Vendor dependencies *)

module Utils = Simple_utils.Utils
module Region = Simple_utils.Region
module Ne_list = Nonempty_list

(* Local dependencies *)

module Wrap = Lexing_shared.Wrap

(* Utilities *)

type 'a reg = 'a Region.reg
type 'a wrap = 'a Wrap.wrap
type comment = string wrap
type dec_name = string
type dec_param = string
type decorator = (dec_name * dec_param option) wrap

(* Literals *)

type variable = string wrap
type file_path = string wrap
type bytes_literal = (string * Hex.t) wrap
type int_literal = (string * Z.t) wrap
type string_literal = string wrap

(* Paths (NOT IN REVERSE ORDER. Compare with ast.ml)

   M.N.x.y -> [M; N; x], y
 *)

type simple_path =
  { path : variable list
  ; selected : variable
  }

let print_simple_path (path : simple_path) : unit =
  let { path; selected } = path in
  let app v acc = if acc = "" then v#payload else v#payload ^ "." ^ acc in
  let path = Core.List.fold_right ~f:app ~init:"" path in
  let path = if path = "" then path else path ^ "." in
  Printf.eprintf "%s%s\n%!" path selected#payload

(* The Abstract Syntax Tree *)

type t = statements

(* STATEMENTS *)
and statements = statement Ne_list.t reg

and statement =
  | S_block of statements
  | S_break of Region.t
  | S_decl of declaration
  | S_export of declaration
  | S_expr of expr
  | S_for of for_stmt reg
  | S_for_of of for_of_stmt reg
  | S_if of if_stmt reg
  | S_return of expr option reg
  | S_switch of switch_stmt reg
  | S_while of while_stmt reg

(* Conditional statement *)
and if_stmt =
  { test : expr
  ; if_so : statement
  ; if_not : statement option
  }

(* For-loops *)
and for_stmt =
  { initialiser : statement option
  ; condition : expr option
  ; afterthought : expr list
  ; for_body : statement option
  }

(* For-of loops *)
and for_of_stmt =
  { index_kind : var_kind option
  ; index : (key * value option) reg
  ; expr : expr
  ; for_of_body : statement
  }

and key = variable
and value = variable

and var_kind =
  [ `Let of Region.t
  | `Const of Region.t
  ]

(* Switch statement *)
and switch_stmt = expr * cases
and cases = switch_case Nonempty_list.t * switch_default option
and switch_case = expr * statements option
and switch_default = statements option

(* While-loop *)
and while_stmt = expr * statement

(* DECLARATIONS *)
and declaration =
  | D_class of class_decl reg
  | D_decorated of decorator * declaration
  | D_function of fun_decl reg
  | D_import of import_decl
  | D_interface of interface_decl reg
  | D_namespace of namespace_decl reg
  | D_type of type_decl reg
  | D_value of value_decl reg

(* Class declaration *)
and class_decl =
  { comments : comment list (* From the keyword "class" *)
  ; class_name : variable
  ; implements : simple_path reg list
  ; class_body : class_member Nonempty_list.t reg
  }

and class_member =
  | Method_definition of method_definition reg
  | Public_field_definition of public_field_definition reg

and method_definition =
  { method_sig : method_signature reg
  ; method_body : statements
  }

and method_signature =
  { decorators : decorator list
  ; comments : comment list
  ; static : Region.t option
  ; method_name : variable
  ; generics : variable list
  ; parameters : (variable * type_expr) reg list
  ; rhs_type : type_expr
  }

and public_field_definition =
  { decorators : decorator list
  ; static : Region.t option
  ; name : variable
  ; field_type : type_expr option
  ; field_value : expr
  }

(* Function declaration *)
and fun_decl =
  { comments : comment list (* From the keyword "function" *)
  ; fun_name : variable
  ; generics : variable list
  ; parameters : parameter reg list
  ; rhs_type : type_expr option
  ; fun_body : statements
  }

and parameter = pattern * type_expr option

(* All import declarations *)
and import_decl =
  | Import_alias of import_alias reg
  | Import_all_as of import_all_as reg
  | Import_from of import_from reg

(* import M = N.O *)
and import_alias = variable * simple_path reg

(* import * as M from "/my/path.ts" *)
and import_all_as = variable * file_path

(* import {x, y} from "/my/path.ts" *)
and import_from = variable Ne_list.t * file_path

(* Interfaces

  Note: No value for the type [intf_expr] is decoded: only for further
  translation to unified AST.  *)
and interface_decl =
  { intf_name : variable
  ; intf_extends : simple_path reg list
  ; intf_body : intf_entry reg list reg
  }

and intf_entry =
  { decorators : decorator list
  ; comments : comment list
  ; entry_name : variable
  ; entry_optional : Region.t option
  ; entry_type : type_expr
  }

(* NOTE: Type [intf_expr] is not initial: it is used by the
   translation to the unified AST. *)
and intf_expr =
  | I_body of intf_entry reg list reg
  | I_path of simple_path reg

(* Namespace declaration *)
and namespace_decl =
  { namespace_name : variable
  ; namespace_type : intf_expr list (* Not initial. See [intf_expr]. *)
  ; namespace_body : statements
  }

(* Type declarations *)
and type_decl =
  { name : variable
  ; generics : variable list
  ; type_expr : type_expr
  }

(* Value declaration *)
and value_decl =
  { comments : comment list (* From the keyword "let" or "const" *)
  ; kind : var_kind
  ; bindings : val_binding reg Ne_list.t
  }

and val_binding =
  { pattern : pattern
  ; rhs_type : type_expr option
  ; rhs_expr : expr
  }

(* TYPE EXPRESSIONS *)
and type_expr =
  | T_apply of (type_expr * type_expr list) reg (* t<u,v> *)
  | T_for_all of (variable list * type_expr) reg (* <T,U>(x: T) => U *)
  | T_fun of fun_type reg (* (x : T) => U *)
  | T_int of int_literal (* 42 *)
  | T_object of member_type reg list reg (* {x; @a y : t} *)
  | T_parameter_of of simple_path reg reg (* parameter_of<N.C> *)
  | T_path of simple_path reg (* t  M.t *)
  | T_string of string_literal (* "x" *)
  | T_sum of sum_type (* ["some", T] | ["none"] *)
  | T_tuple of type_expr Ne_list.t reg (* [t, [u, v]] *)
  | T_union of union_type (* number | string *)

(* Sum type

  A sum type is a special case of a union type, where all the summands
  are tuple types whose first component is an identifier starting with
  a dollar ($) sign. That identifier denotes a _data constructor_, and
  the rest of the components are the _type parameters_ to that
  constructor. The tuple type as a whole is called a _variant_. For
  instance:

  type option<T> = ["some", T] | ["none"];

  The constructors are "some" and "none". The former takes a
  parameter T, whereas the latter takes none.

  The values of a sum type are created with a tuple whose first member
  (component) is the constructor coerced to a singleton type, and the
  other members are the arguments. For instance:

  const some_number : option<number> = ["some" as "some", 1];

  Those values are projected by means of _pattern matchings_. Those
  are a special case of a call to a predefined function "$match",
  whose first argument is the value to be matched (the projected
  subject), and the second is an object pattern whose property names
  are the constructors above, and the properties themselves are
  functions taking the parameters to the constructor. The compiler
  checks that no constructor has been forgotten. For instance:

  function to_list<T> (x : option<T>) : list<T> {
    return $match(x, { some: (y) => [y],
                       none: ()  => []});

  const singleton : list<number> = to_list<number>(some_number);
 *)
and sum_type = variant reg Ne_list.t reg
and variant = string_literal * type_expr list

(* Object type and class bodies *)
and member_type =
  { decorators : decorator list
  ; comments : comment list
  ; property_name : variable
  ; rhs_type : type_expr
  }

(* Functional type *)
and fun_type = (variable * type_expr) reg list * type_expr

(* Object *)
and 'a _object = 'a property reg list reg

and 'a property =
  { decorators : decorator list (* From the property identifier *)
  ; comments : comment list (* From the property identifier *)
  ; property_name : variable
  ; static : Region.t option
  ; property_rhs : 'a
  }

(* Union type *)
and union_type = type_expr Ne_list.t reg

(* PATTERNS *)
and pattern =
  | P_array of pattern array (* [x, ...y, z] [] *)
  | P_bytes of bytes_literal (* 0xFFFA *)
  | P_false of Region.t (* false *)
  | P_int of int_literal (* 42 *)
  | P_object of pattern _object (* {x, y: z} *)
  | P_string of string_literal (* "string" *)
  | P_true of Region.t (* true *)
  | P_var of simple_path reg (* x  M.N.x *)
  | P_typed of (pattern * type_expr) reg (* NOTE: ONLY INTERNAL *)

(* Array pattern (shadowing the predefined type [array]) *)
and 'a array = 'a element list reg

and 'a element =
  | Spread of 'a
  | Element of 'a

(* EXPRESSIONS *)
and expr =
  | E_add of (expr * expr) reg (* x + y *)
  | E_add_eq of (expr * expr) reg (* x += y *)
  | E_and of (expr * expr) reg (* x && y *)
  | E_app of (expr * expr list) reg (* f(x,y)  foo() *)
  | E_array of expr array (* [x, ...y, z]  [] *)
  | E_arrow_fun of arrow_fun_expr reg (* (x : int) => e *)
  | E_assign of (expr * expr) reg (* x = y *)
  | E_bit_and of (expr * expr) reg (* x & y *)
  | E_bit_and_eq of (expr * expr) reg (* x &= y *)
  | E_bit_neg of expr reg (* ~x *)
  | E_bit_or of (expr * expr) reg (* x | y *)
  | E_bit_or_eq of (expr * expr) reg (* x |= y *)
  | E_bit_sl of (expr * expr) reg (* x << y *)
  | E_bit_sl_eq of (expr * expr) reg (* x <<= y *)
  | E_bit_sr of (expr * expr) reg (* x >> y *)
  | E_bit_sr_eq of (expr * expr) reg (* x >>= y *)
  | E_bit_xor of (expr * expr) reg (* x ^ y *)
  | E_bit_xor_eq of (expr * expr) reg (* x ^= y *)
  | E_bytes of bytes_literal (* 0xFFFA *)
  | E_contract_of of simple_path reg reg (* contract_of (M.N) *)
  | E_ctor_app of (string_literal * expr list) reg (* ["K", 1, e] *)
  | E_div of (expr * expr) reg (* x / y *)
  | E_div_eq of (expr * expr) reg (* x /= y *)
  | E_equal of (expr * expr) reg (* x == y *)
  | E_false of Region.t (* false *)
  | E_function of function_expr reg (* function (x) {...} *)
  | E_geq of (expr * expr) reg (* x >= y *)
  | E_gt of (expr * expr) reg (* x > y *)
  | E_int of int_literal (* 42 *)
  | E_leq of (expr * expr) reg (* x <= y *)
  | E_lt of (expr * expr) reg (* x < y *)
  | E_match of (expr * match_clause Ne_list.t) reg (* $match(x, {c: (x) => e}) *)
  | E_member of (expr * variable) reg (* e.x *)
  | E_michelson of michelson_expr (* michelson(`{ADD}`) as t *)
  | E_mult of (expr * expr) reg (* x * y *)
  | E_mult_eq of (expr * expr) reg (* x *= y *)
  | E_neg of expr reg (* -x *)
  | E_neq of (expr * expr) reg (* x != y *)
  | E_not of expr reg (* !x *)
  | E_object of expr _object (* {x : e, y} *)
  | E_or of (expr * expr) reg (* x || y *)
  | E_post_decr of variable reg (* x-- *)
  | E_post_incr of variable reg (* x++ *)
  | E_pre_decr of variable reg (* --x *)
  | E_pre_incr of variable reg (* ++x *)
  | E_rem of (expr * expr) reg (* x % n*)
  | E_rem_eq of (expr * expr) reg (* x %= y*)
  | E_string of string_literal (* "abcdef" *)
  | E_sub of (expr * expr) reg (* x - y *)
  | E_subscript of (expr * int_literal) reg (* e[1] *)
  | E_sub_eq of (expr * expr) reg (* x -= y *)
  | E_template of string_literal (* `abcdef` *)
  | E_ternary of ternary reg (* x ? y : z *)
  | E_true of Region.t (* true *)
  | E_typed of typed_expr reg (* e as t *)
  | E_update of update_expr reg (* {...x, y : z} *)
  | E_var of variable (* x *)
  | E_xor of (expr * expr) reg (* x ^^ y *)

(* Pattern matching *)
and match_clause =
  { constructor : variable
  ; filter : parameter reg option
  ; match_rhs : expr
  }

(* Michelson injection: "Michelson (`{ADD}`) as t" *)
and michelson_expr = (variable * string_literal * type_expr) reg

(* Functional expressions *)
and arrow_fun_expr =
  { generics : variable list
  ; parameters : parameter reg list
  ; rhs_type : type_expr option
  ; fun_body : fun_body
  }

and function_expr = arrow_fun_expr

and fun_body =
  | Stmt_body of statements
  | Expr_body of expr

(* Functional update of object expressions *)
and update_expr =
  { obj_expr : expr
  ; updates : expr property reg list
  }

(* Ternary conditional *)
and ternary =
  { condition : expr
  ; truthy : expr
  ; falsy : expr
  }

(* Typed expression *)
and typed_expr = expr (* "as" *) * type_expr

(* PROJECTIONS *)

(* Projecting regions from some nodes of the AST *)

let region_of_import_decl = function
  | Import_alias { region; _ } | Import_all_as { region; _ } | Import_from { region; _ }
    -> region

let rec region_of_declaration = function
  | D_class { region; _ } -> region
  | D_decorated (_, decl) -> region_of_declaration decl
  | D_function { region; _ } -> region
  | D_import d -> region_of_import_decl d
  | D_interface { region; _ } | D_namespace { region; _ } | D_type { region; _ } -> region
  | D_value { region; _ } -> region

let region_of_type_expr = function
  | T_apply { region; _ } | T_for_all { region; _ } | T_fun { region; _ } -> region
  | T_int w -> w#region
  | T_object { region; _ } | T_parameter_of { region; _ } -> region
  | T_string w -> w#region
  | T_sum { region; _ } -> region
  | T_tuple { region; _ } -> region
  | T_union { region; _ } -> region
  | T_path { region; _ } -> region

let region_of_pattern = function
  | P_array { region; _ } -> region
  | P_bytes w -> w#region
  | P_false r -> r
  | P_int w -> w#region
  | P_object { region; _ } -> region
  | P_string w -> w#region
  | P_true r -> r
  | P_var { region; _ } -> region
  | P_typed { region; _ } -> region

let region_of_expr = function
  | E_add { region; _ }
  | E_add_eq { region; _ }
  | E_and { region; _ }
  | E_app { region; _ }
  | E_array { region; _ }
  | E_arrow_fun { region; _ }
  | E_assign { region; _ }
  | E_bit_and { region; _ }
  | E_bit_and_eq { region; _ }
  | E_bit_neg { region; _ }
  | E_bit_or { region; _ }
  | E_bit_or_eq { region; _ }
  | E_bit_sl { region; _ }
  | E_bit_sl_eq { region; _ }
  | E_bit_sr { region; _ }
  | E_bit_sr_eq { region; _ }
  | E_bit_xor { region; _ }
  | E_bit_xor_eq { region; _ } -> region
  | E_bytes w -> w#region
  | E_contract_of { region; _ }
  | E_ctor_app { region; _ }
  | E_div { region; _ }
  | E_div_eq { region; _ }
  | E_equal { region; _ } -> region
  | E_false r -> r
  | E_function { region; _ } | E_geq { region; _ } | E_gt { region; _ } -> region
  | E_int w -> w#region
  | E_leq { region; _ }
  | E_lt { region; _ }
  | E_match { region; _ }
  | E_member { region; _ }
  | E_michelson { region; _ }
  | E_mult { region; _ }
  | E_mult_eq { region; _ }
  | E_neg { region; _ }
  | E_neq { region; _ }
  | E_not { region; _ }
  | E_object { region; _ }
  | E_or { region; _ }
  | E_post_decr { region; _ }
  | E_post_incr { region; _ }
  | E_pre_decr { region; _ }
  | E_pre_incr { region; _ }
  | E_rem { region; _ }
  | E_rem_eq { region; _ } -> region
  | E_string w -> w#region
  | E_sub { region; _ } | E_subscript { region; _ } | E_sub_eq { region; _ } -> region
  | E_template w -> w#region
  | E_ternary { region; _ } -> region
  | E_true r -> r
  | E_typed { region; _ } | E_update { region; _ } -> region
  | E_var w -> w#region
  | E_xor { region; _ } -> region

let region_of_statement = function
  | S_block { region; _ } -> region
  | S_break r -> r
  | S_decl d | S_export d -> region_of_declaration d
  | S_expr e -> region_of_expr e
  | S_for { region; _ } | S_for_of { region; _ } | S_if { region; _ } -> region
  | S_return { region; _ } | S_switch { region; _ } | S_while { region; _ } -> region

let region_of_var_kind = function
  | `Let w | `Const w -> w#region

let region_of_fun_body_to_region = function
  | Stmt_body { region; _ } -> region
  | Expr_body e -> region_of_expr e

let region_of_class_member = function
  | Method_definition { region; _ } | Public_field_definition { region; _ } -> region
