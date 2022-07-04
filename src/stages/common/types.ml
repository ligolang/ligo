[@@@warning "-30"]

module Location  = Simple_utils.Location
module List      = Simple_utils.List
module ValueVar  = Var.ValueVar
module TypeVar   = Var.TypeVar
module ModuleVar = Var.ModuleVar
include Enums

module SMap = Simple_utils.Map.Make(String)

type location = Location.t
type 'a location_wrap = 'a Location.wrap

type attributes = string list

type known_attributes = {
  inline: bool ;
  no_mutation: bool;
  (* Some external constant (e.g. `Test.balance`) do not accept any argument. This annotation is used to prevent LIGO interpreter to evaluate (V_Thunk values) and forces inlining in the compiling (15-self_mini_c)
     TODO: we should change the type of such constants to be `unit -> 'a` instead of just 'a
  *)
  thunk : bool;
  view : bool;
  public: bool;
  (* Controls whether a declaration must be printed or not when using LIGO print commands (print ast-typed , ast-aggregated .. etc ..)
     set to true for standard libraries
  *)
  hidden: bool;
} [@@deriving hash]

type expression_variable = ValueVar.t [@@deriving hash]
type type_variable       = TypeVar.t [@@deriving hash]
type module_variable     = ModuleVar.t [@@deriving hash]

type kind = | Type
            | Singleton [@@deriving yojson,equal,compare,hash]

type label = Label of string [@@deriving hash]
let label_to_yojson (Label l) = `List [`String "Label"; `String l]
let equal_label (Label a) (Label b) = String.equal a b
let compare_label (Label a) (Label b) = String.compare a b
module LMap = Simple_utils.Map.MakeHashable(struct type t = label [@@deriving hash]
                                           let compare = compare_label
                                            end)
type 'a label_map = 'a LMap.t [@@deriving hash]

let const_name = function
  | Const      const     -> const

type 'ty_expr row_element_mini_c = {
  associated_type      : 'ty_expr ;
  michelson_annotation : string option [@hash.ignore] ;
  decl_pos : int [@hash.ignore] ;
  } [@@deriving hash]

type 'ty_exp type_app = {
  type_operator : type_variable ;
  arguments     : 'ty_exp list ;
} [@@deriving hash]

type 'ty_expr row_element = {
  associated_type : 'ty_expr ;
  attributes      : string list ;
  decl_pos        : int ;
  }

type 'a module_access = {
  module_path : module_variable list ;
  element     : 'a ;
} [@@deriving hash]

(* Type level types *)
type 'ty_exp abstraction = {
  ty_binder : type_variable;
  kind : kind ;
  type_ : 'ty_exp ;
} [@@deriving hash]

type 'ty_exp rows = {
  fields     : 'ty_exp row_element label_map;
  attributes : string list ;
  }

type 'ty_exp arrow = {
  type1: 'ty_exp ;
  type2: 'ty_exp ;
  } [@@deriving hash]

(* Expression level types *)
type binder_attributes = {
    const_or_var : [`Const | `Var] option;
  } [@@deriving hash]

type 'ty_exp binder = {
  var  : expression_variable ;
  ascr : 'ty_exp option;
  attributes : binder_attributes ;
  } [@@deriving hash]


type 'exp application = {
  lamb: 'exp ;
  args: 'exp ;
  }

type 'exp constant = {
  cons_name: constant' ; (* this is in enum *)
  arguments: 'exp list ;
  }

type ('exp,'ty_exp) lambda = {
  binder: 'ty_exp binder ;
  output_type : 'ty_exp option;
  result: 'exp ;
  }

type ('exp, 'ty_exp) recursive = {
  fun_name :  expression_variable ;
  fun_type : 'ty_exp ;
  lambda   : ('exp, 'ty_exp) lambda ;
  }

type ('exp, 'ty_exp) let_in = {
    let_binder: 'ty_exp binder ;
    rhs       : 'exp ;
    let_result: 'exp ;
    attributes: attributes ;
  }

type ('exp, 'ty_exp) type_in = {
    type_binder: type_variable ;
    rhs        : 'ty_exp ;
    let_result : 'exp ;
  }

type 'exp raw_code = {
  language : string ;
  code : 'exp ;
  }

type 'exp constructor = {constructor: label; element: 'exp}

type 'exp access =
  | Access_tuple of z
  | Access_record of string
  | Access_map of 'exp

type 'exp accessor = {record: 'exp; path: 'exp access list}
type 'exp update   = {record: 'exp; path: 'exp access list; update: 'exp}

type 'exp record_accessor = {record: 'exp; path: label}
type 'exp record_update   = {record: 'exp; path: label; update: 'exp}

type ('exp) type_abs = {type_binder:type_variable;result:'exp}

type ('exp,'ty_exp) ascription = {anno_expr: 'exp; type_annotation: 'ty_exp}

type 'exp conditional = {
  condition   : 'exp ;
  then_clause : 'exp ;
  else_clause : 'exp ;
  }

and 'exp sequence = {
  expr1: 'exp ;
  expr2: 'exp ;
  }

and ('exp,'ty_exp) assign = {
  binder      : 'ty_exp binder ;
  access_path : 'exp access list ;
  expression  : 'exp ;
  }

and 'exp for_ = {
  binder : expression_variable ;
  start  : 'exp ;
  final  : 'exp ;
  incr   : 'exp ;
  f_body : 'exp ;
  }

and 'exp for_each = {
  fe_binder : expression_variable * expression_variable option ;
  collection : 'exp ;
  collection_type : collect_type ;
  fe_body : 'exp ;
  }

and collect_type =
  | Map
  | Set
  | List
  | Any

and 'exp while_loop = {
  cond : 'exp ;
  body : 'exp ;
  }

type 'ty_exp list_pattern =
  | Cons of 'ty_exp pattern * 'ty_exp pattern
  | List of 'ty_exp pattern list

and 'ty_exp pattern_repr =
  | P_unit
  | P_var of 'ty_exp binder
  | P_list of 'ty_exp list_pattern
  | P_variant of label * 'ty_exp pattern
  | P_tuple of 'ty_exp pattern list
  | P_record of label list * 'ty_exp pattern list

and 'ty_exp pattern = 'ty_exp pattern_repr Location.wrap

type ('exp , 'ty_exp) match_case = {
  pattern : 'ty_exp pattern ;
  body : 'exp
}

type ('exp , 'ty_exp) match_exp = {
  matchee : 'exp ;
  cases : ('exp , 'ty_exp) match_case list
}

(*** Declarations language ***)

type ('ty_exp,'attr) declaration_type' = {
    type_binder : type_variable ;
    type_expr : 'ty_exp ;
    type_attr : 'attr ;
  }

and ('exp,'ty_exp,'attr) declaration_constant' = {
    binder : 'ty_exp binder;
    expr : 'exp ;
    attr : 'attr ;
  }

and ('exp,'ty_exp,'attr_e,'attr_t,'attr_m) declaration_module' = {
    module_binder : module_variable ;
    module_ : ('exp,'ty_exp,'attr_e,'attr_t,'attr_m) module_expr' ;
    module_attr : 'attr_m
  }

and ('exp,'ty_exp,'attr_e,'attr_t,'attr_m) declaration_content' =
  | Declaration_type     of ('ty_exp,'attr_t) declaration_type'
  | Declaration_constant of ('exp,'ty_exp,'attr_e) declaration_constant'
  | Declaration_module   of ('exp,'ty_exp,'attr_e,'attr_t,'attr_m) declaration_module'

and ('exp,'ty_exp,'attr_e,'attr_t,'attr_m) declaration' = ('exp,'ty_exp,'attr_e,'attr_t,'attr_m) declaration_content' location_wrap

(*** Modules language ***)

and ('exp,'ty_exp,'attr_e,'attr_t,'attr_m) declarations' = ('exp,'ty_exp,'attr_e,'attr_t,'attr_m) declaration' list


and ('exp,'ty_exp,'attr_e,'attr_t,'attr_m) mod_in' = {
  module_binder : module_variable ;
  rhs           : ('exp,'ty_exp,'attr_e,'attr_t,'attr_m) module_expr' ;
  let_result    : 'exp ;
}

and module_path_' = module_variable List.Ne.t

and ('exp,'ty_exp,'attr_e,'attr_t,'attr_m) module_expr_content' =
  | M_struct of ('exp,'ty_exp,'attr_e,'attr_t,'attr_m) declarations'
  | M_variable of module_variable
  | M_module_path of module_path_'
  (* FUTURE: Functor ; Apply *)

and ('exp,'ty_exp,'attr_e,'attr_t,'attr_m) module_expr' = ('exp,'ty_exp,'attr_e,'attr_t,'attr_m) module_expr_content' Location.wrap
