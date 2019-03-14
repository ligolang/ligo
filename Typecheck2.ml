[@@@warning "-30"]

module SMap = Map.Make(String)

module O = struct
  type asttodo = [`TODO] (* occurrences of asttodo will point to some part of the original parser AST *)

  type name_and_region = {name: string; orig: Region.t}
  type type_name  = name_and_region
  type var_name   = name_and_region
  type field_name = name_and_region

  type record_key = [`Field of field_name | `Component of int]

  type pattern =
    PVar    of var_name
  | PWild
  | PInt    of Z.t
  | PBytes  of MBytes.t
  | PString of string
  | PUnit
  | PFalse
  | PTrue
  | PNone
  | PSome   of pattern
  | PCons   of pattern * pattern
  | PNull
  | PRecord of record_key precord

  and 'key precord = ('key * pattern) list

  type type_constructor =
  | Option
  | List
  | Set
  | Map

  type type_expr_case =
  | Sum      of (type_name * type_expr) list
  | Record   of record_key type_record
  | TypeApp  of type_constructor * (type_expr list)
  | Function of { arg: type_expr; ret: type_expr }
  | Ref      of type_expr
  | String
  | Int
  | Unit
  | Bool
  and 'key type_record = ('key * type_expr) list

  and type_expr = { type_expr: type_expr_case; name: string option; orig: AST.type_expr }

  type typed_var = { name:var_name; ty:type_expr; orig: asttodo }

  type type_decl = { name:string; ty:type_expr; orig: asttodo }

  type expr_case =
    App      of { operator: operator; arguments: expr list }
  | Var      of typed_var
  | Constant of constant
  | Record   of record_key expr_record
  | Lambda   of lambda

  and 'key expr_record = ('key * expr) list

  and expr = { expr: expr_case; ty:type_expr; orig: asttodo }

  and decl = { var: typed_var; value: expr; orig: asttodo }

  and lambda = {
      parameter:    typed_var;
      declarations: decl list;
      instructions: instr list;
      result:       expr;
    }

  and operator_case =
    Function    of var_name
  | Construcor  of var_name
  | UpdateField of record_key
  | GetField    of record_key
  | Or | And | Lt | Leq | Gt | Geq | Equal | Neq | Cat | Cons | Add | Sub | Mult | Div | Mod
  | Neg | Not
  | Set
  | MapLookup

  and operator = { operator: operator_case; ty:type_expr; orig: asttodo }

  and constant =
    Unit
  | Int of Z.t | String of string | Bytes of MBytes.t
  | False | True
  | Null
  | EmptySet
  | CNone

  and instr =
    Assignment    of { name: var_name; value: expr; orig: asttodo }
  | While         of { condition: expr; body: instr list; orig: asttodo }
  | ForCollection of { list: expr; var: var_name; body: instr list; orig: asttodo }
  | Match         of { expr: expr; cases: (pattern * instr list) list; orig: asttodo }
  | ProcedureCall of { expr: expr; orig: asttodo } (* expr returns unit, drop the result. Similar to OCaml's ";". *)
  | Fail          of { expr: expr; orig: asttodo }

  type ast = {
      types           : type_decl list;
      storage_decl    : typed_var;
      declarations    : decl list;
      orig: AST.t
    }
end

let temporary_force_dune = 123
