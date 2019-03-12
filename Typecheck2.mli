[@@@warning "-30"]

module SMap = Map.Make(String)

module O = struct
  type asttodo = [`TODO] (* occurrences of asttodo will point to some part of the original parser AST *)

  type type_name = string
  type var_name = { name: string; orig: asttodo }

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
  | PTuple  of pattern list

  type type_constructor =
  | Option
  | List
  | Set
  | Map

  type type_expr_case =
    Prod     of type_expr_case list
  | Sum      of (type_name * type_expr_case) list
  | Record   of (type_name * type_expr_case) list
  | TypeApp  of type_constructor * (type_expr_case list)
  | Function of { args: type_expr_case list; ret: type_expr_case }
  | Ref      of type_expr_case
  | TC       of type_constructor
  | String
  | Int
  | Unit
  | Bool


  type type_expr = { type_expr: type_expr_case; name: string option; orig: AST.type_expr }

  type typed_var = { name:var_name; ty:type_expr; orig: asttodo }

  type type_decl = { name:string; ty:type_expr; orig: asttodo }

  type expr_case =
    App      of { operator: operator; arguments: expr list }
  | Var      of typed_var
  | Constant of constant
  | Lambda   of lambda

  and expr = { expr: expr_case; ty:type_expr; orig: asttodo }

  and decl = { var: typed_var; value: expr; orig: asttodo }

  and lambda = {
      parameters:   typed_var SMap.t;
      declarations: decl list;
      instructions: instr list;
      result:       expr;
    }

  and operator_case =
    Function of string
  | Or | And | Lt | Leq | Gt | Geq | Equal | Neq | Cat | Cons | Add | Sub | Mult | Div | Mod
  | Neg | Not
  | Tuple | Set | List
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
  | ForCollection of { list: expr; key: var_name; value: var_name option; body: instr list; orig: asttodo }
  | If            of { condition: expr; ifso: instr list; ifnot: instr list; orig: asttodo }
  | Match         of { expr: expr; cases: (pattern * instr list) list; orig: asttodo }
  | DropUnit      of { expr: expr; orig: asttodo } (* expr returns unit, drop the result. Similar to OCaml's ";". *)
  | Fail          of { expr: expr; orig: asttodo }

  type ast = {
      types           : type_decl list;
      storage_decl    : typed_var;
      operations_decl : typed_var;
      declarations    : decl list;
      orig: AST.t
    }
end

let temporary_force_dune = 123
