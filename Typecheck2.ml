[@@@warning "-27"] (* TODO *)
[@@@warning "-32"] (* TODO *)
[@@@warning "-30"]

module SMap = Map.Make(String)

module I = AST2.O

module O = struct
  type asttodo = [`TODO] (* occurrences of asttodo will point to some part of the original parser AST *)

  type name_and_region = {name: string; orig: Region.t}
  type type_name  = name_and_region
  type var_name   = name_and_region
  type field_name = name_and_region

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
  | PRecord of (field_name * pattern) SMap.t

  type type_constructor =
    Option
  | List
  | Set
  | Map

  type type_expr_case =
    Sum      of (type_name * type_expr) SMap.t
  | Record   of (field_name * type_expr) SMap.t
  | TypeApp  of type_constructor * (type_expr list)
  | Function of { arg: type_expr; ret: type_expr }
  | Ref      of type_expr
  | String
  | Int
  | Unit
  | Bool

  and type_expr = { type_expr: type_expr_case; name: string option; orig: Region.t }

  type typed_var = { name:var_name; ty:type_expr; orig: asttodo }

  type type_decl = { name:string; ty:type_expr; orig: asttodo }

  type expr_case =
    App      of { operator: operator; arguments: expr list }
  | Var      of typed_var
  | Constant of constant
  | Record   of (field_name * expr) list
  | Lambda   of lambda

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
  | Constructor of var_name
  | UpdateField of field_name
  | GetField    of field_name
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
      orig            : AST.t
    }
end

type te = O.type_expr list SMap.t
type ve = O.type_expr list SMap.t
type tve = te * ve

let fold_map f a l =
  let f (acc, l) elem =
    let acc', elem' = f acc elem
    in acc', (elem' :: l) in
  let last_acc, last_l = List.fold_left f (a, []) l
  in last_acc, List.rev last_l

let a_type_constructor (tve : tve) : I.type_constructor -> O.type_constructor = function
  Option -> failwith "TODO"
| List   -> failwith "TODO"
| Set    -> failwith "TODO"
| Map    -> failwith "TODO"

let a_type_expr_case (tve : tve) : I.type_expr_case -> O.type_expr_case = function
    Sum      l          -> failwith "TODO"
  | Record   l          -> failwith "TODO"
  | TypeApp  (tc, args) -> failwith "TODO"
  | Function {arg;ret}  -> failwith "TODO"
  | Ref      t          -> failwith "TODO"
  | String              -> failwith "TODO"
  | Int                 -> failwith "TODO"
  | Unit                -> failwith "TODO"
  | Bool                -> failwith "TODO"


let a_type_expr (tve : tve) ({type_expr;name;orig} : I.type_expr) : O.type_expr =
  failwith "TODO"

let a_type (tve : tve) ({name;ty;orig} : I.type_decl) : tve * O.type_decl =
  failwith "TODO"

let a_types (tve : tve) (l : I.type_decl list) : tve * O.type_decl list =
  fold_map a_type tve l

let a_storage_decl : tve -> I.typed_var -> tve * O.typed_var =
  failwith "TODO"

let a_declarations : tve -> I.decl list -> tve * O.decl list =
  failwith "TODO"

let a_ast I.{types; storage_decl; declarations; orig} =
  let tve = SMap.empty, SMap.empty in
  let tve, types = a_types tve types in
  let tve, storage_decl = a_storage_decl tve storage_decl in
  let tve, declarations = a_declarations tve declarations in
  let _ = tve in
  O.{types; storage_decl; declarations; orig}

let annotate : I.ast -> O.ast = a_ast

