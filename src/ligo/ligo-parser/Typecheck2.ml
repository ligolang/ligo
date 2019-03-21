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
  | PBytes  of Hex.t
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
  | Bytes
  | Int
  | Unit
  | Bool

  and type_expr = { type_expr: type_expr_case; name: type_name option; orig: Region.t }

  type typed_var = { name:var_name; ty:type_expr; orig: asttodo }

  type type_decl = { name: type_name; ty:type_expr; orig: asttodo }

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
  | Int of Z.t | String of string | Bytes of Hex.t
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

let map f l = List.rev (List.rev_map f l)

let shadow (name : string) (typ : O.type_expr) (env : O.type_expr list SMap.t)
    : O.type_expr list SMap.t =
  SMap.update name (function None -> Some [typ] | Some tl -> Some (typ :: tl)) env

let lookup (name : string) (env : O.type_expr list SMap.t) : O.type_expr =
  match SMap.find name env with
    latest :: shadowed -> latest
  | []                 -> failwith "Unbound variable"

let string_of_name ({name;_} : I.name_and_region) = name

let a_name_and_region ({name; orig} : I.name_and_region) : O.name_and_region =
  {name; orig}

let a_type_constructor (tve : tve) : I.type_constructor -> O.type_constructor = function
  Option -> Option
| List   -> List
| Set    -> Set
| Map    -> Map

let a_type_expr_case (tve : tve) : I.type_expr_case -> O.type_expr_case = function
    Sum      lt         -> failwith "TODO"
  | Record   lt         -> failwith "TODO"
  | TypeApp  (tc, args) -> failwith "TODO"
  | Function {arg;ret}  -> failwith "TODO"
  | Ref      t          -> failwith "TODO"
  | String              -> String
  | Int                 -> Int
  | Unit                -> Unit
  | Bool                -> Bool

let a_type_expr (tve : tve) ({type_expr;name;orig} : I.type_expr) : O.type_expr =
  let type_expr = a_type_expr_case tve type_expr in
  let name = match name with
      None -> None
     |Some name -> Some (a_name_and_region name)
  in {type_expr;name;orig}

let a_type (te,ve : tve) ({name;ty;orig} : I.type_decl) : tve * O.type_decl =
  let ty = a_type_expr (te,ve) ty in
  let tve = shadow (string_of_name name) ty te, ve in
  let name = (a_name_and_region name) in
  tve, {name; ty; orig}

let a_types (tve : tve) (l : I.type_decl list) : tve * O.type_decl list =
  fold_map a_type tve l

let a_storage_decl : tve -> I.typed_var -> tve * O.typed_var =
  failwith "TODO"

let type_expr_case_equal (t1 : O.type_expr_case) (t2 : O.type_expr_case) : bool = match t1,t2 with
    Sum      m1,                  Sum      m2                  -> failwith "TODO" (* of (O.name_and_region * O.type_expr) SMap.t *)
  | Record   m1,                  Record   m2                  -> failwith "TODO" (* of (O.name_and_region * O.type_expr) SMap.t *)
  | TypeApp  (tc1, args1),        TypeApp  (tc2, args2)        -> failwith "TODO" (* of O.type_constructor * O.type_expr list *)
  | Function {arg=arg1;ret=ret1}, Function {arg=arg2;ret=ret2} -> failwith "TODO" (* of { arg : O.type_expr; ret : O.type_expr; } *)
  | Ref      t1,                  Ref      t2                  -> failwith "TODO" (* of O.type_expr *)
  | String,                       String                       -> true
  | Int,                          Int                          -> true
  | Unit,                         Unit                         -> true
  | Bool,                         Bool                         -> true
  | _                                                          -> false

let type_expr_equal (t1 : O.type_expr) (t2 : O.type_expr) : bool =
  type_expr_case_equal t1.type_expr t2.type_expr

let check_type_expr_equal (expected : O.type_expr) (actual : O.type_expr) : unit =
  if type_expr_equal expected actual then
    ()
  else
    failwith "got [actual] but expected [expected]"

let a_var_expr (te,ve : tve) (expected : O.type_expr) (var_name : I.name_and_region) : O.expr_case =
  check_type_expr_equal expected (lookup (string_of_name var_name) ve);
  Var { name = a_name_and_region var_name;
        ty   = expected;
        orig = `TODO }

let a_constant_expr (tve : tve) (expected : O.type_expr) (constant : I.constant) : O.expr_case =
  let to_type_expr type_expr_case : O.type_expr =
    { type_expr = type_expr_case; name = None; orig = Region.ghost } in
  let actual : O.type_expr = match constant with
      Unit       -> to_type_expr Unit
    | Int      _ -> to_type_expr Int
    | String   _ -> to_type_expr String
    | Bytes    _ -> to_type_expr Bytes
    | False      -> to_type_expr Bool
    | True       -> to_type_expr Bool
    | Null     t -> a_type_expr tve t
    | EmptySet t -> a_type_expr tve t
    | CNone    t -> a_type_expr tve t
  in
  check_type_expr_equal expected actual;
  let c : O.constant = match constant with
      Unit       -> Unit
    | Int      i -> Int      i
    | String   s -> String   s
    | Bytes    b -> Bytes    b
    | False      -> False
    | True       -> True
    | Null     _ -> Null
    | EmptySet _ -> EmptySet
    | CNone    _ -> CNone
  in Constant c

let map_to_list m =
  List.rev (SMap.fold (fun field_name_string p l -> p :: l) m [])

let a_field tve (expected,expr) =
  failwith "TODO"

let a_record (tve : tve) (expected : O.type_expr) (record : (I.field_name * I.expr) list)
    : O.expr_case =
  let {type_expr = expected; _} : O.type_expr = expected in
  let expected = match expected with
      Record fields -> fields
    | _ -> failwith "expected some_type but got record" in
  let expected_and_field =
    List.combine
      (map_to_list expected)
      record (* TODO SHOULD BE (map_to_list record) *) in
  Record (map (a_field tve) expected_and_field)

let a_expr_case (te,ve : tve) (expected : O.type_expr) : I.expr -> O.expr_case = function
    App      {operator;arguments} -> failwith "TODO"
  | Var      var_name             -> a_var_expr      (te,ve) expected var_name
  | Constant constant             -> a_constant_expr (te,ve) expected constant
  | Record   record               -> a_record        (te,ve) expected record
  | Lambda   lambda               -> failwith "TODO"

let a_expr (te,ve : tve) (expected : O.type_expr) (e : I.expr) : O.expr =
  let expr_case = a_expr_case (te,ve) expected e in
  { expr = expr_case; ty = expected; orig = `TODO }

let a_declaration (te,ve : tve) ({name;ty;value} : I.decl) : tve * O.decl =
  let ty = a_type_expr (te,ve) ty in
  let value = a_expr (te,ve) ty value in
  let ve = shadow (string_of_name name) ty ve in
  let name = a_name_and_region name in
  (te,ve), {var={name;ty;orig=`TODO};value;orig = `TODO}

let a_declarations (tve : tve) (l : I.decl list) : tve * O.decl list =
  fold_map a_declaration tve l

let a_ast I.{types; storage_decl; declarations; orig} =
  let tve = SMap.empty, SMap.empty in
  let tve, types = a_types tve types in
  let tve, storage_decl = a_storage_decl tve storage_decl in
  let tve, declarations = a_declarations tve declarations in
  let _ = tve in
  O.{types; storage_decl; declarations; orig}

let annotate : I.ast -> O.ast = a_ast
