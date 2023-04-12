open Ligo_prim

let generated_flag = "#?generated"

let get_binder_name : Value_var.t -> string =
 fun v -> if Value_var.is_generated v then generated_flag else Value_var.to_name_exn v


let get_type_binder_name : Type_var.t -> string =
 fun v -> if Type_var.is_generated v then generated_flag else Type_var.to_name_exn v


let get_mod_binder_name : Module_var.t -> string =
 fun v -> if Module_var.is_generated v then generated_flag else Module_var.to_name_exn v


module Location = Simple_utils.Location
module List = Simple_utils.List
module LSet = Caml.Set.Make (Location)

type type_case =
  | Core of Ast_core.type_expression
  | Resolved of Ast_typed.type_expression
  | Unresolved

type def_type =
  | Local
  | Global

type vdef =
  { name : string
  ; uid : string
  ; range : Location.t
  ; body_range : Location.t
  ; t : type_case
  ; references : LSet.t
  ; def_type : def_type
  }

type tdef =
  { name : string
  ; uid : string
  ; range : Location.t
  ; body_range : Location.t
  ; content : Ast_core.type_expression
  ; def_type : def_type
  ; references : LSet.t
  }

type mod_case =
  | Def of def list
  | Alias of string list

and mdef =
  { name : string
  ; uid : string
  ; range : Location.t
  ; body_range : Location.t
  ; references : LSet.t
  ; mod_case : mod_case
  ; def_type : def_type
  }

and def =
  | Variable of vdef
  | Type of tdef
  | Module of mdef

let def_compare a b =
  match a, b with
  | Variable x, Variable y -> String.compare x.name y.name
  | Type x, Type y -> String.compare x.name y.name
  | Module x, Module y -> String.compare x.name y.name
  | Variable _, (Type _ | Module _) -> -1
  | (Type _ | Module _), Variable _ -> 1
  | Type _, Module _ -> 1
  | Module _, Type _ -> -1


let def_equal a b = 0 = def_compare a b

let get_def_name = function
  | Variable d -> d.name
  | Type d -> d.name
  | Module d -> d.name


let get_def_uid = function
  | Variable d -> d.uid
  | Type d -> d.uid
  | Module d -> d.uid


let get_range = function
  | Type t -> t.range
  | Variable v -> v.range
  | Module m -> m.range


let get_body_range = function
  | Type t -> t.body_range
  | Variable v -> v.body_range
  | Module m -> m.body_range


let make_def_id name (loc : Location.t) =
  match loc with
  | File region -> name ^ "#" ^ region#compact ~file:false `Point
  | Virtual v -> name ^ "#" ^ v


type scope = Location.t * def list
type scopes = scope list

let rec flatten_defs defs =
  match defs with
  | [] -> []
  | (Module { mod_case = Def d; _ } as def) :: defs ->
    [ def ] @ flatten_defs (shadow_defs d) @ flatten_defs defs
  | def :: defs -> def :: flatten_defs defs


and shadow_defs : def list -> def list =
 fun defs ->
  match defs with
  | [] -> []
  | def :: defs ->
    let shadow_def def' = not @@ def_equal def def' in
    def :: shadow_defs (List.filter defs ~f:shadow_def)


let fix_shadowing_in_scope : scope -> scope =
 fun (loc, defs) ->
  let defs = shadow_defs defs in
  let defs = flatten_defs defs in
  loc, defs


let fix_shadowing_in_scopes : scopes -> scopes =
 fun scopes -> List.map scopes ~f:fix_shadowing_in_scope
