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


let get_def_type = function
  | Type t -> t.def_type
  | Variable v -> v.def_type
  | Module m -> m.def_type


let add_references_to_def : def -> LSet.t -> def =
 fun def references ->
  match def with
  | Variable vdef ->
    Variable { vdef with references = LSet.union vdef.references references }
  | Type tdef -> Type { tdef with references = LSet.union tdef.references references }
  | Module mdef -> Module { mdef with references = LSet.union mdef.references references }


let get_references = function
  | Type t -> t.references
  | Variable v -> v.references
  | Module m -> m.references


let make_def_id name (loc : Location.t) =
  match loc with
  | File region -> name ^ "#" ^ region#compact ~file:false `Point
  | Virtual v -> name ^ "#" ^ v


let make_v_def : string -> type_case -> def_type -> Location.t -> Location.t -> def =
 fun name t def_type range body_range ->
  let uid = make_def_id name range in
  Variable { name; range; body_range; t; uid; references = LSet.empty; def_type }


let make_t_def : string -> def_type -> Location.t -> Ast_core.type_expression -> def =
 fun name def_type loc te ->
  let uid = make_def_id name loc in
  Type
    { name
    ; range = loc
    ; body_range = te.location
    ; uid
    ; content = te
    ; def_type
    ; references = LSet.empty
    }


let make_m_def
    : range:Location.t -> body_range:Location.t -> string -> def_type -> def list -> def
  =
 fun ~range ~body_range name def_type members ->
  let uid = make_def_id name range in
  let mod_case = Def members in
  Module { name; range; body_range; mod_case; uid; references = LSet.empty; def_type }


let make_m_alias_def
    :  range:Location.t -> body_range:Location.t -> string -> def_type -> string list
    -> def
  =
 fun ~range ~body_range name def_type alias ->
  let uid = make_def_id name range in
  let mod_case = Alias alias in
  Module { name; range; body_range; mod_case; uid; references = LSet.empty; def_type }


let filter_local_defs : def list -> [ `Global of def list ] * [ `Local of def list ] =
 fun defs ->
  let gdefs, ldefs =
    List.partition_tf ~f:(fun def -> Caml.(get_def_type def = Global)) defs
  in
  `Global gdefs, `Local ldefs


let rec ignore_local_defs : def list -> def list =
 fun defs ->
  match defs with
  | [] -> []
  | Variable def :: defs when Caml.(def.def_type = Local) -> ignore_local_defs defs
  | Type def :: defs when Caml.(def.def_type = Local) -> ignore_local_defs defs
  | Module def :: defs when Caml.(def.def_type = Local) -> ignore_local_defs defs
  | Module ({ mod_case = Def def; _ } as mdef) :: defs ->
    Module { mdef with mod_case = Def (ignore_local_defs def) } :: ignore_local_defs defs
  | def :: defs -> def :: ignore_local_defs defs


type scope = Location.t * def list
type scopes = scope list

let add_defs_to_scope : def list -> scope -> scope =
 fun defs scope ->
  let loc, scope_defs = scope in
  loc, scope_defs @ defs


let add_defs_to_scopes : def list -> scopes -> scopes =
 fun defs scopes -> List.map scopes ~f:(add_defs_to_scope defs)


let merge_same_scopes : scopes -> scopes =
 fun scopes ->
  let rec aux scopes acc =
    match scopes with
    | [] -> acc
    | (loc, scope) :: scopes ->
      let same, different =
        List.partition_tf scopes ~f:(fun (_, s) -> List.equal def_equal s scope)
      in
      let merged_scope_loc =
        List.fold_left same ~init:loc ~f:(fun loc (loc', _) -> Location.cover loc loc')
      in
      let merged_scope = merged_scope_loc, scope in
      aux different (merged_scope :: acc)
  in
  aux scopes []


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


module Bindings_map = Simple_utils.Map.Make (struct
  type t = Ast_typed.expression_variable

  let compare = Value_var.compare
end)

type bindings_map = Ast_typed.type_expression Bindings_map.t
