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
module LSet = Caml.Set.Make (Simple_utils.Location_ordered)

module Uid : sig
  type t

  val make : string -> Location.t -> t
  val ( = ) : t -> t -> bool
  val to_name : t -> string
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end = struct
  type t = Uid of string [@@unboxed]

  let make name (loc : Location.t) : t =
    Uid
      (match loc with
      | File region -> Format.sprintf "%s#%s" name (region#compact ~file:false `Point)
      | Virtual v -> Format.sprintf "%s#%s" name v)


  let ( = ) (Uid a : t) (Uid b : t) : bool = String.(a = b)

  (** Given an UID of format [{name}#{line}:{start_col}-{end_col}], returns only
    the [{name}] part. *)
  let to_name (Uid uid : t) : string = String.take_while uid ~f:(Char.( <> ) '#')

  (** Returns an UID of format [{name}#{line}:{start_col}-{end_col}]. *)
  let to_string (Uid uid : t) : string = uid

  let pp (ppf : Format.formatter) (Uid uid : t) : unit = Format.fprintf ppf "%s" uid
end

type ('core_expr, 'typed_expr) resolve_case =
  | Core of 'core_expr
  | Resolved of 'typed_expr
  | Unresolved

type type_case = (Ast_core.type_expression, Ast_typed.type_expression) resolve_case
type signature_case = (Ast_core.signature, Ast_typed.signature) resolve_case

type def_type =
  | Local
  | Parameter
  | Module_field
  | Global

type vdef_attributes =
  | No_attributes
  | Value_attr of Value_attr.t
  | Sig_item of Sig_item_attr.t

type tdef_attributes =
  | Type_attr of Type_or_module_attr.t
  | Sig_type of Sig_type_attr.t
  | No_attributes

type mdef_attributes =
  | Module_attr of Type_or_module_attr.t
  | Signature_attr of Signature_attr.t
  | No_attributes

type vdef =
  { name : string
  ; uid : Uid.t
  ; range : Location.t
  ; body_range : Location.t option
  ; t : type_case
  ; references : LSet.t
  ; def_type : def_type
  ; mod_path : string list
  ; attributes : vdef_attributes
  }

type tdef =
  { name : string
  ; uid : Uid.t
  ; range : Location.t
  ; body_range : Location.t option
  ; content : Ast_core.type_expression option
        (** The RHS ([u]) of a type definition [type t = u]. For signatures and
            interfaces, the type might be abstract and have no content, e.g.:
            [module type I = sig type t end]. *)
  ; def_type : def_type
  ; references : LSet.t
  ; mod_path : string list
  ; attributes : tdef_attributes
  }

type mod_case =
  | Def of def list
  | Alias of
      { module_path : Uid.t list
      ; resolved_module : Uid.t option
      ; file_name : string option
            (** If module name is mangled (i.e. it was obtained from preprocessing some import directive)
                then this field will contain a file name of a module. *)
      }

and mod_name =
  | Original of string
  | Filename of string

and mdef =
  { name : mod_name
  ; uid : Uid.t
  ; range : Location.t
  ; body_range : Location.t option
  ; references : LSet.t
  ; mod_case : mod_case
  ; def_type : def_type
  ; mod_path : string list
  ; signature : signature_case
  ; attributes : mdef_attributes
  }

and def =
  | Variable of vdef
  | Type of tdef
  | Module of mdef

let mod_name_compare a b =
  match a, b with
  | Original a, Original b -> String.compare a b
  | Filename a, Filename b -> String.compare a b
  | Original _, Filename _ -> -1
  | Filename _, Original _ -> 1


let def_compare a b =
  match a, b with
  | Variable x, Variable y -> String.compare x.name y.name
  | Type x, Type y -> String.compare x.name y.name
  | Module x, Module y -> mod_name_compare x.name y.name
  | Variable _, (Type _ | Module _) -> -1
  | (Type _ | Module _), Variable _ -> 1
  | Type _, Module _ -> 1
  | Module _, Type _ -> -1


let def_equal a b = 0 = def_compare a b

let get_mod_name_name = function
  | Original n -> n
  | Filename n -> n


let get_def_name = function
  | Variable d -> d.name
  | Type d -> d.name
  | Module d -> get_mod_name_name d.name


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


(** [mvar_to_id] takes and [Module_var.t] and gives id of the form
    [{name}#{line}:{start_col}-{end_col}] *)
let mvar_to_id (m : Module_var.t) : Uid.t =
  let name = Format.asprintf "%a" Module_var.pp m in
  let loc = Module_var.get_location m in
  Uid.make name loc


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


let fix_shadowing_in_scopes : scopes -> scopes = List.map ~f:fix_shadowing_in_scope
