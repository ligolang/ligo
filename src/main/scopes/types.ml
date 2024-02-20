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
module LSet = Caml.Set.Make (Simple_utils.Location)
module LMap = Simple_utils.Map.Make (Simple_utils.Location)

module Uid : sig
  type t [@@deriving compare, sexp]

  val make : string -> Location.t -> t
  val make_var : (module Var with type t = 'v) -> 'v -> t option
  val ( = ) : t -> t -> bool
  val equal : t -> t -> bool
  val to_name : t -> string
  val to_location : t -> Location.t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val sexp_of_t : t -> Sexp.t
  val hash : t -> int
end = struct
  type t = Uid of string * Location.t [@@deriving compare, sexp]

  let make name loc = Uid (name, loc)

  let make_var (type v) : (module Var with type t = v) -> v -> t option =
   fun (module V) v ->
    if V.is_generated v
    then None
    else
      let open V in
      let name = to_name_exn v in
      let loc = get_location v in
      Option.some @@ make name loc


  let ( = ) (Uid (n1, l1)) (Uid (n2, l2)) = String.equal n1 n2 && Location.equal l1 l2
  let equal = ( = )

  (** Given an UID of format [{name}#{line}:{start_col}-{end_col}], returns only
    the [{name}] part. *)
  let to_name (Uid (name, _loc)) = name

  (** Given an UID of format [{name}#{line}:{start_col}-{end_col}], returns only
    the [{line}:{start_col}-{end_col}] part. *)
  let to_location (Uid (_name, loc)) = loc

  (** Returns an UID of format [{name}#{line}:{start_col}-{end_col}]. *)
  let to_string (Uid (name, loc)) =
    match loc with
    | File region -> Format.sprintf "%s#%s" name (region#compact ~file:false `Point)
    | Virtual v -> Format.sprintf "%s#%s" name v


  let pp : t Fmt.t = fun ppf uid -> Format.fprintf ppf "%s" (to_string uid)
  let hash uid = String.hash (to_string uid)
end

type ('core_expr, 'typed_expr) resolve_case =
  | Core of 'core_expr
  | Resolved of 'typed_expr
  | Unresolved
[@@deriving compare]

type type_case = (Ast_core.type_expression, Ast_typed.type_expression) resolve_case
[@@deriving compare]

type signature_case = (Ast_core.signature, Ast_typed.signature) resolve_case
[@@deriving compare]

(** Whether a [mdef] was declared as a module/namespace or signature/interface. *)
type mdef_type =
  | Module
  | Signature
[@@deriving compare]

type def_type =
  | Local
  | Parameter
  | Module_field
  | Global
[@@deriving compare]

type vdef_attributes =
  | No_attributes
  | Value_attr of Value_attr.t
  | Sig_item of Sig_item_attr.t
[@@deriving compare]

type tdef_attributes =
  | Type_attr of Type_or_module_attr.t
  | Sig_type of Sig_type_attr.t
  | No_attributes
[@@deriving compare]

type mdef_attributes =
  | Module_attr of Type_or_module_attr.t
  | Signature_attr of Signature_attr.t
  | No_attributes
[@@deriving compare]

type vdef =
  { name : string
  ; uid : Uid.t
  ; range : Location.t
  ; decl_range : Location.t
  ; t : type_case
  ; references : LSet.t
  ; def_type : def_type
  ; mod_path : Uid.t list
  ; attributes : vdef_attributes
  }

let compare_vdef (v1 : vdef) (v2 : vdef) : int = Uid.compare v1.uid v2.uid

type tdef =
  { name : string
  ; uid : Uid.t
  ; range : Location.t
  ; decl_range : Location.t
  ; content : Ast_core.type_expression option
        (** The RHS ([u]) of a type definition [type t = u]. For signatures and
            interfaces, the type might be abstract and have no content, e.g.:
            [module type I = sig type t end]. *)
  ; def_type : def_type
  ; references : LSet.t
  ; mod_path : Uid.t list
  ; attributes : tdef_attributes
  }

let compare_tdef (t1 : tdef) (t2 : tdef) : int = Uid.compare t1.uid t2.uid

type mod_name = string [@@deriving compare]

type resolve_mod_name =
  | Unresolved_path of { module_path : Uid.t list }
  | Resolved_path of
      { module_path : Uid.t list
      ; resolved_module_path : Uid.t list
      ; resolved_module : Uid.t
      }
[@@deriving compare]

type alias = { resolve_mod_name : resolve_mod_name } [@@deriving compare]

let get_module_path : resolve_mod_name -> Uid.t list = function
  | Unresolved_path path -> path.module_path
  | Resolved_path path -> path.module_path


type extension = resolve_mod_name [@@deriving compare]

type label_case =
  | Ctor
  | Field

type ldef =
  { name : string
  ; uid : Uid.t
  ; range : Location.t
  ; decl_range : Location.t
  ; references : LSet.t
  ; content : Ast_core.type_expression
  ; def_type : def_type
  ; orig_type_loc : Location.t
        (* Location that points to type variable definition
           or to the declaration itself if variable is absent. *)
  ; label_case : label_case
  ; mod_path : Uid.t list
  }

let compare_ldef (l1 : ldef) (l2 : ldef) : int = Uid.compare l1.uid l2.uid

type implementation =
  | Ad_hoc_signature of def list
  | Standalone_signature_or_module of resolve_mod_name

and mod_case =
  | Def of def list
  | Alias of alias

and mdef =
  { name : mod_name
  ; uid : Uid.t
  ; range : Location.t
  ; decl_range : Location.t
  ; references : LSet.t
  ; mod_case : mod_case
  ; def_type : def_type
  ; mod_path : Uid.t list
  ; signature : signature_case
  ; attributes : mdef_attributes
  ; implements : implementation list
  ; extends : extension list
  ; mdef_type : mdef_type
  ; inlined_name : string option
        (* We want to preserve the original name
           of mangled module when it's inlined.
           This info could be helpful (e.g. in hovers) *)
  }

and def =
  | Variable of vdef
  | Type of tdef
  | Module of mdef
  | Label of ldef
[@@deriving compare]

let compare_mdef (m1 : mdef) (m2 : mdef) : int = Uid.compare m1.uid m2.uid
let equal_def a b = 0 = compare_def a b

let equal_def_by_name (a : def) (b : def) : bool =
  match a, b with
  | Variable x, Variable y -> String.equal x.name y.name
  | Type x, Type y -> String.equal x.name y.name
  | Module x, Module y -> compare_mod_name x.name y.name = 0
  | Label x, Label y -> String.equal x.name y.name
  | (Variable _ | Type _ | Module _ | Label _), (Variable _ | Type _ | Module _ | Label _)
    -> false


let get_def_name = function
  | Variable d -> d.name
  | Type d -> d.name
  | Module d -> d.name
  | Label d -> d.name


let get_def_uid = function
  | Variable d -> d.uid
  | Type d -> d.uid
  | Module d -> d.uid
  | Label d -> d.uid


let get_range = function
  | Type t -> t.range
  | Variable v -> v.range
  | Module m -> m.range
  | Label l -> l.range


let get_decl_range = function
  | Type t -> t.decl_range
  | Variable v -> v.decl_range
  | Module m -> m.decl_range
  | Label l -> l.decl_range


let get_def_type = function
  | Type t -> t.def_type
  | Variable v -> v.def_type
  | Module m -> m.def_type
  | Label l -> l.def_type


let get_mod_path = function
  | Type t -> t.mod_path
  | Variable v -> v.mod_path
  | Module m -> m.mod_path
  | Label l -> l.mod_path


type definitions = { definitions : def list }

let wrap_definitions definitions = { definitions }

(** [mvar_to_id] takes a [Module_var.t] and gives an [Uid.t] of the form
    [{name}#{line}:{start_col}-{end_col}]. *)
let mvar_to_id (m : Module_var.t) : Uid.t =
  let name = Format.asprintf "%a" Module_var.pp m in
  let loc = Module_var.get_location m in
  Uid.make name loc


(** The opposite operation of [mvar_to_id]. This function is unsafe. *)
let id_to_mvar (uid : Uid.t) : Module_var.t =
  let name = Uid.to_name uid in
  let loc = Uid.to_location uid in
  Module_var.of_input_var name ~loc


type 'scope scope_case = Location.t * 'scope list

(* Used in LSP *)
type scope = Uid.t scope_case
type scopes = scope list

(* Used in debugger *)
type inlined_scope = def scope_case
type inlined_scopes = inlined_scope list

let rec flatten_defs : definitions -> def list =
 fun { definitions } ->
  match definitions with
  | [] -> []
  | Module ({ mod_case = Def d; _ } as mdef) :: defs ->
    (* All definitions are flattened. We're not interested in [Def] mod case. *)
    Module { mdef with mod_case = Def [] }
    :: List.concat_map
         ~f:Simple_utils.Function.(flatten_defs <@ wrap_definitions)
         [ d; defs ]
  | def :: defs -> def :: flatten_defs (wrap_definitions defs)


module Uid_map = struct
  include Simple_utils.Map.Make (Uid)

  let of_defs_list (definitions : definitions) : def t =
    definitions
    |> flatten_defs
    |> List.fold_left ~init:empty ~f:(fun acc elt -> add (get_def_uid elt) elt acc)
end
