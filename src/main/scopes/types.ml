open Ligo_prim

(** Indicates that a variable is generated. *)
let generated_flag = "#?generated"

(** Attempts to get a value's name or returns [generated_flag] if it's generated. *)
let get_binder_name : Value_var.t -> string =
 fun v -> if Value_var.is_generated v then generated_flag else Value_var.to_name_exn v


(** Attempts to get a type's name or returns [generated_flag] if it's generated. *)
let get_type_binder_name : Type_var.t -> string =
 fun v -> if Type_var.is_generated v then generated_flag else Type_var.to_name_exn v


(** Attempts to get a module's name or returns [generated_flag] if it's generated. *)
let get_mod_binder_name : Module_var.t -> string =
 fun v -> if Module_var.is_generated v then generated_flag else Module_var.to_name_exn v


module Location = Simple_utils.Location
module List = Simple_utils.List
module LSet = Location.Location_set
module LMap = Location.Location_map

module Uid : sig
  (** A UID uniquely identifies some value, type, module, or label, by using its name and
      location. *)
  type t [@@deriving compare, sexp]

  (** Create a UID from a variable name and its location. *)
  val make : string -> Location.t -> t

  (** Create a UID from a variable. *)
  val make_var : (module Var with type t = 'v) -> 'v -> t option

  (** Test two UIDs for equality. *)
  val ( = ) : t -> t -> bool

  (** The same as [( = )]. *)
  val equal : t -> t -> bool

  (** Given an UID of format [{name}#{line}:{start_col}-{end_col}], returns only the
      [{name}] part. *)
  val to_name : t -> string

  (** Given an UID of format [{name}#{line}:{start_col}-{end_col}], returns only the
      location based on this part. *)
  val to_location : t -> Location.t

  (** Returns an UID of format [{name}#{line}:{start_col}-{end_col}]. *)
  val to_string : t -> string

  (** Formats an UID as [{name}#{line}:{start_col}-{end_col}]. *)
  val pp : Format.formatter -> t -> unit

  (** Create an S-expression out of this UID. *)
  val sexp_of_t : t -> Sexp.t

  (** Hash this UID. *)
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
  let to_name (Uid (name, _loc)) = name
  let to_location (Uid (_name, loc)) = loc

  let to_string (Uid (name, loc)) =
    match loc with
    | File region -> Format.sprintf "%s#%s" name (region#compact ~file:false `Point)
    | Virtual v -> Format.sprintf "%s#%s" name v


  let pp : t Fmt.t = fun ppf uid -> Format.fprintf ppf "%s" (to_string uid)
  let hash uid = String.hash (to_string uid)
end

(* TODO (#2155): remove Core *)

(** During the scopes pass, we might come across values and modules/namespaces whose types
    or signatures are annotated (core), inferred (resolved), or neither (unresolved). *)
type ('core_expr, 'typed_expr) resolve_case =
  | Core of 'core_expr (** Annotated type or signature for a value or module/namespace. *)
  | Resolved of 'typed_expr
      (** Inferred type or signature for a value or module/namespace. *)
  | Unresolved
      (** Unnanotated value or module/namespace that could not have its type or signature inferred. *)
[@@deriving compare]

(** During the types pass, we might come across values whose types are annotated (core),
    inferred (resolved), or neither (unresolved). *)
type type_case = (Ast_core.type_expression, Ast_typed.type_expression) resolve_case
[@@deriving compare]

(** During the types pass, we might come across modules/namespaces whose signatures are
    annotated (core), inferred (resolved), or neither (unresolved). *)
type signature_case = (Ast_core.signature, Ast_typed.signature) resolve_case
[@@deriving compare]

(** Whether a [mdef] was declared as a module/namespace or signature/interface. *)
type mdef_type =
  | Module
  | Signature
[@@deriving compare]

(** A definition may be declared locally in an expression's body, as a function parameter,
    as a module field, or globally. This data type is used to represent this. *)
type def_type =
  | Local (** Declared locally within an expression's body. *)
  | Parameter (** Declared as function parameter. *)
  | Module_field (** Declared as a module field. *)
  | Global (** Declared globally. *)
[@@deriving compare]

(** An attribute attached to a [vdef]. *)
type vdef_attributes =
  | No_attributes
  | Value_attr of Value_attr.t
  | Sig_item of Sig_item_attr.t
[@@deriving compare]

(** An attribute attached to a [tdef]. *)
type tdef_attributes =
  | Type_attr of Type_or_module_attr.t
  | Sig_type of Sig_type_attr.t
  | No_attributes
[@@deriving compare]

(** An attribute attached to a [mdef]. *)
type mdef_attributes =
  | Module_attr of Type_or_module_attr.t
  | Signature_attr of Signature_attr.t
  | No_attributes
[@@deriving compare]

(** Definition of a value. *)
type vdef =
  { name : string (** The name of this definition. *)
  ; uid : Uid.t (** The UID of this definition. *)
  ; range : Location.t (** The range in which this definition's name is declared. *)
  ; decl_range : Location.t (** The range in which this definition is declared. *)
  ; t : type_case (** The type of this definition. *)
  ; references : LSet.t (** All locations where this definition is referenced. *)
  ; def_type : def_type (** The kind of scope where this is declared. *)
  ; mod_path : Uid.t list (** The module path in which this is declared. *)
  ; attributes : vdef_attributes (** The attributes attached to this definition. *)
  }

(** Compares two [vdefs] by their [uid]s. *)
let compare_vdef (v1 : vdef) (v2 : vdef) : int = Uid.compare v1.uid v2.uid

(** Definition of a type. *)
type tdef =
  { name : string (** The name of this definition. *)
  ; uid : Uid.t (** The UID of this definition. *)
  ; range : Location.t (** The range in which this definition's name is declared. *)
  ; decl_range : Location.t (** The range in which this definition is declared. *)
  ; content : Ast_core.type_expression option
        (** The RHS ([u]) of a type definition [type t = u]. For signatures and
            interfaces, the type might be abstract and have no content, e.g.:
            [module type I = sig type t end], in which case it will be [None]. *)
  ; references : LSet.t (** All locations where this definition is referenced. *)
  ; def_type : def_type (** The kind of scope where this is declared. *)
  ; mod_path : Uid.t list (** The module path in which this is declared. *)
  ; attributes : tdef_attributes (** The attributes attached to this definition. *)
  }

(** Compares two [tdefs] by their [uid]s. *)
let compare_tdef (t1 : tdef) (t2 : tdef) : int = Uid.compare t1.uid t2.uid

(** An [mdef] may refer to another [mdef] (or to none if it's an invalid module path).
    This data type tracks a module path, and if possible, tries to resolve it to remove as
    many indirections as possible, pointing to a definition (not alias). *)
type resolve_mod_name =
  | Unresolved_path of { module_path : Uid.t list (** The original module path. *) }
      (** A path that was not resolved (or could not resolved). *)
  | Resolved_path of
      { module_path : Uid.t list
            (** The original module path (doesn't need to be resolved). *)
      ; resolved_module_path : Uid.t list
            (** Like [module_path], but each individual UID has been resolved. *)
      ; resolved_module : Uid.t
            (** Like [resolved_module_path], but the UID points to the final resolved
                module. *)
      } (** A path that was fully resolved into a definition (not alias). *)
[@@deriving compare]

(** An [mdef] may be an alias to another [mdef]. This data type tracks the resolved (or
    not, if it's an invalid alias) module path. *)
type alias = { resolve_mod_name : resolve_mod_name } [@@deriving compare]

(** Gets the (potentially unresolved) module path of a [resolve_mod_name]. *)
let get_module_path : resolve_mod_name -> Uid.t list = function
  | Unresolved_path path -> path.module_path
  | Resolved_path path -> path.module_path


(** An extension represents a module/namespace that is extended by another
    module/namespace. *)
type extension = resolve_mod_name [@@deriving compare]

(** Whether a [ldef] was declared as a constructor or record field. *)
type label_case =
  | Ctor (** Constructor. *)
  | Field (** Record field. *)

(** Definition of a label (constructor or record field). *)
type ldef =
  { name : string (** The name of this definition. *)
  ; uid : Uid.t (** The UID of this definition. *)
  ; range : Location.t (** The range in which this definition's name is declared. *)
  ; decl_range : Location.t (** The range in which this definition is declared. *)
  ; references : LSet.t (** All locations where this definition is referenced. *)
  ; content : Ast_core.type_expression
        (** The RHS ([t]) of a constructor [C of t] or record field [f : t]. *)
  ; def_type : def_type (** The kind of scope where this is declared. *)
  ; orig_type_loc : Location.t
        (** Location that points to type variable definition or to the declaration itself
            if variable is absent. *)
  ; label_case : label_case
        (** Whether this definition was declared as a constructor or record field. *)
  ; mod_path : Uid.t list (** The module path in which this is declared. *)
  }

(** Compares two [ldefs] by their [uid]s. *)
let compare_ldef (l1 : ldef) (l2 : ldef) : int = Uid.compare l1.uid l2.uid

(** An implementation represents a signature/interface that is implemented by a
    module/namespace. *)
type implementation =
  | Ad_hoc_signature of def list
      (** A "floating" signature/interface, such as the annotation in
          [module M : sig (* ... *) = (* ... *)]. *)
  | Standalone_signature_or_module of resolve_mod_name
      (** A defined signature/interface, such as [S] in [module M : S = (* ... *)]. *)

(** Tracks whether an [mdef] defines fields or aliases to another [mdef]. *)
and mod_case =
  | Def of def list
      (** An [mdef] definition (as opposed to an alias). Contains its fields. *)
  | Alias of alias (** An [mdef] alias (as opposed to a definition). *)

(** Definition of a module/namespace or signature/interface. *)
and mdef =
  { name : string (** The name of this definition. *)
  ; uid : Uid.t (** The UID of this definition. *)
  ; range : Location.t (** The range in which this definition's name is declared. *)
  ; decl_range : Location.t (** The range in which this definition is declared. *)
  ; references : LSet.t (** All locations where this definition is referenced. *)
  ; mod_case : mod_case
        (** Whether this definition defines fields or aliases to another [mdef]. *)
  ; def_type : def_type (** The kind of scope where this is declared. *)
  ; mod_path : Uid.t list (** The module path in which this is declared. *)
  ; signature : signature_case (** The signature/interface of this definition. *)
  ; attributes : mdef_attributes (** The attributes attached to this definition. *)
  ; implements : implementation list
        (** The signatures/interfaces that this definition implements. *)
  ; extends : extension list (** The modules/namespaces that this definition extends. *)
  ; mdef_type : mdef_type
        (** Whether this definition was declared as a module/namespace or
            signature/interface. *)
  ; inlined_name : string option
        (** Preserve the original name of a mangled module when it's inlined. This info
            could be helpful (e.g. in hovers). *)
  }

(** A definition represents a [vdef], [tdef], [mdef], or [ldef]. *)
and def =
  | Variable of vdef
  | Type of tdef
  | Module of mdef
  | Label of ldef
[@@deriving compare]

(** Compares two [mdefs] by their [uid]s. *)
let compare_mdef (m1 : mdef) (m2 : mdef) : int = Uid.compare m1.uid m2.uid

(** Checks two [defs] by their [uid]s. *)
let equal_def a b = 0 = compare_def a b

(** Compares two [def]s by name and level (variable, type, module, label). *)
let compare_def_by_name (a : def) (b : def) : int =
  match a, b with
  | Variable x, Variable y -> String.compare x.name y.name
  | Type x, Type y -> String.compare x.name y.name
  | Module x, Module y -> String.compare x.name y.name
  | Label x, Label y -> String.compare x.name y.name
  | Variable _, _ | Type _, (Module _ | Label _) | Module _, Label _ -> -1
  | Label _, _ | Module _, (Type _ | Variable _) | Type _, Variable _ -> 1


(** Gets the inner [name] field of a definition. *)
let get_def_name = function
  | Variable d -> d.name
  | Type d -> d.name
  | Module d -> d.name
  | Label d -> d.name


(** Gets the inner [uid] field of a definition. *)
let get_def_uid = function
  | Variable d -> d.uid
  | Type d -> d.uid
  | Module d -> d.uid
  | Label d -> d.uid


(** Gets the inner [range] field of a definition. *)
let get_range = function
  | Type t -> t.range
  | Variable v -> v.range
  | Module m -> m.range
  | Label l -> l.range


(** Gets the inner [decl_range] field of a definition. *)
let get_decl_range = function
  | Type t -> t.decl_range
  | Variable v -> v.decl_range
  | Module m -> m.decl_range
  | Label l -> l.decl_range


(** Gets the inner [def_type] field of a definition. *)
let get_def_type = function
  | Type t -> t.def_type
  | Variable v -> v.def_type
  | Module m -> m.def_type
  | Label l -> l.def_type


(** Gets the inner [mod_path] field of a definition. *)
let get_mod_path = function
  | Type t -> t.mod_path
  | Variable v -> v.mod_path
  | Module m -> m.mod_path
  | Label l -> l.mod_path


(** Wraps a {b folded} list of definitions, meaning that it contains just global
    declarations. If we unfolded it by default, then we'd have [2ⁿ] definitions where [n]
    is the maximum module depth. The functions in LSP's [Def] module can be used to deal
    with this data structure more efficiently, unfolding it as needed. *)
type definitions = { definitions : def list }

(** Helper to wrap a list of definitions. Doesn't do any folding or unfolding. *)
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


(** The location in which a scope is valid and the definitions that are in scope. *)
type 'scope scope_case = Location.t * 'scope list

(* Used by the LSP. *)

(** The location in which a scope is valid and the definition UIDs that are in scope. *)
type scope = Uid.t scope_case

(** All the scopes in a document. *)
type scopes = scope list

(* Used by the debugger, *)

(** The location in which a scope is valid and the definitions that are in scope (for the
    debugger). *)
type inlined_scope = def scope_case

(** All the scopes in a document (for the debugger). *)
type inlined_scopes = inlined_scope list

(** The result of running [Scopes.run]. *)
type t =
  { definitions : definitions
        (** All the definitions collected during scoping. Runs all the scopes passes
            (except for [Types_pass] if [with_types] is [false]). *)
  ; program : Ast_typed.program option
        (** Result of type-checking. If [types_pass] is [false], will be [None]. *)
  ; inlined_scopes : inlined_scopes lazy_t
        (** Scoping result, used by the debugger. It's calculated lazily since this field
            is rarely used. *)
  ; lambda_types : Ast_typed.ty_expr LMap.t
        (** A map of all labels whose types are functions. *)
  }

(** Unfolds [definitions], recursively collecting all definitions in [mdef]s in the
    output. We set `mod_case = Def []` to avoid exponential growth. The [Def] module in
    [Lsp_helpers] offers alternatives to also deal with definitions. *)
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


(** A map whose keys are [Uid.t]s. *)
module Uid_map = struct
  include Simple_utils.Map.Make (Uid)

  (** Creates a map with all definitions. The same considerations from [flatten_defs]
      apply here. Used by the debugger. *)
  let of_defs_list (definitions : definitions) : def t =
    definitions
    |> flatten_defs
    |> List.fold_left ~init:empty ~f:(fun acc elt -> add (get_def_uid elt) elt acc)
end
