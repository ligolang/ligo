(**
  The [get-scope] LIGO command has to retrieve the following information from a given LIGO source file, including :
   - The definitions : A list of everything that's been defined (all variables, type variables and modules)
   - The scopes : A mapping between defintions and their range of availability (their scope)
   - The references : A mapping between defintions and all the location where they are used.

  In order to retrieve this information, the {!Scopes} module traverses the AST typed of the give LIGO file,
  maintaining an {i environment} telling which defintions are available at which point in the AST,
  and fills in the above structures along the way.

  This module contains the {i environment} type along with the functions to manipulate it.
  An environment is :
  - A list of all accessible defintions :
    - [parent] are the ones defined outside of the current module (in ancestor modules)
    - [avail_defs] are the ones defined in the current module
  - A map from all encountered module to :
    - The list of their definitions inside (if it's a module)
    - The module it points to (if it's a module alias)

  {1  Writing the env (adding and removing defintions)}

  {2 Adding}

  Update of this environment consists in adding a variable, type or module definition to it.
  It should be used by the AST-traversal function at the recursive calls.

  For example, for [let a = rhs in body],
  a recursive AST-traversing [expression] function called on this [E_let_in] with a given [env]
  should recursively call itself on [body] with [env + definition of a],
  because the definitions available in [body] are all the ones already there
  + the new one introduced by the let_in.

  {2 Removing}

  Removing definitions from the env shouldn't be necessary
  since discarding updated env is automatically done
  by discarding of the local variable when exiting this recursive call.

  In above example, once recursive call to [my_body] is done,
  the [env + definition of a], local variable to that recursive call,
  is simply discarded.

  {1 Reading the env (lookup functions)}

  Reading the environment means giving a var from a rhs and determining which declaration it refers to.
  For variable and type variables, it means simply looking into the list of available declarations.

  For module variables, there's one more step. After knowing which module declaration a module var refers to,
  since that module declaration can be an _alias_, we have to use the [module_map] to resolve aliases and know
  which real module it points to.

  For module paths (lists of module variables), like [A.B.C], we have to do the above thing on [A],
  then re-iterate the process on [B] using [A]'s list of declarations, then again on [C].

  {i Note :} When a variable [x] shadows another [x], they will be both in the list of available declarations,
  and lookup will hit on the first [x], thus never hitting the second, this is how shadowing is handled.
*)

open Core
module Value_var = Ligo_prim.Value_var
module Type_var = Ligo_prim.Type_var
module Module_var = Ligo_prim.Module_var
module Label = Ligo_prim.Label
module Location = Simple_utils.Location
module Uid_map = Types.Uid_map

(******************************************************************************)

(** Associates [Uid.t]s to their [Types.def]s.  *)
type def_map = Types.def Uid_map.t

module Def = struct
  (** A simpler form of [Types.def] that only stores a primitive variable. *)
  type t =
    | Variable of Value_var.t
    | Type of Type_var.t
    | Module of Module_var.t
    | Label of Label.t
  [@@deriving equal, compare, sexp]

  (** Make a UID from a [t]. *)
  let make_uid = function
    | Variable v -> Types.Uid.make_var (module Value_var) v
    | Type v -> Types.Uid.make_var (module Type_var) v
    | Module v -> Types.Uid.make_var (module Module_var) v
    | Label (Label (label, loc)) -> Some (Types.Uid.make label loc)


  (** Gets the location of a [t]. *)
  let get_location = function
    | Variable v -> Value_var.get_location v
    | Type t -> Type_var.get_location t
    | Module m -> Module_var.get_location m
    | Label (Label (_, loc)) -> loc


  (** Pretty-print a [t], for debugging. *)
  let pp : Format.formatter -> t -> unit =
   fun ppf def ->
    match def with
    | Variable vvar -> Format.fprintf ppf "Variable <%a>" Value_var.pp vvar
    | Type tvar -> Format.fprintf ppf "Type <%a>" Type_var.pp tvar
    | Module mvar -> Format.fprintf ppf "Module <%a>" Module_var.pp mvar
    | Label (Label (label, _)) -> Format.fprintf ppf "Label <%s>" label


  (** Find the given [t] in the provided [def_map] by its UID and return its [Types.def]
      equivalent. *)
  let to_types_def_opt (prg_defs : def_map) : t -> Types.def option =
   fun new_def ->
    let open Option.Let_syntax in
    let%bind def_uid = make_uid new_def in
    Map.find prg_defs def_uid


  (** Find the given [t]s in the provided [def_map] by their UIDs and return their
      [Types.def] equivalents. If its equivalent is not available, it won't be present in
      the output. *)
  let defs_to_types_defs (prg_defs : def_map) : t list -> Types.def list =
   fun new_defs -> List.filter_map ~f:(to_types_def_opt prg_defs) new_defs


  (** Compares two [t]s by name and level. *)
  let compare_def_by_name (a : t) (b : t) : int =
    let open Types in
    match a, b with
    | Variable x, Variable y -> String.compare (get_binder_name x) (get_binder_name y)
    | Type x, Type y -> String.compare (get_type_binder_name x) (get_type_binder_name y)
    | Module x, Module y -> String.compare (get_mod_binder_name x) (get_mod_binder_name y)
    | Label x, Label y -> Label.T.compare x y
    | Variable _, _ | Type _, (Module _ | Label _) | Module _, Label _ -> -1
    | Label _, _ | Module _, (Type _ | Variable _) | Type _, Variable _ -> 1
end

(** A simpler form of [Types.def] that only stores a primitive variable. *)
type def = Def.t

module Env_make (S : sig
  (** Implementation for definitions (e.g., using a list or a map). OCaml has no HKTs, so
      this type emulates it. *)
  type defs

  (** Creates an empty collection of definitions. *)
  val empty_defs : defs

  (** Tries to find a definition in the collection. *)
  val lookup_def_opt : defs -> def -> def option

  (** Adds a definition to the collection. *)
  val add_def : def -> defs -> defs

  (** Combines two collections of definitions. *)
  val union_defs : defs -> defs -> defs

  (** Pretty-prints the data structure. *)
  val pp_defs : defs Fmt.t
end) =
struct
  (** Simplified version of [Types.mod_case]. *)
  type defs_or_alias =
    | Defs of S.defs
    | Alias of Module_var.t

  (** Pretty print [defs_or_alias] for debugging. *)
  let pp_defs_or_alias (ppf : Format.formatter) : defs_or_alias -> unit = function
    | Defs defs -> Format.fprintf ppf "Defs: %a" S.pp_defs defs
    | Alias var -> Format.fprintf ppf "Alias: %a" Module_var.pp var


  (******************************************************************************)

  (** Maps the encountered modules to:
      - their aliases (in case of module alias)
      - their list of defs (in case of mod expr)

      Two different modules will always correspond to two different entries.

      Moreover, we use [Module_var.t option] so that [None] will represent the global
      module.

      However, {!Module_var.compare} doesn't account for location, so same-name
      modules will wrongly correspond to the same key, so same entry.

      This is why we augment {!Module_var} with a location-aware comparison function. *)
  module Module_map = struct
    module Mvar_map = Map.Make (struct
      type t = Module_var.t option [@@deriving sexp]

      let compare =
        Option.compare (fun m1 m2 ->
            match Module_var.compare m1 m2 with
            | 0 ->
              Location.compare (Module_var.get_location m1) (Module_var.get_location m2)
            | cmp -> cmp)
    end)

    include Mvar_map

    (** Associates module variables to their contents (module definition or module alias). *)
    type t = defs_or_alias Mvar_map.t

    (** [resolve_mvar] takes [Module_var.t] and tries to resolve it in the [module_map],
        in case of module alias it recusively resolves the alias in the [module_map]. *)
    let rec resolve_mvar : Module_var.t -> t -> (Module_var.t * S.defs) option =
     fun mv module_map ->
      match%bind.Option Map.find module_map (Some mv) with
      | Defs defs -> Some (mv, defs)
      | Alias mv' -> resolve_mvar mv' module_map


    (** Pretty-print a module map for debugging. *)
    let pp : t Fmt.t =
     fun ppf module_map ->
      Format.fprintf
        ppf
        "%a"
        Fmt.(list (pair (option Module_var.pp) pp_defs_or_alias))
        (Map.to_alist module_map)
  end

  type module_map = Module_map.t

  (******************************************************************************)
  (** Environment *)

  (** In [env] the record fields represent the following:
    1. [parent] represents the defintions from the parent(ancestors) modules.
    2. [avail_defs] represents the top-level defintions of the current module
        & in the context of an expression it represents the bindings encountered so far.
    3. [module_map] is a map from [Module_var.t] -> [defs_or_alias] this is a flat
        representation of modules & its contents, this simplifies the handling
        for nested modules.
    4. [parent_mod] is the last seen module ([None] means global), used for handling
       inclusions.
    5. [label_types] is a map from all constructors/record fields locations to the types that they belong to.
        {[
          type t = Foo | Bar of int

          let x = Foo
        ]}
        The location of [Foo] inside [let x = ...] would be mapped into
        [type t = Foo | Bar of int]. We need this info to properly
        implement references for constructors and record fields. *)
  type env =
    { parent : S.defs
    ; avail_defs : S.defs
    ; module_map : module_map
    ; parent_mod : Module_var.t option
    ; label_types : Ast_core.ty_expr Location.Map.t
    }

  (** Shorthand alias for [env]. *)
  type t = env

  (** Creates an empty environment, using [None] (global module) as the [parent_mod]. *)
  let empty =
    { parent = S.empty_defs
    ; avail_defs = S.empty_defs
    ; module_map = Module_map.empty
    ; parent_mod = None
    ; label_types = Location.Map.empty
    }


  (** Pretty-print the environment for debugging. *)
  let pp : t Fmt.t =
   fun ppf { parent; avail_defs; module_map; parent_mod; label_types } ->
    Format.fprintf
      ppf
      "{ parent: %a\n\
       ; avail_defs: %a\n\
       ; module_map: %a\n\
       ; parent_mod: %a\n\
       ; label_types: %a\n\
       }"
      S.pp_defs
      parent
      S.pp_defs
      avail_defs
      Module_map.pp
      module_map
      Fmt.Dump.(option Module_var.pp)
      parent_mod
      Fmt.Dump.(list (pair Location.pp Ast_core.PP.type_expression))
      (Map.to_alist label_types)


  (******************************************************************************)
  (** Lookup functions *)

  (** [lookup_def_opt] is a helper function which will look up the [env] and try to find the
      [def] matching the given [def] predicate.
      It first tries to look up the [avail_defs] in the [env], and if it does not find the
      [def] there it looks up the [parent] defs. *)
  let lookup_def_opt : t -> def -> def option =
   fun env def ->
    let find (defs : S.defs) = S.lookup_def_opt defs def in
    match find env.avail_defs with
    | Some def -> Some def
    | None -> find env.parent


  (** [lookup_vvar_opt] looks up the [Value_var.t] in the [env] with the
      help of [lookup_def_opt] *)
  let lookup_vvar_opt : t -> Value_var.t -> def option =
   fun env v -> lookup_def_opt env (Variable v)


  (** [lookup_tvar_opt] looks up the [Type_var.t] in the [env] with the
      help of [lookup_def_opt] *)
  let lookup_tvar_opt : t -> Type_var.t -> def option =
   fun env t -> lookup_def_opt env (Type t)


  (* TODO : Shouldn't this one be the same as the two functions above ?? *)

  (** [lookup_mvar_in_defs_opt] tries to find the defintion of the [Module_var.t] in the [defs] *)
  let lookup_mvar_in_defs_opt : Module_var.t -> S.defs -> Module_var.t option =
   fun m defs ->
    Option.bind (S.lookup_def_opt defs (Module m)) ~f:(function
        | Module m -> Some m
        | Variable _ | Type _ | Label _ -> None)


  (** [resolve_mvar] a [Module_var.t] and looks up in the defs ([def list])
        it gets the module var, and then calls [Module_map.resolve_mvar] for module var.

    Given :
    {[
      module M = struct (* Let's call this var M1 *)
        let a = 42
        let b = 24
        let c = 33
      end
      module N = List (* N1, dummy example to illustrate shadowing *)
      module N = M (* N2, M2 *)
      let x = N.a (* N3 *)
    ]}

    A call to this function with input [N3] will return
    [Some (N2, M1, [a; b; c])]. These elements represent :
    - Which [N] does [N3] refers to (here [N2] and not [N1])
    - Which actual non-alias module does [N2] point to
    - The list of declaration of that non-alias module. *)
  let resolve_mvar
      :  Module_var.t -> S.defs -> module_map
      -> (Module_var.t * Module_var.t * S.defs) option
    =
   fun mv defs module_map ->
    (* First look in the avail defs *)
    match lookup_mvar_in_defs_opt mv defs with
    | None -> None
    | Some real ->
      (* Then look in the map *)
      (match Module_map.resolve_mvar real module_map with
      | None -> None
      | Some (mv, defs) -> Some (real, mv, defs))


  (**
    Resolves a module path (a non-empty list of module variables).

    It calls [resolve_mvar] on each of the variables of the path.

    To resolve [A.B.C], it first resolves [A].
    - If [A] is not resolved, it returns [None].
    - If [A] is resolved, it then tries to resolve [B] using
    [A]'s list of definitions (provided by [resolve_mvar]).

    It re-iterates this process on each variable of the module path.

    If the path is fully resolved, it returns the last module's triplet
    (referee, alisee and def list), otherwise it returns [None].
    *)
  let rec resolve_mpath
      :  Module_var.t Nonempty_list.t -> S.defs -> module_map
      -> (Module_var.t * Module_var.t * S.defs) option
    =
   fun (hd :: tl) defs mmap ->
    match resolve_mvar hd defs mmap with
    | None -> None
    | Some (_real, _m, mdefs) as res ->
      (match tl with
      | [] -> res
      | hd' :: tl' -> resolve_mpath (hd' :: tl') mdefs mmap)


  (**
    Combination of both {!fold_mpath} and {!resolve_mpath} in a single list traversal.

    It calls {!resolve_mvar} on each of the module variable of the input list.

    For each resolved variable, it calls [f] with the list's current element and {!resolve_mvar}'s provided triplet.

    @return If the path is fully resolved, it folds over the whole list,
    and also returns the last module's triplet.

    @return If the path is not fully resolved, it folds until the first unresolved module.

    {i Note:} A triplet is, given for example :
    {[
      module M = struct (* Let's call this var M1 *)
        let a = 42
        let b = 24
        let c = 33
      end
      module N = List (* N1, dummy example to illustrate shadowing *)
      module N = M (* N2, M2 *)
      let x = N.a (* N3 *)
    ]}

    A call to {!resolve_mvar} with input [N3] will return the triplet :
    [Some (N2, M1, [a; b; c])]. These elements represent :
    - Which [N] does [N3] refers to (here [N2] and not [N1])
    - Which actual non-alias module does [N2] point to
    - The list of declaration of that non-alias module

    *)
  let rec fold_resolve_mpath
      :  Module_var.t Nonempty_list.t -> S.defs -> module_map -> init:'a
      -> f:('a -> Module_var.t * Module_var.t * Module_var.t * S.defs -> 'a)
      -> 'a * (Module_var.t * Module_var.t * S.defs) option
    =
   fun (hd :: tl) defs mmap ~init ~f ->
    let self mpath defs ~init = fold_resolve_mpath mpath defs mmap ~init ~f in
    match resolve_mvar hd defs mmap with
    | None -> init, None
    | Some (real, m, mdefs) as res ->
      let init = f init (hd, real, m, mdefs) in
      (match tl with
      | [] -> init, res
      | hd' :: tl' -> self (hd' :: tl') mdefs ~init)


  (******************************************************************************)
  (** Update functions *)

  (** [add_vvar] adds [Value_var.t] to [avail_defs] in the [env]. *)
  let add_vvar : Value_var.t -> t -> t =
   fun v env -> { env with avail_defs = S.add_def (Variable v) env.avail_defs }


  (** [add_tvar] adds [Type_var.t] to [avail_defs] in the [env]. *)
  let add_tvar : Type_var.t -> t -> t =
   fun t env -> { env with avail_defs = S.add_def (Type t) env.avail_defs }


  (** [add_label] adds [Label.t] to [avail_defs] in the [env]. *)
  let add_label : Label.t -> t -> t =
   fun c env -> { env with avail_defs = S.add_def (Label c) env.avail_defs }


  (** [add_mvar] adds the [Module_var.t] to the [avail_defs]. It also adds a [defs_or_alias]
      to the [module_map] in the [env]. *)
  let add_mvar : Module_var.t -> defs_or_alias option -> module_map -> t -> t =
   fun m defs_or_alias_opt module_map env ->
    Option.value_map
      defs_or_alias_opt
      ~default:{ env with module_map }
      ~f:(fun defs_or_alias ->
        { env with
          avail_defs = S.add_def (Module m) env.avail_defs
        ; module_map = Map.set ~key:(Some m) ~data:defs_or_alias module_map
        })


  (** [include_mvar] resolves the given [defs_or_alias option] as what is being included and
      adds it to [env.parent_mod] in the following manner:
      * If the [module_map] doesn't contain [env.parent_module], it will insert the child
        definitions.
      * If it contains the parent module and its value is [Alias _], it will keep this old
        entry.
      * Otherwise, if it contains the parent module and its value is [Defs _], it will be
        concatenated with the child definitions.
      All of this only holds if the given [defs_or_alias_opt] is [Some _] and it could be
      resolved, otherwise the old [env] is just returned updated with the given [module_map]. *)
  let include_mvar : defs_or_alias option -> module_map -> t -> t =
   fun defs_or_alias_opt module_map env ->
    match
      match%bind.Option defs_or_alias_opt with
      | Defs child_defs -> Some child_defs
      | Alias child_binder ->
        let%map.Option _resolved_child_binder, child_defs =
          (* TODO: shouldn't we use [Env.resolve_mvar] instead? Note that it might be [None]
             if [env.avail_defs] is [[]]. *)
          Module_map.resolve_mvar child_binder module_map
        in
        child_defs
    with
    | None -> { env with module_map }
    | Some child_defs ->
      { env with
        avail_defs = S.union_defs child_defs env.avail_defs
      ; module_map =
          Map.change module_map env.parent_mod ~f:(function
              | None -> Some (Defs child_defs)
              | Some (Alias _) as alias -> alias
              | Some (Defs defs') -> Some (Defs (S.union_defs child_defs defs')))
      }


  include S
end

(* Let's keep this implementation for scopes since it relies on the definition order.

   Plus this pass uses mostly modifying operations (like [add_vvar]), so, using lists
   there would be faster. *)
module Env_list_impl = struct
  open Core

  type defs = def list

  let empty_defs : defs = []

  let lookup_def_opt : defs -> def -> def option =
   fun defs def -> List.find ~f:(Def.equal def) defs


  let add_def : def -> defs -> defs = List.cons
  let union_defs : defs -> defs -> defs = ( @ )
  let pp_defs : defs Fmt.t = Fmt.Dump.(list Def.pp)
end

(** List-backed environment, for use by the debugger. *)
module Env_list = Env_make (Env_list_impl)

module Env_map_impl = struct
  (* For some reasons sets are not working here, so, let's use maps instead.

     The basic idea is to store in this map original definitions. [equal] and [compare]
     for [Def.t] don't take into account the location, so, this could be used to resolve
     references. *)
  include Map.Make (Def)

  type defs = def t

  let empty_defs : defs = empty
  let lookup_def_opt : defs -> def -> def option = Map.find

  let add_def : def -> defs -> defs =
   fun def defs -> Map.update defs def ~f:(Fn.const def)


  let to_defs_list : defs -> def list = Map.data

  let union_defs : defs -> defs -> defs =
    Map.merge_skewed ~combine:(fun ~key:_ lhs _rhs -> lhs)


  let pp_defs : defs Fmt.t =
   fun ppf defs -> Format.fprintf ppf "%a" Fmt.Dump.(list Def.pp) (to_defs_list defs)
end

(** Map-backed environment, for use by the language server. *)
module Env_map = Env_make (Env_map_impl)
