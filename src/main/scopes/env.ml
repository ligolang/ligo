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
  j A list of all accessible defintions :
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

open Ligo_prim
open Simple_utils
module LSet = Types.LSet
module LMap = Map.Make (Location)

(******************************************************************************)

module Def = struct
  type def =
    | Variable of Value_var.t
    | Type of Type_var.t
    | Module of Module_var.t

  type t = def

  let get_location = function
    | Variable v -> Value_var.get_location v
    | Type t -> Type_var.get_location t
    | Module m -> Module_var.get_location m


  let pp : Format.formatter -> t -> unit =
   fun ppf def ->
    match def with
    | Variable vvar -> Format.fprintf ppf "Variable <%a>" Value_var.pp vvar
    | Type tvar -> Format.fprintf ppf "Type <%a>" Type_var.pp tvar
    | Module mvar -> Format.fprintf ppf "Module <%a>" Module_var.pp mvar


  let to_types_def_opt (prg_defs : Types.def list) : def -> Types.def option =
   fun new_def ->
    let find_def_opt (def : def) (old_defs : Types.def list) : Types.def option =
      let f (types_def : Types.def) : bool =
        Location.equal (Types.get_range types_def) (get_location def)
      in
      List.find ~f old_defs
    in
    find_def_opt new_def prg_defs
    (* match find_def_opt new_def prg_defs with
    | Some _ as ok -> ok
    | None -> pp Format.err_formatter new_def; None *)


  let defs_to_types_defs (prg_defs : Types.def list) : def list -> Types.def list =
   fun new_defs -> List.filter_map ~f:(to_types_def_opt prg_defs) new_defs
end

type def = Def.t

type defs_or_alias =
  | Defs of def list
  | Alias of Module_var.t

(******************************************************************************)

(** Maps the encountered modules to :
    - their aliasee (in case of module alias)
    - their list of defs (in case of mod expr)

    Two different modules will always correspond to two different entries.

    However, {!Module_var.compare} doesn't account for location, so same-name
    modules will wrongly correspond to the same key, so same entry. 

    This is why we augment {!Module_var} with a location-aware comparison function. *)
module Module_map = struct
  module Mvar_map = Map.Make (struct
    type t = Module_var.t

    let compare m1 m2 =
      match Module_var.compare m1 m2 with
      | 0 -> Location.compare (Module_var.get_location m1) (Module_var.get_location m2)
      | cmp -> cmp
  end)

  include Mvar_map

  type module_map = defs_or_alias Mvar_map.t
  type t = module_map

  (** [resolve_mvar] takes [Module_var.t] and tries to resolve it in the [module_map],
      in case of module alias it recusively resolves the alias in the [module_map]. *)
  let rec resolve_mvar : Module_var.t -> t -> (Module_var.t * def list) option =
   fun mv module_map ->
    let self mv = resolve_mvar mv module_map in
    match Mvar_map.find_opt mv module_map with
    | Some (Defs defs) -> Some (mv, defs)
    | Some (Alias mv') -> self mv'
    | None -> None
end

type module_map = Module_map.t

(******************************************************************************)
(** Environment *)

(** In [env] the record fields represent the following:
    1. [parent] represents the defintions from the parent(ancestors) modules.
    2. [avail_defs] represents the top-level defintions of the current module
        & in the context of an expression it represents the bindings encountered 
        sofar.
    3. [module_map] is a map from [Module_var.t] -> [defs_or_alias] this is a flat
        representation of modules & its contents, this simplifies the handling
        for nested modules *)
type env =
  { parent : def list
  ; avail_defs : def list
  ; module_map : module_map
  }

type t = env

let empty = { parent = []; avail_defs = []; module_map = Module_map.empty }

(******************************************************************************)
(** Lookup functions *)

(* TODO : Doc comment doesn't match function, what was meant here ? *)

(** [find_definition_location] is a helper function which will look up the [env]
    and try to find the [Location.t] of the [def]
    It first tries to look up the [avail_defs] then if it does not find the [def]
    there it looks up the [parent] defs *)
let lookup_def_opt : env -> (def -> bool) -> def option =
 fun env f ->
  let find (defs : def list) = List.find ~f defs in
  match find env.avail_defs with
  | Some def -> Some def
  | None -> find env.parent


(** [vvar_definition_location] looks up the [Value_var.t] in the [env] with the
    help of [find_definition_location] *)
let lookup_vvar_opt : env -> Value_var.t -> def option =
 fun env v ->
  lookup_def_opt env (function
      | Variable v' -> Value_var.equal v v'
      | Type _ | Module _ -> false)


(** [tvar_definition_location] looks up the [Type_var.t] in the [env] with the
    help of [find_definition_location] *)
let lookup_tvar_opt : env -> Type_var.t -> def option =
 fun env t ->
  lookup_def_opt env (function
      | Type t' -> Type_var.equal t t'
      | Variable _ | Module _ -> false)


(* TODO : Shouldn't this one be the same as the two functions above ?? *)

(** [lookup_mvar_in_defs_opt] tries to find the defintion of the [Module_var.t] in the [defs] *)
let lookup_mvar_in_defs_opt : Module_var.t -> def list -> Module_var.t option =
 fun m defs ->
  let open Def in
  List.find defs ~f:(function
      | Variable _ | Type _ -> false
      | Module m' -> Module_var.equal m m')
  |> Option.find_map ~f:(function
         | Module m -> Some m
         | Variable _ | Type _ -> None)


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
  - The list of declaration of that non-alias module
  
  *)
let resolve_mvar
    :  Module_var.t -> def list -> module_map
    -> (Module_var.t * Module_var.t * def list) option
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
    :  Module_var.t List.Ne.t -> def list -> module_map
    -> (Module_var.t * Module_var.t * def list) option
  =
 fun (hd, tl) defs mmap ->
  let self defs mdefs = resolve_mpath defs mdefs mmap in
  match resolve_mvar hd defs mmap with
  | None -> None
  | Some (_real, _m, mdefs) as res ->
    (match tl with
    | [] -> res
    | hd' :: tl' -> self (hd', tl') mdefs)


(**
  Folds over a module path (non-empty list of module variables).

  It calls [resolve_mvar] on each var of the module path, and,
  for each resolved module, calls [f].

  If the path is fully resolved, it folds over the whole list,
  otherwise it stops at the first unresolved variable.
  *)
let rec fold_mpath
    :  Module_var.t List.Ne.t -> def list -> module_map -> init:'a
    -> f:('a -> Module_var.t * Module_var.t * Module_var.t * def list -> 'a) -> 'a
  =
 fun (hd, tl) defs mmap ~init ~f ->
  let self nelist mdefs ~init = fold_mpath nelist mdefs mmap ~init ~f in
  match resolve_mvar hd defs mmap with
  | None -> init
  | Some (real, m, mdefs) ->
    let init = f init (hd, real, m, mdefs) in
    (match tl with
    | [] -> init
    | hd' :: tl' -> self (hd', tl') mdefs ~init)


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
    :  Module_var.t List.Ne.t -> def list -> module_map -> init:'a
    -> f:('a -> Module_var.t * Module_var.t * Module_var.t * def list -> 'a)
    -> 'a * (Module_var.t * Module_var.t * def list) option
  =
 fun (hd, tl) defs mmap ~init ~f ->
  let self mpath defs ~init = fold_resolve_mpath mpath defs mmap ~init ~f in
  match resolve_mvar hd defs mmap with
  | None -> init, None
  | Some (real, m, mdefs) as res ->
    let init = f init (hd, real, m, mdefs) in
    (match tl with
    | [] -> init, res
    | hd' :: tl' -> self (hd', tl') mdefs ~init)


(******************************************************************************)
(** Update functions *)

(** [add_vvar_to_avail_defs] adds [Value_var.t] to [avail_defs] in the [env] *)
let add_vvar : Value_var.t -> env -> env =
 fun v env -> { env with avail_defs = Variable v :: env.avail_defs }


(** [add_tvar_to_avail_defs] adds [Type_var.t] to [avail_defs] in the [env] *)
let add_tvar : Type_var.t -> env -> env =
 fun t env -> { env with avail_defs = Type t :: env.avail_defs }


(** [add_mvar] adds [Module_var.t] to the [avail_defs] and also adds an
    [defs_or_alias] to the [module_map] in the [env] *)
let add_mvar : Module_var.t -> defs_or_alias option -> module_map -> env -> env =
 fun m defs_or_alias_opt module_map env ->
  let env = { env with module_map } in
  Option.fold defs_or_alias_opt ~init:env ~f:(fun env defs_or_alias ->
      { env with
        avail_defs = Module m :: env.avail_defs
      ; module_map = Module_map.add m defs_or_alias env.module_map
      })
