open Ast

(* This plug-in system ensures the following:

 * the state of the plug-ins can only store unification vars in a few
   specific types (ReprMap.t and ReprSet.t)

 * when two unification variables are aliased, the plug-in is forced
   to update its state accordingly (it can either update each map/set
   with the supplied function or discard the entire map/set, but it
   cannot forget to update one of the maps/sets in its state; it is
   technically possible to modify the maps/sets (e.g. take a random
   value from one map/set and add it to another) but this would hardly
   be done accidentally).

 * the ReprMap and ReprSet modules only allow monotonic updates
   (additions but no deletions), unless one has access to the
   comparison function (which we do not provide to other modules), and
   the 'typeVariable type is always quantified/hidden in positions
   where it could be used to remove from a map/set or completely empty
   it. *)

(* This is (temporary?) in a functor to give access to
   Typer_errors.typer_error. Maybe this file should be moved to
   src/passes to solve that issue and hardcode the type for errors
   (which would limit extensibility by plugins though). *)
module Dep_cycle (Typer_errors : sig type typer_error end) = struct
  open UnionFind
  (* The types are given in an approximative haskell, because its
     syntax is more readable and more consitent than OCaml's constant
     switch between module types and types *)

  (* merge_keys contains functions which merge the given keys of a map, or elements of a set *)
  (* data merge_keys old new = updater {
       map :: forall v . old → old → map old v → map new v
       set :: old → old → set old → set new
     }
  *)
  type ('old, 'new_) merge_keys = {
    map : 'v . ('old, 'v) ReprMap.t -> ('new_, 'v) ReprMap.t;
    set :       'old      ReprSet.t ->  'new_      ReprSet.t;
    (* var :       'old                ->  'new_               ; *)
  }

  (* Each normalizer returns an updated database (after storing the
     incoming constraint) and a list of constraints, used when the
     normalizer rewrites the constraints e.g. into simpler ones. *)
  (* type normalizer state a b = state → a → (state, set b) *)
  type ('state, 'a , 'b) normalizer = 'state -> 'a -> ('state * 'b PolySet.t)

  (* type normalizer_rm state a = state → a → MonadError typer_error state *)
  type ('state, 'a) normalizer_rm = 'state -> 'a -> ('state, Typer_errors.typer_error) Trace.result

  (* t is the type of the state of the plugin
     data Plugin (t :: 🞰→🞰) = Plugin {
       create_state :: forall typeVariable . (typeVariable → typeVariable → int) → t typeVariable
       add_constraint :: normalizer (t typeVariable) type_constraint_simpl type_constraint_simpl
       remove_constraint :: normalizer_rm (t typeVariable) type_constraint_simpl
       merge_aliases :: forall old new . merge_keys old new → old → old → t old → t new
     }
  *)
  module type Plugin = sig
    type 'typeVariable t
    (* Create the indexer's initial state *)
    val create_state : cmp:('typeVariable -> 'typeVariable -> int) -> 'typeVariable t
    (* Update the state when a constraint is added *)
    val add_constraint : (type_variable -> 'type_variable) -> 'type_variable t -> type_constraint_simpl -> 'type_variable t
    (* Update the state when a constraint is removed *)
    (* TODO: check this API to see if we're giving too much flexibility to the plugin *)
    val remove_constraint : (type_variable -> 'type_variable) -> 'type_variable t -> type_constraint_simpl -> ('type_variable t, Typer_errors.typer_error) Trace.result
    (* Update the state to merge entries of maps and sets of type
       variables.  *)
    val merge_aliases : ('old, 'new_) merge_keys -> 'old t -> 'new_ t
  end

  (* The kind PerPluginType describes type-level functions which take
     a type Plugin.t, and produce an arbitrary type which can depend
     on it.

     e.g. given a module
       Ppt : PerPluginType
     the type
       Ppt.M(SomePlugin).t
     could be one of:
       type_variable SomePlugin.t
       int
       …
  *)
  (* type PerPluginType = 🞰→(🞰→🞰)→🞰 *)
  module type PerPluginTypeArg = sig type 'typeVariable t end (* just the part of Plugin we care about *)
  module type PerPluginType = functor (Plugin : PerPluginTypeArg) -> sig
    type t
  end

  (* These are two useful PerPlugin type-level functions. The first
     gives a `unit' type for each plugin, the second *)
  module PerPluginUnit = functor (Plugin : PerPluginTypeArg) -> struct type t = unit end
  module PerPluginState = functor (Plugin : PerPluginTypeArg) -> struct type t = type_variable Plugin.t end

  module type Monad = sig
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
  end

  module NoMonad = struct
    type 'a t = 'a
    let return x = x
    let bind x ~f = f x
  end

  (* type MappedFunction (t :: 🞰) (Plugin :: 🞰→🞰) =
       Plugin t → MakeInType t → MakeOutType t *)
  module type MappedFunction = sig
    type extra_args
    module MakeInType : PerPluginType
    module MakeOutType : PerPluginType
    module Monad : Monad
    module F(Plugin : Plugin) : sig
      val f : extra_args -> MakeInType(Plugin).t -> MakeOutType(Plugin).t Monad.t
    end
  end

  (* flds is an object containing one method per plug-in *)
  module type Indexer_plugin_fields = functor (Ppt : PerPluginType) -> sig
    type flds

    (* The assignments plug-in must always be present. We force its
       inclusion by asking for a function extracting that field. *)
    module Assignments : sig
      type 'typeVariable t
      val find_opt : 'type_variable -> 'type_variable t -> constructor_or_row option
      val bindings : 'type_variable t -> ('type_variable * constructor_or_row) list
    end
    val assignments : flds -> < assignments : Ppt(Assignments).t >
  end
  module type IndexerPlugins = sig
    module PluginFields : Indexer_plugin_fields

    (* A default value where the field for each plug-in has type unit *)
    val plugin_fields_unit : PluginFields(PerPluginUnit).flds

    (* A function which applies F to each field *)
    module MapPlugins : functor (F : MappedFunction) ->
    sig
      val f :
        F.extra_args ->
        (PluginFields(F.MakeInType).flds) ->
        (PluginFields(F.MakeOutType).flds F.Monad.t)
    end
  end
end
