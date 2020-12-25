(** Updates the dbs.grouped_by_variable field when new constraints are
   discovered.

    This field contains a map from type variables to lists of
   constraints that are related to that variable (in other words, the
   key appears in the equation).
 *)

open Ast_typed.Types
open UnionFind
open Trace

(* map from (unionfind) variables to constraints containing them *)
type 'typeVariable t = ('typeVariable, constraints) ReprMap.t
let create_state ~cmp =
  let merge : constraints -> constraints -> constraints = fun cs1 cs2 ->
    {
      constructor = cs1.constructor @ cs2.constructor ;
      poly        = cs1.poly        @ cs2.poly        ;
      row         = cs1.row         @ cs2.row         ;
    }
  in
  ReprMap.create ~cmp ~merge

let add_constraints_related_to : _ -> type_variable -> constraints -> _ t -> _ t =
  fun repr variable c state ->
  ReprMap.monotonic_update (repr variable) (function
        None -> c
      | Some (x : constraints) -> {
          constructor = c.constructor @ x.constructor ;
          poly        = c.poly        @ x.poly        ;
          (* tc          = c.tc          @ x.tc          ; *)
          row         = c.row         @ x.row         ;
        })
    state

let add_constraint repr state new_constraint =
  let store_constraint tvars constraints =
    let aux dbs (tvar : type_variable) =
      add_constraints_related_to repr tvar constraints dbs
    in List.fold_left aux state tvars
  in
  match new_constraint with
    SC_Constructor c -> store_constraint [c.tv] {constructor = [c] ; poly = []  ; (* tc = [] ; *) row = []}
  | SC_Row         c -> store_constraint [c.tv] {constructor = []  ; poly = []  ; (* tc = [] ; *) row = [c]}
  | SC_Typeclass   _c -> state (* store_constraint c.args {constructor = []  ; poly = []  ; (\* tc = [c]; *\) row = []} *)
  | SC_Poly        c -> store_constraint [c.tv] {constructor = []  ; poly = [c] ; (* tc = [] ; *) row = []}
  | SC_Alias _ -> failwith "TODO: impossible: tc_alias handled in main solver loop"

exception CouldNotRemove
(* exception NestedFailure of string *)

let rm_constraints_related_to : _ -> type_variable -> constraints -> _ t -> (_ t, _) result =
  fun repr variable c state ->
    (* TODO: remove the empty set if a variable is not associated with
       any constraint after this removal. *)
    (* let rm_typeclass_simpl : c_typeclass_simpl list -> c_typeclass_simpl list -> c_typeclass_simpl list =
     *   fun x c ->
     *     (\* TODO: use a set, not a list. *\)
     *     List.fold_left (fun x' ci ->
     *         try
     *           List.remove_element
     *             ~compare:(fun a b ->
     *                 try
     *                   Ast_typed.Compare.constraint_identifier a.id_typeclass_simpl b.id_typeclass_simpl
     *                 with
     *                   Failure msg -> raise (NestedFailure msg))
     *             ci
     *             x'
     *         with
     *           Failure _msg -> raise CouldNotRemove
     *         | NestedFailure msg -> raise (Failure msg))
     *       x
     *       c in *)
    match
      ReprMap.monotonic_update (repr variable) (function
            None -> raise CouldNotRemove (* Some c *)
          | Some (x : constraints) -> {
              (* TODO: the test for removal in
                 src/test/db_index_tests.ml is commented out because
                 the feature is not implemented yet. *)
              constructor = (assert (List.length c.constructor = 0) (* Only removal of typeclass_simpl implemented for now (the others don't have constraint ids yet) *); x.constructor) ;
              poly        = (assert (List.length c.poly        = 0) (* Only removal of typeclass_simpl implemented for now (the others don't have constraint ids yet) *); x.poly       ) ;
              (* tc          = rm_typeclass_simpl x.tc c.tc         ; *)
              row         = (assert (List.length c.row         = 0) (* Only removal of typeclass_simpl implemented for now (the others don't have constraint ids yet) *); x.row        ) ;
            })
        state
    with
    exception CouldNotRemove -> fail Typer_common.Errors.could_not_remove
    | result -> ok result

let remove_constraint repr state constraint_to_rm =
  let rm_constraint tvars constraints =
    let aux state (tvar : type_variable) =
      rm_constraints_related_to repr tvar constraints state
    in bind_fold_list aux state tvars
  in
  match constraint_to_rm with
      SC_Constructor ({tv ; c_tag = _ ; tv_list} as c) -> rm_constraint (tv :: tv_list)             {constructor = [c] ; poly = []  ; (* tc = [] ; *) row = []}
    | SC_Row         ({tv ; r_tag = _ ; tv_map } as c) -> rm_constraint (tv :: LMap.to_list tv_map) {constructor = []  ; poly = []  ; (* tc = [] ; *) row = [c]}
    | SC_Typeclass   _ -> ok state (* ({tc = _ ; args}            as c) -> rm_constraint args                        {constructor = []  ; poly = []  ; (\* tc = [c]; *\) row = []} *)
    | SC_Poly        ({tv; forall = _}           as c) -> rm_constraint [tv]                        {constructor = []  ; poly = [c] ; (* tc = [] ; *) row = []}
    | SC_Alias { a; b } -> ignore (a,b); fail (Typer_common.Errors.internal_error __LOC__ "can't remove aliasing constraints")
      (* Constraint_databases.merge_constraints a b dbs *)

let merge_aliases =
  fun updater state -> updater.map state

let get_constraints_by_lhs : 'type_variable -> 'type_variable t -> constraints =
  fun variable state ->
  (* get the class of the variable *)
  match ReprMap.find_opt variable state with
    Some l -> l
  | None -> {
      constructor = [] ;
      poly        = [] ;
      (* tc          = [] ; *)
      row         = [] ;
    }
let bindings : 'type_variable t -> ('type_variable * constraints) list = fun state -> ReprMap.bindings state
