open Ast_typed.Types
open Trace

type 'typeVariable t = {
  forwards: (constraint_identifier, refined_typeclass) PolyMap.t ;
  backwards: (constraint_identifier, constraint_identifier) PolyMap.t (* maybe not needed anymore now that c_typeclass_simpl has an "original" field ? *)
}

let set_of_vars l = (PolySet.add_list l (PolySet.create ~cmp:Var.compare)).set

let make_refined_typeclass refined original : refined_typeclass = { refined; original ; vars = set_of_vars refined.args }

let create_state ~cmp:_ = {
  forwards = PolyMap.create ~cmp:Ast_typed.Compare.constraint_identifier ;
  backwards = PolyMap.create ~cmp:Ast_typed.Compare.constraint_identifier ;
}

let add_constraint _repr state new_constraint =
    match new_constraint with
  | SC_Typeclass c ->
    (* dbs.refined_typeclasses : map id → id *)

    (
      match c.original_id with
      | Some original ->
        let metadata = make_refined_typeclass c original in
        {
          forwards = PolyMap.update original (function None -> Some metadata | Some _ -> failwith "can't register a refined typeclass, there is already a refined typeclass for the same original one") state.forwards;
          backwards = PolyMap.update c.id_typeclass_simpl (fun _ -> Some original) state.backwards ;
        }
      | None -> state
    )

    (* (\* stores a copy of the typeclass constraint c if there was no
     *    existing refined_typeclass for it *\)
     * let tc = tc_to_constraint_identifier c in
     * (match PolyMap.find_opt tc state.forwards with
     *    None ->
     *    (\* let copied = {
     *     *   c with
     *     *   is_mandatory_constraint = false;
     *     *   id_typeclass_simpl = ConstraintIdentifier (!global_next_constraint_id);
     *     * } in *\)
     *    let metadata = make_refined_typeclass copied in
     *    let copied' = SC_Typeclass copied in
     *    let state = {
     *      state with
     *      refined_typeclasses = PolyMap.update
     *          tc
     *          (function
     *              Some _existing ->
     *              failwith "Internal error: attempted to register two refined typeclasses for the same typeclass"
     *            | None -> Some metadata)
     *          dbs.refined_typeclasses;
     *      refined_typeclasses_back = PolyMap.update
     *          metadata.refined.id_typeclass_simpl
     *          (function
     *              Some _existing -> 
     *              failwith "Internal error: ???"
     *            | None -> Some tc)
     *          dbs.refined_typeclasses_back
     *    } in
     *    dbs, [copied'; new_constraint]
     *  | Some _ -> state) *)
  | _ -> state

let remove_constraint _repr state constraint_to_remove =
  match constraint_to_remove with
  | SC_Typeclass c ->
    let original =
      try
        PolyMap.find c.id_typeclass_simpl state.backwards
      with
        Not_found ->
        failwith "Internal error: Can't remove refined typeclass: it is not attached to a typeclass."
    in
    ok {
      forwards =
        PolyMap.remove original state.forwards ;
      backwards =
        PolyMap.remove c.id_typeclass_simpl state.backwards ;
    }
  | _ -> ok state

let merge_aliases : 'old 'new_ . ('old, 'new_) merge_keys -> 'old t -> 'new_ t =
  fun _merge_keys state -> { forwards = state.forwards ; backwards = state.backwards }

let find_opt : constraint_identifier -> 'typeVariable t -> refined_typeclass option = fun id state ->
  PolyMap.find_opt id state.forwards
let find : c_typeclass_simpl -> 'typeVariable t -> refined_typeclass = fun c state ->
  match find_opt c.id_typeclass_simpl state with
  | Some x -> x
  | None -> make_refined_typeclass c c.id_typeclass_simpl
let values : 'typeVariable t -> refined_typeclass list = fun m -> List.map snd @@ PolyMap.bindings m.forwards

