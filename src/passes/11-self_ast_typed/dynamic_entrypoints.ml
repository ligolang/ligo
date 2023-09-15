open Errors
open Ligo_prim
open Simple_utils.Trace
(* Approach:
    For a module with a contract sort:
      - Generate a binder $initial_dynamic_entrypoints of type `dyn_entrypoints`
        holding inital dynamic entrypoints
      - Morph @dyn_entry declarations right-end sides into natural numbers
      - Morph all applications `C_CAST_DYNAMIC_ENTRYPOINT x` to `x`
*)

let loc = Location.generated
let t_dyn_entries = Ast_typed.(t_big_map ~loc (t_nat ~loc ()) (t_bytes ~loc ()))

let e_packed_entry v ty =
  let open Ast_typed in
  let t_pack ty = t_arrow ~loc ty (t_bytes ~loc ()) () in
  let ins = make_e ~loc (e_string @@ Ligo_string.Verbatim "{ PACK }") (t_pack ty) in
  make_e
    ~loc
    (E_raw_code { language = "michelson"; code = e_a_applications ~loc ins [ v ] })
    (t_bytes ~loc ())


let e_dynamic_entries (lst : (Ast_typed.expression * Ast_typed.expression) list) =
  let open Ast_typed in
  let lst' = List.map ~f:(fun (a, b) -> e_a_pair ~loc a b) lst in
  let e_lst =
    let t_list = t_list ~loc (t_pair ~loc (t_nat ~loc ()) (t_bytes ~loc ())) in
    List.fold
      lst'
      ~init:(make_e ~loc (E_constant { cons_name = C_NIL; arguments = [] }) t_list)
      ~f:(fun acc el ->
        make_e ~loc (E_constant { cons_name = C_CONS; arguments = [ el; acc ] }) t_list)
  in
  make_e ~loc (e_big_map_literal e_lst) (t_big_map ~loc (t_nat ~loc ()) (t_bytes ~loc ()))


let is_opted_out rhs : bool =
  let f prev e =
    let continue = true in
    let is_opt_out =
      match Ast_typed.get_e_constant e with
      | Some { cons_name = C_OPT_OUT_ENTRY; _ } -> true
      | _ -> false
    in
    continue, prev || is_opt_out, e
  in
  fst @@ Ast_typed.Helpers.fold_map_expression f false rhs


let map_contract ~storage_type ~parameter_type decls sig_ =
  let open Ast_typed in
  (*
    - morph dynamic entrypoints RHS to nat
    - record dynamic entrypoints indexes and old RHS
  *)
  let (_, dyns), casted_items =
    List.fold_map decls ~init:(0, []) ~f:(fun (ctr, acc) el ->
        let Location.{ wrap_content; location } = el in
        match wrap_content with
        | D_value { binder; attr; expr }
        | D_irrefutable_match { pattern = { wrap_content = P_var binder; _ }; expr; attr }
          when attr.dyn_entry ->
          let nat_rhs = e_a_nat ~loc @@ Z.of_int ctr in
          let d =
            Location.{ wrap_content = D_value { binder; attr; expr = nat_rhs }; location }
          in
          (ctr + 1, (binder, nat_rhs, expr) :: acc), d
        | D_value _ | D_irrefutable_match _ | D_type _ | D_module _ | D_module_include _
          -> (ctr, acc), el)
  in
  match dyns with
  | [] -> decls, sig_
  | _ ->
    let sig_items =
      let new_sig =
        S_value
          ( Magic_vars.initial_dynamic_entrypoints
          , t_dyn_entries
          , { dyn_entry = false; entry = false; view = false } )
      in
      new_sig :: sig_.sig_items
    in
    let initial =
      let binder = Binder.make Magic_vars.initial_dynamic_entrypoints t_dyn_entries in
      let expr =
        let lst =
          dyns
          (* filter out non initial entrypoints *)
          |> List.filter ~f:(fun (_, _, rhs) -> not (is_opted_out rhs))
          (* generate the packing expression *)
          |> List.map ~f:(fun (_, key, rhs) ->
                 key, e_packed_entry rhs rhs.type_expression)
        in
        e_dynamic_entries lst
      in
      Location.wrap ~loc @@ D_value { binder; attr = ValueAttr.default_attributes; expr }
    in
    initial :: casted_items, { sig_ with sig_items }


let map_module module_ sig_ =
  match sig_.Ast_typed.sig_sort with
  | Ss_module -> module_, sig_
  | Ss_contract { storage = storage_type; parameter = parameter_type } ->
    map_contract ~storage_type ~parameter_type module_ sig_


let mapper =
  Helpers.Declaration_mapper.map_module (fun decl ->
      match Location.unwrap decl with
      | Ast_typed.D_module
          { module_binder
          ; module_attr
          ; module_ = { module_content = M_struct module_; module_location; signature }
          ; annotation
          } ->
        let module_, signature = map_module module_ signature in
        Location.wrap ~loc:(Location.get_location decl)
        @@ Ast_typed.D_module
             { module_binder
             ; module_attr
             ; module_ = { module_content = M_struct module_; module_location; signature }
             ; annotation
             }
      | _ -> decl)


let dynamic_entries : Ast_typed.program -> Ast_typed.program =
 fun prg ->
  match prg.pr_sig.sig_sort with
  | Ss_module ->
    let pr_module = mapper prg.pr_module in
    { pr_module; pr_sig = Ast_typed.to_signature pr_module }
  | Ss_contract { storage = storage_type; parameter = parameter_type } ->
    let module_ = mapper prg.pr_module in
    let sig_items = Ast_typed.Misc.to_sig_items module_ in
    let module_, sig_ =
      map_contract ~storage_type ~parameter_type module_ { prg.pr_sig with sig_items }
    in
    { pr_module = module_; pr_sig = sig_ }


(* remove dynamic entries-specialized constants *)
let rm_specialized ~raise (prg : Ast_typed.program) =
  let pr_module =
    Helpers.map_module
      (fun e ->
        let opt =
          let open Simple_utils.Option in
          let* c = Ast_typed.get_e_constant e in
          match c with
          (* C_CAST_DYNAMIC_ENTRYPOINT is used to cast an dyn entry variable to a nat,
             simply remove the cast primitive and keep the variable as is *)
          | { cons_name = C_CAST_DYNAMIC_ENTRYPOINT; arguments = [ x ] } -> Some x
          (* C_OPT_OUT_ENTRY should only appear on dyn entry declaration RHS
             but they are already reduced to nat at this point.
             Simply throw on any remaining *)
          | { cons_name = C_OPT_OUT_ENTRY; _ } ->
            raise.error (illegal_non_initial_dynamic e.location)
          | _ -> None
        in
        Option.value ~default:e opt)
      prg.pr_module
  in
  { prg with pr_module }


let program ~raise prg = prg |> dynamic_entries |> rm_specialized ~raise
