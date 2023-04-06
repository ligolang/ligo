open Ast_unified
open Pass_type
open Errors
open Simple_utils.Trace
module Location = Simple_utils.Location
module List = Simple_utils.List
module VarSet = Caml.Set.Make (Variable)

let get_var_pattern pattern =
  let rec aux pattern =
    match get_p_typed pattern with
    | Some (_, pattern) ->
      (* if _ignored is Some <ty>, emit a warning ?*)
      aux pattern
    | None -> get_p_var pattern
  in
  aux pattern


let rec dig ~f e =
  match get_e_type_abstraction e with
  | Some { type_binder; result } ->
    e_type_abstraction ~loc:(get_e_loc e) { type_binder; result = dig ~f result }
  | None ->
    (match get_e_annot e with
    | Some (expr, ty) -> e_annot ~loc:(get_e_loc e) (dig ~f expr, ty)
    | None -> f e)


let recursify ~raise fun_name let_rhs =
  dig
    ~f:(fun let_rhs ->
      match get_e_poly_fun let_rhs with
      | Some { type_params; parameters; ret_type; body } ->
        let is_rec = VarSet.mem fun_name (Free_vars.fv_expr let_rhs) in
        if is_rec
        then (
          let fun_type =
            let param_type_opt =
              List.map ~f:(fun x -> Option.map ~f:fst @@ get_p_typed x.pattern) parameters
            in
            let fun_type_opt = Option.all (param_type_opt @ [ ret_type ]) in
            trace_option ~raise (recursive_no_annot let_rhs) fun_type_opt
          in
          e_poly_recursive
            ~loc:(get_e_loc let_rhs)
            { fun_name
            ; fun_type = Extracted (List.Ne.of_list fun_type)
            ; lambda = { type_params; parameters; ret_type; body }
            })
        else let_rhs
      | None -> let_rhs)
    let_rhs


let mono_binder_opt pattern =
  match get_pattern_binders pattern with
  | [ fun_name ] -> Some fun_name
  | _ -> None


let compile ~raise ~syntax =
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_simple_let_in ({ binder; rhs; _ } as letin) ->
      let f_binder_opt = mono_binder_opt binder in
      let rhs =
        Option.value_map f_binder_opt ~default:rhs ~f:(fun fun_name ->
            recursify ~raise fun_name rhs)
      in
      e_simple_let_in ~loc { letin with rhs }
    | E_let_in
        ({ is_rec = false; type_params = None; lhs = pattern, []; rhs; _ } as letin) ->
      let f_binder_opt = mono_binder_opt pattern in
      let rhs =
        Option.value_map f_binder_opt ~default:rhs ~f:(fun fun_name ->
            recursify ~raise fun_name rhs)
      in
      e_let_in ~loc { letin with rhs }
    | E_let_mut_in
        ({ is_rec = false; type_params = None; lhs = pattern, []; rhs; _ } as letin) ->
      let f_binder_opt = mono_binder_opt pattern in
      let rhs =
        Option.value_map f_binder_opt ~default:rhs ~f:(fun fun_name ->
            recursify ~raise fun_name rhs)
      in
      e_let_mut_in ~loc { letin with rhs }
    | _ -> make_e ~loc e.wrap_content
  in
  let declaration : _ declaration_ -> declaration =
   fun d ->
    let loc = Location.get_location d in
    let ignore = make_d ~loc d.wrap_content in
    match Location.unwrap d with
    | D_const { type_params = None; pattern; rhs_type; let_rhs } ->
      let f_binder_opt = mono_binder_opt pattern in
      let let_rhs =
        Option.value_map f_binder_opt ~default:let_rhs ~f:(fun fun_name ->
            recursify ~raise fun_name let_rhs)
      in
      d_const ~loc { type_params = None; pattern; rhs_type; let_rhs }
    | D_var { type_params = None; pattern; rhs_type; let_rhs } ->
      let f_binder_opt = mono_binder_opt pattern in
      let let_rhs =
        Option.value_map f_binder_opt ~default:let_rhs ~f:(fun fun_name ->
            recursify ~raise fun_name let_rhs)
      in
      d_var ~loc { type_params = None; pattern; rhs_type; let_rhs }
    | _ -> ignore
  in
  if Syntax_types.equal syntax JsLIGO
  then `Cata { idle_cata_pass with expr; declaration }
  else `Cata idle_cata_pass


let reduction = Iter.defaults

let pass ~raise ~syntax =
  morph
    ~name:__MODULE__
    ~compile:(compile ~raise ~syntax)
    ~decompile:`None (* for now .. *)
    ~reduction_check:reduction
