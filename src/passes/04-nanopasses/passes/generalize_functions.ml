open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

(* REMITODO : create E_fun and e_poly_recursive which do not hold type_params ? *)
(* REMITODO : t_forall on ret_type if any *)

include Flag.No_arg ()

let fun_type_from_parameters ~raise parameters ret_type body =
  let param_types_opt =
    List.map parameters ~f:(fun Param.{ pattern; _ } ->
        Option.map ~f:fst (get_p_typed pattern))
  in
  let fun_type_opt = param_types_opt @ [ ret_type ] in
  let lst =
    trace_option ~raise (recursive_no_annot body) (List.Ne.of_list_opt fun_type_opt)
  in
  List.Ne.map
    (function
      | Some ty -> ty
      | None -> raise.error (recursive_no_annot body))
    lst


let compile ~raise =
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_poly_fun ({ type_params = Some ty_params; _ } as pf) ->
      let body = e_type_abstract_ez ty_params pf.body in
      e_poly_fun ~loc { pf with body; type_params = None }
    | E_poly_recursive ({ lambda = { type_params = Some ty_params; _ } as pf; _ } as x) ->
      let lambda =
        let body = e_type_abstract_ez ty_params pf.body in
        { pf with body; type_params = None }
      in
      e_poly_recursive ~loc { x with lambda }
    | e -> make_e ~loc e
  in
  let declaration : _ declaration_ -> declaration =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | D_fun { is_rec; fun_name; type_params; parameters; ret_type; return } ->
      let body =
        Option.value_map type_params ~default:return ~f:(fun tys ->
            e_type_abstract_ez tys return)
      in
      let let_rhs =
        if is_rec
        then (
          let fun_type = fun_type_from_parameters ~raise parameters ret_type return in
          e_poly_recursive
            ~loc:(get_e_loc return)
            { fun_name
            ; fun_type = Extracted fun_type
            ; lambda = { type_params = None; parameters; ret_type; body }
            })
        else
          e_poly_fun
            ~loc:(get_e_loc return)
            { type_params = None; parameters; ret_type; body }
      in
      d_const
        ~loc
        { type_params = None
        ; pattern = p_var ~loc:(Variable.get_location fun_name) fun_name
        ; rhs_type = None
        ; let_rhs
        }
    | d -> make_d ~loc d
  in
  Fold { idle_fold with expr; declaration }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content =
            ( E_poly_fun { type_params = Some _; _ }
            | E_poly_recursive { lambda = { type_params = Some _; _ }; _ } )
        ; _
        } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  ; declaration =
      (function
      | { wrap_content = D_fun _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let name = __MODULE__
let decompile ~raise:_ = Nothing (* for now ? *)
