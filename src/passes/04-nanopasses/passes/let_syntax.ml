open Ast_unified
open Pass_type
open Errors
module Trace = Simple_utils.Trace
module Ne_list = Simple_utils.Ne_list
module Location = Simple_utils.Location
module Ligo_option = Simple_utils.Ligo_option
include Flag.No_arg ()

(* this pass handle the let-syntax, e.g:
  let f (type a b) x y : t = ..
  let rec f (type a b) x y : t = ..
  let v : t = ..
  let f x y = ..

  In essence, discriminating functions binding from a regular binding (match)
*)

let type_lhs_to_rhs : ty_expr option -> pattern -> ty_expr option =
 fun ty_opt pattern ->
  let ty_from_pattern =
    match get_p pattern with
    | P_unit -> Some (tv_unit ~loc:(get_p_loc pattern) ())
    | P_typed (ty, _) -> Some ty
    | _ -> None
  in
  match ty_opt with
  | Some _ -> ty_opt
  | None -> ty_from_pattern


(* this handles case like `let rec f = fun x y -> ..` *)
let merge_rhs type_params parameters rhs_type body =
  let parameters =
    List.map parameters ~f:(fun pattern -> Param.{ param_kind = `Const; pattern })
  in
  match get_e_poly_fun body with
  | Some { type_params = in_type_params; parameters = in_parameters; ret_type; body } ->
    let type_params =
      match type_params, in_type_params with
      | Some x, Some y -> Some (Ne_list.append x y)
      | x, None | None, x -> x
    in
    Poly_fun.{ type_params; parameters = parameters @ in_parameters; ret_type; body }
  | None -> Poly_fun.{ type_params; parameters; ret_type = rhs_type; body }


let compile_let_rhs
    ~raise
    ~loc
    is_rec
    fun_pattern
    type_params
    (parameters : pattern list)
    (rhs_type : ty_expr option)
    (body : expr)
  =
  let param_tys_opt : ty_expr list option =
    Option.all (List.map parameters ~f:(fun x -> Option.map ~f:fst (get_p_typed x)))
  in
  let rhs =
    let fun_ = merge_rhs type_params parameters rhs_type body in
    if is_rec
    then (
      let fun_name =
        match get_p fun_pattern with
        | P_var x -> x
        | _ -> failwith "impossible parsing?"
      in
      let fun_type =
        match parameters with
        | [] ->
          (* `let rec f : a -> b -> c = fun x y -> x + y` *)
          Recursive.User (Trace.trace_option ~raise (recursive_no_annot body) rhs_type)
        | _ ->
          (* let rec f (x:a) (x:b) : c = x + y *)
          let ft_opt : ty_expr Nonempty_list.t option =
            let open Ligo_option in
            let* rhs_type in
            let* param_tys_opt in
            let (hd, tl) : ty_expr * ty_expr list =
              match param_tys_opt with
              | [] -> rhs_type, []
              | hd :: tl -> hd, tl @ [ rhs_type ]
            in
            Option.return Ne_list.(hd :: tl)
          in
          Recursive.Extracted (Trace.trace_option ~raise (recursive_no_annot body) ft_opt)
      in
      e_poly_recursive ~loc Recursive.{ fun_name; fun_type; lambda = fun_ })
    else e_poly_fun ~loc fun_
  in
  let lhs =
    let lhs_ty_opt =
      let open Ligo_option in
      let* param_tys_opt in
      let* rhs_type in
      let ft =
        match param_tys_opt with
        | [] -> rhs_type
        | lst -> t_fun_of_list ~loc:(get_t_loc rhs_type) (lst @ [ rhs_type ])
      in
      let generalized =
        match type_params with
        | None -> ft
        | Some type_params -> t_type_forall_ez type_params ft
      in
      Option.return generalized
    in
    Option.value_map lhs_ty_opt ~default:fun_pattern ~f:(fun ty ->
        p_typed ~loc:(get_p_loc fun_pattern) ty fun_pattern)
  in
  lhs, rhs


let compile ~raise =
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_let_in
        { is_rec = false; type_params = None; lhs = [ binder ]; rhs_type; rhs; body } ->
      let rhs_type = type_lhs_to_rhs rhs_type binder in
      let rhs =
        Option.value_map rhs_type ~default:rhs ~f:(fun t ->
            e_annot ~loc:(get_e_loc rhs) (rhs, t))
      in
      e_simple_let_in ~loc { binder; rhs; let_result = body }
    | E_let_in { is_rec; type_params; lhs = fun_pattern :: params; rhs_type; rhs; body }
      ->
      let fun_pattern, rhs =
        compile_let_rhs
          ~raise
          ~loc:(get_e_loc rhs)
          is_rec
          fun_pattern
          type_params
          params
          rhs_type
          rhs
      in
      e_simple_let_in ~loc { binder = fun_pattern; rhs; let_result = body }
    | e -> make_e ~loc e
  in
  let declaration : _ declaration_ -> declaration =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | D_let
        { is_rec = false; type_params = None; pattern = [ pattern ]; rhs_type; let_rhs }
      ->
      let rhs_type = type_lhs_to_rhs rhs_type pattern in
      let expr =
        Option.value_map rhs_type ~default:let_rhs ~f:(fun t ->
            e_annot ~loc:(get_e_loc let_rhs) (let_rhs, t))
      in
      d_irrefutable_match ~loc { pattern; expr }
    | D_let { is_rec; type_params; pattern = fun_pattern :: params; rhs_type; let_rhs } ->
      let fun_pattern, rhs =
        compile_let_rhs
          ~raise
          ~loc:(get_e_loc let_rhs)
          is_rec
          fun_pattern
          type_params
          params
          rhs_type
          let_rhs
      in
      d_const
        ~loc
        Simple_decl.
          { type_params = None; pattern = fun_pattern; rhs_type = None; let_rhs = rhs }
    | D_var { type_params = Some ty_params; pattern; rhs_type; let_rhs } ->
      let rhs_type = Option.map rhs_type ~f:(t_type_forall_ez ty_params) in
      d_var ~loc { type_params = None; pattern; rhs_type; let_rhs }
    | D_const { type_params = Some ty_params; pattern; rhs_type; let_rhs } ->
      let rhs_type = Option.map rhs_type ~f:(t_type_forall_ez ty_params) in
      d_const ~loc { type_params = None; pattern; rhs_type; let_rhs }
    | d -> make_d ~loc d
  in
  Fold { idle_fold with expr; declaration }


let reduction ~(raise : _ Trace.raise) =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_let_in _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  ; declaration =
      (function
      | { wrap_content =
            ( D_var { type_params = Some _; _ }
            | D_const { type_params = Some _; _ }
            | D_let _ )
        ; _
        } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let name = __MODULE__
let decompile ~raise:_ = Nothing (* for now ? *)
