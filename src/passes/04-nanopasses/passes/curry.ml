open Ast_unified
open Pass_type
open Simple_utils.Trace
open Simple_utils
open Errors
module Location = Simple_utils.Location
module Binder = Ligo_prim.Binder
module Param = Ligo_prim.Param
include Flag.No_arg ()

let mut_flag = function
  | `Var -> Param.Mutable
  | `Const -> Param.Immutable


let get_var_or_ty_pattern pattern =
  let ty, pattern =
    let rec aux pattern =
      match get_p_typed pattern with
      | Some (ty, pattern) ->
        (* if _ignored is Some <ty>, emit a warning ?*)
        let _ignored, pattern = aux pattern in
        Some ty, pattern
      | None ->
        (match get_p_unit pattern with
        | Some () ->
          let loc = get_p_loc pattern in
          Some (tv_unit ~loc ()), pattern
        | None -> None, pattern)
    in
    aux pattern
  in
  ty, get_p_var pattern


let map_nested_type_abstraction_result expr =
  let rec aux (acc : expr) (f : expr -> expr) : (expr -> expr) * expr =
    let loc = get_e_loc acc in
    match get_e acc with
    | E_type_abstraction { type_binder; result } ->
      aux result (fun x -> f @@ e_type_abstraction ~loc { type_binder; result = x })
    | _ -> f, acc
  in
  aux expr Fun.id


let make_ty_fun ~loc (fun_type : _ Recursive.general_fun_type) =
  match fun_type with
  | User x -> x
  | Extracted ty_lst ->
    let ret, tys =
      let hd, tl = List.Ne.rev ty_lst in
      hd, List.rev tl
    in
    List.fold_right tys ~init:ret ~f:(fun ty acc -> t_fun ~loc (ty, acc))


(* fun_return_types [parameters] [ret_type] associates each parameter of a lambda with
   its return type if possible, e.g:

   parameters == [ Some (a : unit) ; Some (b : nat) ; Some (c : string) ]
   ret_type == Some (int)

   result ==
    [ Some (a:int) , nat -> string -> int 
    ; Some (b:nat) , string -> nat
    ; Some (c:string) , int]
*)
let fun_return_types parameters ret_type : (_ Types.Param.t * ty_expr option) list =
  match parameters with
  | _ :: tl ->
    (* the type of the first parameter is ignored because it won't appear in any lambda return type *)
    let tl_param_type =
      List.map
        ~f:(fun Types.Param.{ pattern; _ } ->
          let pattern_ty_opt, _ = get_var_or_ty_pattern pattern in
          pattern_ty_opt)
        tl
    in
    let loc = Option.value_map ret_type ~f:get_t_loc ~default:Location.generated in
    let rec aux lst =
      match lst with
      | _ :: tl ->
        let ty_sub =
          if List.for_all lst ~f:Option.is_some
          then (
            let fun_tys = List.filter_map ~f:Fun.id lst in
            Some (t_fun_of_list ~loc fun_tys))
          else None
        in
        ty_sub :: aux tl
      | [] -> []
    in
    let ret_types = aux (tl_param_type @ [ ret_type ]) in
    List.zip_exn parameters ret_types
  | [] -> []


(* compile_curry [parameters] [ret_type] [body] build a curried lambda sequence
  based on [parameters]. In case of complex pattern (something else than a variable pattern),
  the parameter will be bound to [body] using a let in construction. *)
let compile_curry ~raise ~loc recursive_opt parameters ret_type body =
  let push_within, body' = map_nested_type_abstraction_result body in
  let nested_lambda_data =
    let param_data = fun_return_types parameters ret_type in
    List.map param_data ~f:(fun ({ param_kind; pattern }, ret_type) ->
        let param_type, v_opt = get_var_or_ty_pattern pattern in
        match v_opt with
        | Some v ->
          let binder = Param.make ~mut_flag:(mut_flag param_kind) v param_type in
          binder, ret_type, None
        | None ->
          let fresh_binder = Variable.fresh ~loc:Location.generated () in
          (* REMITODO : inspect param_kind to know if e_simple_let_in or e_let_mut_in *)
          let prelude let_result =
            e_simple_let_in
              ~loc:(get_e_loc body)
              { binder = pattern
              ; rhs = e_variable ~loc:Location.generated fresh_binder
              ; let_result
              }
          in
          let binder = Param.make ~mut_flag:Immutable fresh_binder param_type in
          binder, ret_type, Some prelude)
  in
  let mk_lambda (binder, ret_type, prelude_opt) acc =
    let result = Option.value_map prelude_opt ~default:acc ~f:(fun f -> f acc) in
    Lambda.{ binder; output_type = ret_type; result }
  in
  match recursive_opt with
  | None ->
    let nested_lambdas =
      List.fold_right nested_lambda_data ~init:body' ~f:(fun x acc ->
          e_lambda ~loc (mk_lambda x acc))
    in
    push_within nested_lambdas
  | Some (fun_name, fun_type) ->
    let (top_param, _, top_prelude_opt), tl =
      match List.Ne.of_list_opt nested_lambda_data with
      | None -> failwith "impossible ?"
      | Some v -> v
    in
    let nested_lambdas =
      List.fold_right tl ~init:body' ~f:(fun x acc -> e_lambda ~loc (mk_lambda x acc))
    in
    let top_binder_ty, top_lambda_ty =
      trace_option ~raise (recursive_no_annot body) @@ get_t_fun fun_type
    in
    let binder = Param.set_ascr top_param top_binder_ty in
    push_within
    @@ e_recursive
         ~loc
         { fun_name
         ; fun_type
         ; lambda = mk_lambda (binder, top_lambda_ty, top_prelude_opt) nested_lambdas
         }


let compile ~raise =
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_poly_fun { type_params = None; parameters; ret_type; body } ->
      compile_curry ~raise ~loc None parameters ret_type body
    | E_poly_recursive
        { fun_name
        ; fun_type
        ; lambda = { type_params = None; parameters; ret_type; body }
        } ->
      let fun_type = make_ty_fun ~loc fun_type in
      compile_curry ~raise ~loc (Some (fun_name, fun_type)) parameters ret_type body
    | E_call (f, args) ->
      let args = Location.unwrap args in
      (match args with
      | [] -> e_application ~loc Application.{ lamb = f; args = e_unit ~loc }
      | _ ->
        List.fold args ~init:f ~f:(fun acc arg ->
            e_application ~loc Application.{ lamb = acc; args = arg }))
    | e -> make_e ~loc e
  in
  Fold { idle_fold with expr }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_poly_fun _ | E_poly_recursive _ | E_call _; _ } ->
        raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let name = __MODULE__
let decompile ~raise:_ = Nothing (* for now ? *)
