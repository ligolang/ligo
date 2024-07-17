(* note: As we do not support typed pattern in the checker yet.
  this pass aims to only restrict them to typed variable pattern
  The type is ignored when non-propagatable
*)

open Core
open Ast_unified
open Pass_type
open Errors
module Trace = Simple_utils.Trace
module Location = Simple_utils.Location
module Ligo_option = Simple_utils.Ligo_option
include Flag.No_arg ()

(* Utilities *)

let zip_opt a b =
  let open List in
  match zip a b with
  | Or_unequal_lengths.Ok x -> Some x
  | Or_unequal_lengths.Unequal_lengths -> None


let name = __MODULE__

let annot_if_pvar : ty_expr -> pattern -> pattern =
 fun ty p ->
  match get_p_var p with
  | Some x -> p_var_typed ~loc:(get_p_loc p) ty x
  | None -> p


(* Main *)

let compile ~raise:_ =
  let pattern : _ pattern_ -> pattern =
   fun p ->
    let loc = Location.get_location p in
    match Location.unwrap p with
    | P_list (List lst) ->
      (* this is stupid, but typer do not support list pattern if not empty *)
      List.fold_right lst ~init:(p_list ~loc (List [])) ~f:(fun e acc ->
          p_list ~loc (Cons (e, acc)))
    | P_typed (ty, p') ->
      (match get_p p' with
      | P_var x -> p_var_typed ~loc ty x
      | P_variant (label, psum) ->
        let p_typed_opt =
          let open Ligo_option in
          let* tys = get_t_sum_raw ty in
          let* ty = Non_linear_rows.find_ty tys label in
          let* ty in
          let* psum in
          Option.return @@ annot_if_pvar ty psum
        in
        p_variant ~loc label p_typed_opt
      | P_tuple ptup ->
        let p_typed_opt =
          let open Ligo_option in
          let* ty_opts = get_t_record_raw ty in
          let* tys = Option.all (Non_linear_rows.get_tys ty_opts) in
          let* pty = zip_opt tys ptup in
          Option.return @@ List.map ~f:(fun (ty, p) -> annot_if_pvar ty p) pty
        in
        Option.value_map p_typed_opt ~default:p' ~f:(fun lst -> p_tuple ~loc lst)
      | P_tuple_with_ellipsis ptup ->
        let p_typed_opt =
          let open Ligo_option in
          let* ty_opts = get_t_record_raw ty in
          let* tys = Option.all (Non_linear_rows.get_tys ty_opts) in
          let* pty = zip_opt tys ptup in
          Option.return
          @@ List.map ~f:(fun (ty, { pattern; _ }) -> annot_if_pvar ty pattern) pty
        in
        Option.value_map p_typed_opt ~default:p' ~f:(fun lst -> p_tuple ~loc lst)
      | P_pun_record prec ->
        let p_typed_opt =
          let open Ligo_option in
          let* ty_opts = get_t_record_raw ty in
          let* tys = Option.all @@ Non_linear_rows.get_tys ty_opts in
          let* pty = zip_opt tys prec in
          Option.return
          @@ List.map pty ~f:(fun (ty, field) ->
                 Field.map Fun.id (fun p -> annot_if_pvar ty p) field)
        in
        Option.value_map p_typed_opt ~default:p' ~f:(fun lst -> p_pun_record ~loc lst)
      | _ -> p')
    | p -> make_p ~loc p
  in
  Fold { idle_fold with pattern }


let decompile ~raise:_ =
  let pattern : _ pattern_ -> pattern =
   fun p ->
    let loc = Location.get_location p in
    match Location.unwrap p with
    | P_variant (l, Some p) ->
      if Option.is_some @@ get_p_unit p
      then p_variant ~loc l None
      else p_variant ~loc l (Some p)
    | P_var_typed (ty, v) ->
      let pvar = p_var ~loc v in
      p_typed ~loc ty pvar
    | p -> make_p ~loc p
  in
  Fold { idle_fold with pattern }


let reduction ~(raise : _ Trace.raise) =
  { Iter.defaults with
    pattern =
      (function
      | { wrap_content = P_typed _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }
