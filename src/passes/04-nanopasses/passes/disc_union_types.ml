open Errors
open Ast_unified
open Pass_type
open Simple_utils.Trace
module StrSet = Caml.Set.Make (String)
module LSet = Caml.Set.Make (Label)

(* morph discriminatory union types to sum-types and switches instructions to pattern matching *)
let name = __MODULE__

include Flag.No_arg ()

type reg =
  { label : Label.t
  ; id_set : StrSet.t
  ; ty : ty_expr
  }

let morph_t_disc ~raise ~err ~loc (rows : ty_expr Non_linear_disc_rows.t) : reg * ty_expr =
  (* all type in disc must be record *)
  let rows_record =
    List.map rows ~f:(fun ((), { associated_type = ty; _ }) ->
        trace_option ~raise err (get_t_record_raw ty))
  in
  (* all type in disc must have a string singleton *)
  let singleton_rows =
    List.map rows_record ~f:(fun rows ->
        let ty_string_rows =
          List.filter_map rows ~f:(fun (label, row) ->
              let Non_linear_rows.{ associated_type = ty; _ } = row in
              Option.map
                (Option.value_map ~default:None ty ~f:get_t_string)
                ~f:(fun str -> label, str))
        in
        match ty_string_rows with
        | [ (label, id) ] ->
          let ty_rest =
            let lst = List.filter ~f:(fun (l', _) -> not (Label.equal l' label)) rows in
            match lst with
            | [] -> None
            | _ -> Some (t_record_raw ~loc lst)
          in
          label, id, ty_rest
        | _ -> raise.error err)
  in
  match singleton_rows with
  | [] -> raise.error err
  | (label, _, _) :: _ ->
    let id_set = StrSet.of_list (List.map ~f:snd3 singleton_rows) in
    let label_set = LSet.of_list (List.map ~f:fst3 singleton_rows) in
    let same_label_different_id =
      LSet.cardinal label_set = 1 && StrSet.cardinal id_set = List.length singleton_rows
    in
    if same_label_different_id
    then (
      let ty =
        let rows =
          List.mapi singleton_rows ~f:(fun decl_pos (_, id, ty) ->
              ( Label.of_string id
              , Non_linear_rows.{ associated_type = ty; attributes = []; decl_pos } ))
        in
        t_sum_raw ~loc rows
      in
      let reg =
        { label; id_set = StrSet.of_list (List.map ~f:snd3 singleton_rows); ty }
      in
      reg, ty)
    else raise.error err


let compile ~raise =
  let default_fold, default_unfold = default_refold_acc ~plus:List.append ~init:[] in
  let detect_t d =
    let loc = Location.get_location d in
    match Location.unwrap d with
    | D_type { name; type_expr = ty, acc } ->
      (match get_t_disc_union ty with
      | Some rows ->
        let err = unsupported_disc_union_type ty in
        let reg, type_expr = morph_t_disc ~raise ~err ~loc:(get_t_loc ty) rows in
        d_type ~loc { name; type_expr }, reg :: acc
      | None -> default_fold.declaration d)
    | _ -> default_fold.declaration d
  in
  let switch_to_match (i, registered_unions) =
    let loc = get_i_loc i in
    let opt =
      let open Simple_utils.Option in
      let* { switchee; cases } = get_i_switch i in
      let cases = List.Ne.to_list cases in
      let* { struct_; path } = get_e_proj switchee in
      let* matchee_var = get_e_variable struct_ in
      let* proj_name = Selection.get_field_name path in
      let* cases =
        Option.all
        @@ List.map
             ~f:(function
               | Switch_case (case, b) ->
                 let* lit = get_e_literal case in
                 (match lit with
                 | Literal_string x -> Some (Simple_utils.Ligo_string.extract x, b)
                 | _ -> None)
               | Switch_default_case _ -> (* not allowed *) None)
             cases
      in
      let* { ty = matching_ty; _ } =
        List.find registered_unions ~f:(fun { label; id_set; _ } ->
            let lst = List.map ~f:fst cases in
            Label.equal label proj_name && StrSet.equal id_set (StrSet.of_list lst))
      in
      let res =
        let cases =
          List.Ne.of_list
          @@ List.map cases ~f:(fun (str, block_opt) ->
                 let pattern =
                   p_variant ~loc (Label.of_string str) (Some (p_var ~loc matchee_var))
                 in
                 let rhs : _ Test_clause.t =
                   match block_opt with
                   | None -> ClauseInstr (i_skip ~loc)
                   | Some block -> ClauseBlock block
                 in
                 Case.{ pattern; rhs })
        in
        let expr = e_annot ~loc:(get_e_loc struct_) (struct_, matching_ty) in
        i_case ~loc { expr; cases }
      in
      return res
    in
    let i = Option.value opt ~default:i in
    default_unfold.instruction (i, registered_unions)
  in
  (* the fold register disc_unions types and morph them to variant 
     the unfold use registered disc_unions and morph switches into matches
  *)
  Refold_acc
    ( { default_fold with declaration = detect_t }
    , { default_unfold with instruction = switch_to_match } )


let reduction ~raise =
  { Iter.defaults with
    ty_expr =
      (function
      | { wrap_content = T_disc_union _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }

(* TODO #1811 *)
let decompile ~raise:_ = Nothing
