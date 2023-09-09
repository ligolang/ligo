open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

(* map ty on attr *)
let map_ty_on_attr
    :  ty_expr -> on_attr:(Attribute.t -> bool)
    -> f:(Attribute.t option -> ty_expr -> ty_expr) -> ty_expr
  =
 fun ty ~on_attr ~f ->
  let rec aux : Attribute.t option -> ty_expr -> ty_expr =
   fun acc ty ->
    let loc = get_t_loc ty in
    match get_t_attr ty with
    | Some (attr, ty) when on_attr attr ->
      let acc = if Option.is_none acc then Some attr else acc in
      aux acc ty
    | Some (attr, ty) -> t_attr ~loc attr (aux acc ty)
    | None -> f acc ty
  in
  aux None ty


let compile_row ~f layout_attr_opt (lst : ty_expr option Non_linear_rows.t) =
  (* setting to default layout, but getting modified afterwards *)
  let lst =
    lst
    |> List.sort ~compare:(fun (_, ra) (_, rb) ->
           let open Non_linear_rows in
           Int.compare ra.decl_pos rb.decl_pos)
    |> List.map ~f
  in
  let layout =
    let labels = List.map ~f:(fun (l, _, a) -> l, a) lst in
    let lfields =
      List.map labels ~f:(fun (name, attr) ->
          let a =
            List.find_map attr ~f:(function
                | Attribute.{ key = "annot"; value = None } -> Some ""
                | Attribute.{ key = "annot"; value } -> value
                | _ -> None)
          in
          Ligo_prim.Layout.{ name; annot = a })
    in
    match layout_attr_opt with
    | Some Attribute.{ key = "layout"; value = Some "tree" } ->
      Ligo_prim.Layout.tree lfields
    | Some Attribute.{ key = "layout"; value = Some "comb" } ->
      Ligo_prim.Layout.comb lfields
    | _ -> Ligo_prim.Layout.default lfields
  in
  let fields = List.map ~f:(fun (a, b, _) -> a, b) lst in
  Row.of_alist_exn ~layout:(Some layout) fields


let compile_row_sum ~loc =
  compile_row ~f:(fun (label, Non_linear_rows.{ associated_type; attributes; _ }) ->
      let ty =
        Option.value_or_thunk associated_type ~default:(fun () ->
            tv_unit ~loc:Location.generated ())
      in
      label, ty, attributes)


let compile_row_record ~loc =
  compile_row ~f:(fun (label, Non_linear_rows.{ associated_type; attributes; _ }) ->
      let ty =
        Option.value_or_thunk associated_type ~default:(fun () ->
            t_var ~loc (Ty_variable.of_input_var ~loc (Label.to_string label)))
      in
      label, ty, attributes)


module Normalize_layout = struct
  include Flag.No_arg ()

  let name = "compute_layout1"

  let compile ~raise:_ =
    let ty_expr : _ ty_expr_ -> ty_expr =
     fun t ->
      let loc = Location.get_location t in
      match Location.unwrap t with
      | T_attr _ as t ->
        map_ty_on_attr
          (make_t ~loc t)
          ~on_attr:(function
            | { key = "layout"; _ } -> true
            | _ -> false)
          ~f:(fun layout_attr_opt ty ->
            match get_t ty with
            | T_sum_raw row -> t_sum ~loc (compile_row_sum ~loc layout_attr_opt row)
            | T_record_raw row ->
              t_record ~loc (compile_row_record ~loc layout_attr_opt row)
            | _ -> ty)
      | t -> make_t ~loc t
    in
    Fold { idle_fold with ty_expr }


  let decompile ~raise:_ = Nothing
  let reduction ~raise:_ = Iter.defaults
end

module Normalize_no_layout = struct
  include Flag.No_arg ()

  let name = "compute_layout2"

  let compile ~raise:_ =
    let ty_expr : _ ty_expr_ -> ty_expr =
     fun t ->
      let loc = Location.get_location t in
      match Location.unwrap t with
      | T_sum_raw row -> t_sum ~loc (compile_row_sum ~loc None row)
      | T_record_raw row -> t_record ~loc (compile_row_record ~loc None row)
      | t -> make_t ~loc t
    in
    Fold { idle_fold with ty_expr }


  let reduction ~raise =
    { Iter.defaults with
      ty_expr =
        (function
        | { wrap_content = T_sum_raw _ | T_record_raw _; _ } ->
          raise.error (wrong_reduction __MODULE__)
        | _ -> ())
    }


  let decompile ~raise:_ = Nothing

  open Unit_test_helpers.Ty_expr

  let%expect_test "type punning" =
    {|
    (T_record_raw
      (((Label x) ((associated_type ()) (decl_pos 0)))
       ((Label y) ((associated_type ()) (decl_pos 1)))))
  |}
    |-> compile;
    [%expect
      {|
      (T_record
       ((fields (((Label x) (T_var x)) ((Label y) (T_var y))))
        (layout
         ((Inner
           ((Field ((name (Label x)) (annot ())))
            (Field ((name (Label y)) (annot ())))))))))
    |}]
end
