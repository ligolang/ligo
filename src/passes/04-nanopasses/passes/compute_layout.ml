open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

let compile_row ~loc layout_attr_opt (lst : ty_expr option Non_linear_rows.t) =
  (* setting to default layout, but getting modified afterwards *)
  let lst =
    lst
    |> List.sort ~compare:(fun (_, ra) (_, rb) ->
           let open Non_linear_rows in
           Int.compare ra.decl_pos rb.decl_pos)
    |> List.map ~f:(fun (label, Non_linear_rows.{ associated_type; attributes; _ }) ->
           let ty =
             Option.value_or_thunk associated_type ~default:(fun () -> tv_unit ~loc ())
           in
           label, ty, attributes)
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
    let has_row_annots = List.exists lst ~f:(fun (_, _, a) -> not (List.is_empty a)) in
    match layout_attr_opt, has_row_annots with
    | Some Attribute.{ key = "layout"; value = Some "tree" }, _ ->
      Some (Ligo_prim.Layout.tree lfields)
    | Some Attribute.{ key = "layout"; value = Some "comb" }, _ ->
      Some (Ligo_prim.Layout.comb lfields)
    | _, true -> Some (Ligo_prim.Layout.default lfields)
    | _ -> None
  in
  let fields = List.map ~f:(fun (a, b, _) -> a, b) lst in
  Row.of_alist_exn ~layout fields


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


let compile1 =
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
          | T_sum_raw row -> t_sum ~loc (compile_row ~loc layout_attr_opt row)
          | T_record_raw row -> t_record ~loc (compile_row ~loc layout_attr_opt row)
          | _ -> ty)
    | t -> make_t ~loc t
  in
  `Cata { idle_cata_pass with ty_expr }


let compile2 =
  let ty_expr : _ ty_expr_ -> ty_expr =
   fun t ->
    let loc = Location.get_location t in
    match Location.unwrap t with
    | T_sum_raw row -> t_sum ~loc (compile_row ~loc None row)
    | T_record_raw row -> t_record ~loc (compile_row ~loc None row)
    | t -> make_t ~loc t
  in
  `Cata { idle_cata_pass with ty_expr }


let reduction ~raise =
  { Iter.defaults with
    ty_expr =
      (function
      | { wrap_content = T_sum_raw _ | T_record_raw _; _ } ->
        raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let pass1 =
  morph ~name:__MODULE__ ~compile:compile1 ~decompile:`None ~reduction_check:Iter.defaults


let pass2 ~raise =
  morph
    ~name:__MODULE__
    ~compile:compile2
    ~decompile:`None
    ~reduction_check:(reduction ~raise)
