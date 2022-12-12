open Ligo_prim

type t =
  { type_subst : (Kind.t * Type.t) Type_var.Map.t
  ; layout_subst : Type.layout Layout_var.Map.t
  }
[@@deriving sexp, compare]

let empty = { type_subst = Type_var.Map.empty; layout_subst = Layout_var.Map.empty }

let add_texists_eq t tvar kind type_ =
  { t with type_subst = Map.set t.type_subst ~key:tvar ~data:(kind, type_) }


let add_lexists_eq t lvar layout =
  { t with layout_subst = Map.set t.layout_subst ~key:lvar ~data:layout }


let find_texists_eq t tvar = Map.find t.type_subst tvar
let find_lexists_eq t lvar = Map.find t.layout_subst lvar

let merge t1 t2 =
  let merge_subst subst1 subst2 =
    Map.merge_skewed subst1 subst2 ~combine:(fun ~key:_ _eq1 eq2 -> eq2)
  in
  { type_subst = merge_subst t1.type_subst t2.type_subst
  ; layout_subst = merge_subst t1.layout_subst t2.layout_subst
  }


module Apply = struct
  let rec type_ subst (t : Type.t) : Type.t =
    let apply = type_ subst in
    let return content = { t with content } in
    match t.content with
    | T_exists tvar ->
      (match find_texists_eq subst tvar with
      | Some (_, t) -> apply t
      | None -> t)
    | T_variable _tvar -> t
    | T_construct construct ->
      let parameters = List.map ~f:apply construct.parameters in
      return @@ T_construct { construct with parameters }
    | T_sum row' ->
      let row = row subst row' in
      return @@ T_sum row
    | T_record row' ->
      let row = row subst row' in
      return @@ T_record row
    | T_arrow arr ->
      let arr = Arrow.map apply arr in
      return @@ T_arrow arr
    | T_singleton _ -> t
    | T_abstraction abs ->
      let abs = Abstraction.map apply abs in
      return @@ T_abstraction abs
    | T_for_all for_all ->
      let for_all = Abstraction.map apply for_all in
      return @@ T_for_all for_all


  and row subst (t : Type.row) : Type.row =
    let fields = Record.map ~f:(row_elem subst) t.fields in
    let layout = layout subst t.layout in
    { fields; layout }


  and row_elem subst (t : Type.row_element) : Type.row_element =
    Rows.map_row_element_mini_c (type_ subst) t


  and layout subst (t : Type.layout) : Type.layout =
    match t with
    | L_tree | L_comb -> t
    | L_exists lvar ->
      (match find_lexists_eq subst lvar with
      | Some t -> layout subst t
      | None -> t)
end
