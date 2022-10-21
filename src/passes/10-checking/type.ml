module Location = Simple_utils.Location
open Ligo_prim

type t =
  { content : content
  ; meta : (meta[@equal.ignore] [@compare.ignore] [@hash.ignore])
  ; orig_var :
      (Type_var.t option[@equal.ignore] [@compare.ignore] [@hash.ignore])
  ; location : (Location.t[@equal.ignore] [@compare.ignore] [@hash.ignore])
  }

and meta = Ast_core.type_expression option

and content =
  | T_variable of Type_var.t
  | T_exists of Type_var.t
  | T_construct of construct
  | T_sum of row
  | T_record of row
  | T_arrow of t Arrow.t
  | T_singleton of Literal_value.t
  | T_abstraction of t Abstraction.t
  | T_for_all of t Abstraction.t
[@@deriving
  ez
    { prefixes =
        [ ( "make_t"
          , fun ?(loc = Location.generated) content meta : t ->
              { content; location = loc; orig_var = None; meta } )
        ; ("get", fun x -> x.content)
        ]
    ; wrap_constructor =
        ( "content"
        , fun type_content ?loc ?type_meta () ->
            make_t ?loc type_content type_meta )
    ; wrap_get = "content", get
    ; default_get = `Option
    }]

and row =
  { fields : row_element Record.t
  ; layout : layout
  }

and row_element = t Rows.row_element_mini_c

and construct =
  { language : string
  ; constructor : Literal_types.t
  ; parameters : t list
  }

and layout =
  | L_comb
  | L_tree
  | L_exists of Layout_var.t
[@@deriving equal, compare, hash]

let rec free_vars t =
  let module Set = Type_var.Set in
  match t.content with
  | T_variable tvar -> Set.singleton tvar
  | T_exists _ -> Set.empty
  | T_construct { parameters; _ } ->
    parameters |> List.map ~f:free_vars |> Set.union_list
  | T_sum row | T_record row -> free_vars_row row
  | T_arrow arr -> arr |> Arrow.map free_vars |> Arrow.fold Set.union Set.empty
  | T_for_all abs | T_abstraction abs ->
    abs |> Abstraction.map free_vars |> Abstraction.fold Set.union Set.empty
  | T_singleton _ -> Set.empty


and free_vars_row { fields; _ } =
  Record.fold fields ~init:Type_var.Set.empty ~f:(fun fvs row_elem ->
      Set.union (free_vars_row_elem row_elem) fvs)


and free_vars_row_elem row_elem = free_vars row_elem.associated_type

let rec subst ?(free_vars = Type_var.Set.empty) t ~tvar ~type_ =
  let subst t = subst t ~free_vars ~tvar ~type_ in
  let subst_abstraction abs = subst_abstraction abs ~free_vars ~tvar ~type_ in
  let subst_row row = subst_row row ~free_vars ~tvar ~type_ in
  let return content = { t with content } in
  match t.content with
  | T_variable tvar' ->
    if Type_var.(tvar = tvar') then type_ else return @@ T_variable tvar'
  | T_exists tvar' -> return @@ T_exists tvar'
  | T_construct { language; constructor; parameters } ->
    let parameters = List.map ~f:subst parameters in
    return @@ T_construct { language; constructor; parameters }
  | T_sum row ->
    let row = subst_row row in
    return @@ T_sum row
  | T_record row ->
    let row = subst_row row in
    return @@ T_record row
  | T_singleton literal -> return @@ T_singleton literal
  | T_arrow arr ->
    let arr = Arrow.map subst arr in
    return @@ T_arrow arr
  | T_abstraction abs ->
    let abs = subst_abstraction abs in
    return @@ T_abstraction abs
  | T_for_all abs ->
    let abs = subst_abstraction abs in
    return @@ T_for_all abs


and subst_var ?(free_vars = Type_var.Set.empty) t ~tvar ~tvar' =
  subst t ~free_vars ~tvar ~type_:(t_variable tvar' ())


and subst_abstraction
    ?(free_vars = Type_var.Set.empty)
    { ty_binder; kind; type_ }
    ~tvar
    ~type_:type_'
    : _ Abstraction.t
  =
  let subst t = subst t ~free_vars ~tvar ~type_:type_' in
  if Type_var.(tvar = ty_binder)
  then { ty_binder; kind; type_ }
  else if Set.mem free_vars ty_binder
  then (
    let ty_binder' = Type_var.fresh () in
    let type_ = subst_var type_ ~free_vars ~tvar:ty_binder ~tvar':ty_binder' in
    { ty_binder = ty_binder'; kind; type_ = subst type_ })
  else { ty_binder; kind; type_ = subst type_ }


and subst_row ?(free_vars = Type_var.Set.empty) { fields; layout } ~tvar ~type_ =
  { fields = Record.map fields ~f:(subst_row_elem ~free_vars ~tvar ~type_)
  ; layout
  }


and subst_row_elem ?(free_vars = Type_var.Set.empty) row_elem ~tvar ~type_ =
  Rows.map_row_element_mini_c (subst ~free_vars ~tvar ~type_) row_elem

let subst t ~tvar ~type_ =
  let free_vars = free_vars t in
  subst ~free_vars t ~tvar ~type_ 