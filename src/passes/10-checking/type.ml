module Location = Simple_utils.Location
module PP = Simple_utils.PP_helpers
open Ligo_prim

type meta = Ast_core.type_expression option [@@deriving yojson]

type t =
  { content : content
  ; meta : (meta[@equal.ignore] [@compare.ignore] [@hash.ignore] [@sexp.opaque])
  ; orig_var :
      (Type_var.t option[@equal.ignore] [@compare.ignore] [@hash.ignore])
  ; location :
      (Location.t
      [@equal.ignore] [@compare.ignore] [@hash.ignore] [@sexp.opaque])
  }

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
  yojson
  , ez
      { prefixes =
          [ ( "make_t"
            , fun ?(loc = Location.generated) content meta : t ->
                { content; location = loc; orig_var = None; meta } )
          ; ("get", fun x -> x.content)
          ]
      ; wrap_constructor =
          ( "content"
          , fun type_content ?loc ?meta () -> make_t ?loc type_content meta )
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
[@@deriving yojson, equal, sexp, compare, hash]

type constr = ?loc:Location.t -> ?meta:Ast_core.type_expression -> unit -> t

let pp_layout ppf layout =
  match layout with
  | L_comb -> Format.fprintf ppf "comb"
  | L_tree -> Format.fprintf ppf "tree"
  | L_exists lvar -> Format.fprintf ppf "^%a" Layout_var.pp lvar


let pp_lmap_sep value sep ppf lmap =
  let lst = List.sort ~compare:(fun (a, _) (b, _) -> Label.compare a b) lmap in
  let new_pp ppf (k, v) =
    Format.fprintf ppf "@[<h>%a -> %a@]" Label.pp k value v
  in
  Format.fprintf ppf "%a" (PP.list_sep new_pp sep) lst


let pp_lmap_sep_d x = pp_lmap_sep x (PP.tag " ,@ ")

let pp_record_sep value sep ppf (m : 'a Record.t) =
  let lst = Record.LMap.to_kv_list m in
  Format.fprintf ppf "%a" (pp_lmap_sep value sep) lst


let pp_tuple_sep value sep ppf m =
  assert (Record.is_tuple m);
  let lst = Record.tuple_of_record m in
  let new_pp ppf (_, v) = Format.fprintf ppf "%a" value v in
  Format.fprintf ppf "%a" (PP.list_sep new_pp sep) lst


let pp_tuple_or_record_sep_t
    value
    format_record
    sep_record
    format_tuple
    sep_tuple
    ppf
    m
  =
  if Record.is_tuple m
  then Format.fprintf ppf format_tuple (pp_tuple_sep value (PP.tag sep_tuple)) m
  else
    Format.fprintf ppf format_record (pp_record_sep value (PP.tag sep_record)) m


let pp_tuple_or_record_sep_type value =
  pp_tuple_or_record_sep_t
    value
    "@[<h>record[%a]@]"
    " ,@ "
    "@[<h>( %a )@]"
    " *@ "


let rec pp ppf t =
  match t.content with
  | T_variable tvar -> Type_var.pp ppf tvar
  | T_exists tvar -> Format.fprintf ppf "^%a" Type_var.pp tvar
  | T_arrow arr -> Arrow.pp pp ppf arr
  | T_construct construct -> pp_construct ppf construct
  | T_singleton lit -> Literal_value.pp ppf lit
  | T_abstraction abs -> Abstraction.pp_type_abs pp ppf abs
  | T_for_all for_all -> Abstraction.pp_forall pp ppf for_all
  | T_sum row ->
    Format.fprintf
      ppf
      "@[<h>sum[%a]@]"
      (pp_lmap_sep_d pp_row_elem)
      (Record.LMap.to_kv_list_rev row.fields)
  | T_record row -> pp_tuple_or_record_sep_type pp_row_elem ppf row.fields


and pp_construct ppf { constructor; parameters; _ } =
  Format.fprintf
    ppf
    "%s %a"
    (Literal_types.to_string constructor)
    (PP.list_sep_d_par pp)
    parameters


and pp_row_elem ppf (row_elem : row_element) = pp ppf row_elem.associated_type

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


and subst_var t ~tvar ~tvar' =
  subst
    t
    ~free_vars:(Type_var.Set.singleton tvar')
    ~tvar
    ~type_:(t_variable ~loc:(Type_var.get_location tvar') tvar' ())


and subst_abstraction
    ?(free_vars = Type_var.Set.empty)
    { ty_binder; kind; type_ }
    ~tvar
    ~type_:type_'
    : _ Abstraction.t
  =
  let subst t =
    subst t ~free_vars:(Set.add free_vars ty_binder) ~tvar ~type_:type_'
  in
  if Type_var.(tvar = ty_binder)
  then { ty_binder; kind; type_ }
  else if Set.mem free_vars ty_binder
  then (
    let ty_binder' = Type_var.fresh () in
    let type_ = subst_var type_ ~tvar:ty_binder ~tvar':ty_binder' in
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


let destruct_type_abstraction t =
  let rec loop binders t =
    match t.content with
    | T_abstraction { ty_binder; type_; _ } -> loop (ty_binder :: binders) type_
    | _ -> List.rev binders, t
  in
  loop [] t


let default_layout = L_tree

let t_construct constructor parameters ?loc ?meta () : t =
  make_t
    ?loc
    (T_construct { language = Backend.Michelson.name; constructor; parameters })
    meta


let t__type_ ?loc ?meta () : t =
  t_construct Literal_types._type_ [] ?loc ?meta ()
  [@@map
    _type_
    , ( "signature"
      , "chain_id"
      , "string"
      , "bytes"
      , "key"
      , "key_hash"
      , "int"
      , "address"
      , "operation"
      , "nat"
      , "tez"
      , "timestamp"
      , "unit"
      , "bls12_381_g1"
      , "bls12_381_g2"
      , "bls12_381_fr"
      , "never"
      , "mutation"
      , "pvss_key"
      , "baker_hash"
      , "chest_key"
      , "chest"
      , "tx_rollup_l2_address"
      , "michelson_contract"
      , "ast_contract"
      , "int64" )]


let t__type_ t ?loc ?meta () : t =
  t_construct ?loc ?meta Literal_types._type_ [ t ] ()
  [@@map
    _type_
    , ( "list"
      , "set"
      , "contract"
      , "ticket"
      , "sapling_state"
      , "sapling_transaction"
      , "gen" )]


let t__type_ t t' ?loc ?meta () : t =
  t_construct ?loc ?meta Literal_types._type_ [ t; t' ] ()
  [@@map _type_, ("map", "big_map", "typed_address")]


let ez_t_record fields ?loc ?meta ?(layout = default_layout) () : t =
  t_record ?loc ?meta { fields = Record.of_list fields; layout } ()


let t_tuple ts ?loc ?meta () =
  let row_fields =
    List.mapi ts ~f:(fun i t ->
        ( Label.of_int i
        , Rows.
            { associated_type = t; michelson_annotation = None; decl_pos = i } ))
  in
  ez_t_record ?loc ?meta row_fields ()


let t_sum_ez fields ?loc ?meta ?(layout = default_layout) () =
  let fields =
    fields
    |> List.mapi ~f:(fun i (x, y) ->
           ( Label.of_string x
           , ({ associated_type = y; michelson_annotation = None; decl_pos = i }
               : row_element) ))
    |> Record.of_list
  in
  t_sum ?loc ?meta { fields; layout } ()


let t_bool ?loc ?meta () =
  t_sum_ez ?loc ?meta [ "True", t_unit (); "False", t_unit () ] ()


let t_record_with_orig_var row ~orig_var ?loc ?meta () =
  { (t_record row ?loc ?meta ()) with orig_var }


let get_t_construct t constr =
  match t.content with
  | T_construct { constructor = constr'; parameters; _ }
    when Literal_types.equal constr constr' -> Some parameters
  | _ -> None


let get_t_unary_construct t constr =
  match get_t_construct t constr with
  | Some [ a ] -> Some a
  | _ -> None


let get_t_binary_construct t constr =
  match get_t_construct t constr with
  | Some [ a; b ] -> Some (a, b)
  | _ -> None


let get_t__type_ t = get_t_unary_construct t Literal_types._type_
  [@@map
    _type_
    , ( "contract"
      , "list"
      , "set"
      , "ticket"
      , "sapling_state"
      , "sapling_transaction" )]


let get_t__type_ t = get_t_binary_construct t Literal_types._type_
  [@@map _type_, ("map", "big_map")]
