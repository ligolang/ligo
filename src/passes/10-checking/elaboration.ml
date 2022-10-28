module Location = Simple_utils.Location
open Simple_utils.Trace
open Ligo_prim
module I = Type
module O = Ast_typed

type error = [ `Typer_cannot_decode_texists of Type.t * Location.t ]

let cannot_decode_texists (type_ : Type.t) =
  `Typer_cannot_decode_texists (type_, type_.location)


type 'a t =
  { f :
      'err 'wrn.
      raise:(([> error ] as 'err), 'wrn) raise -> Substitution.t -> 'a
  }
[@@unboxed]

include Monad.Make (struct
  type nonrec 'a t = 'a t

  let return result = { f = (fun ~raise:_ _subst -> result) }

  let bind t ~f =
    { f = (fun ~raise subst -> (f (t.f ~raise subst)).f ~raise subst) }


  let map = `Define_using_bind
end)


let all_lmap _ = assert false
let all_lmap_unit _ = assert false


include Let_syntax

let rec decode (type_ : Type.t) ~raise subst =
  let return type_content : O.type_expression =
    { type_content
    ; type_meta = type_.meta
    ; orig_var = type_.orig_var
    ; location = type_.location
    }
  in
  let decode type_ = decode type_ ~raise subst in
  let decode_row row = decode_row row ~raise subst in
  match type_.content with
  | I.T_variable tvar -> return @@ O.T_variable tvar
  | I.T_exists tvar ->
    (match Substitution.find_texists_eq subst tvar with
    | Some (_, type_) -> decode type_
    | None -> raise.error (cannot_decode_texists type_))
  | I.T_arrow arr ->
    let arr = Arrow.map decode arr in
    return @@ O.T_arrow arr
  | I.T_for_all { ty_binder = tvar; kind; type_ } ->
    let type_ = decode type_ in
    return @@ O.T_for_all { ty_binder = tvar; kind; type_ }
  | I.T_singleton lit -> return @@ O.T_singleton lit
  | I.T_abstraction { ty_binder = tvar; kind; type_ } ->
    let type_ = decode type_ in
    return @@ O.T_abstraction { ty_binder = tvar; kind; type_ }
  | I.T_construct { language; constructor; parameters } ->
    let parameters = List.map parameters ~f:decode in
    return @@ O.T_constant { language; injection = constructor; parameters }
  | I.T_sum row ->
    let row = decode_row row in
    return @@ O.T_sum row
  | I.T_record row ->
    let row = decode_row row in
    return @@ O.T_record row


and decode_layout (layout : Type.layout) ~raise subst =
  let open Layout in
  match layout with
  | L_tree -> L_tree
  | L_comb -> L_comb
  | L_exists lvar ->
    (match Substitution.find_lexists_eq subst lvar with
    | Some layout -> decode_layout layout ~raise subst
    | None -> O.default_layout)


and decode_row_elem (row_elem : Type.row_element) ~raise subst =
  Rows.map_row_element_mini_c (fun type_ -> decode type_ ~raise subst) row_elem


and decode_row ({ fields; layout } : Type.row) ~raise subst =
  let fields =
    Record.map ~f:(fun row_elem -> decode_row_elem row_elem ~raise subst) fields
  in
  let layout = decode_layout layout ~raise subst in
  O.{ fields; layout }


let decode type_ = { f = (fun ~raise subst -> decode type_ ~raise subst) }
let run t ~raise subst = t.f ~raise subst
