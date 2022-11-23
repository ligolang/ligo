module Location = Simple_utils.Location
module Trace = Simple_utils.Trace
open Trace
open Ligo_prim
open Errors
module I = Type
module O = Ast_typed

type error = Errors.typer_error
type warning = Main_warnings.all
type 'a t = raise:(error, warning) raise -> Substitution.t -> 'a

include Monad.Make (struct
  type nonrec 'a t = 'a t

  let return result ~raise:_ _subst = result
  let bind t ~f ~raise subst = (f (t ~raise subst)) ~raise subst
  let map = `Define_using_bind
end)

let all_lmap (lmap : 'a t Record.LMap.t) : 'a Record.LMap.t t =
 fun ~raise subst -> Record.LMap.map (fun t -> t ~raise subst) lmap


let all_lmap_unit (lmap : unit t Record.LMap.t) : unit t =
 fun ~raise subst -> Record.LMap.iter (fun _label t -> t ~raise subst) lmap


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
    | None -> raise.error (cannot_decode_texists type_ type_.location))
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


let decode type_ ~raise subst =
  (* Attempt to lift the error to the originally decoded type (improves error reporting) *)
  Trace.try_with
    (fun ~raise ~catch:_ -> decode type_ ~raise subst)
    (fun ~catch:_ (`Typer_cannot_decode_texists (_type, loc)) ->
      raise.error
        (cannot_decode_texists
           type_
           ((* pick the best location! *) if Location.is_dummy_or_generated loc then type_.location else loc)))


let check_anomalies ~syntax ~loc eqs matchee_type ~raise _subst =
  Pattern_anomalies.check_anomalies ~raise ~syntax ~loc eqs matchee_type


let run t ~raise subst = t ~raise subst
