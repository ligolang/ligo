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

let all_lmap (lmap : 'a t Label.Map.t) : 'a Label.Map.t t =
 fun ~raise subst -> Map.map ~f:(fun t -> t ~raise subst) lmap


let all_lmap_unit (lmap : unit t Label.Map.t) : unit t =
 fun ~raise subst -> Label.Map.iter ~f:(fun t -> t ~raise subst) lmap


include Let_syntax

let rec decode (type_ : Type.t) ~raise subst =
  let return type_content : O.type_expression =
    { type_content; orig_var = type_.orig_var; location = type_.location }
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


and decode_layout
    (fields : O.type_expression Label.Map.t)
    (layout : Type.layout)
    ~raise
    subst
  =
  match layout with
  | L_concrete layout -> layout
  | L_exists lvar ->
    (match Substitution.find_lexists_eq subst lvar with
    | Some (_, layout) -> decode_layout fields layout ~raise subst
    | None ->
      let default_layout_from_field_set fields =
        O.default_layout
          (fields
          |> Set.to_list
          |> List.map ~f:(fun name -> { Layout.name; annot = None }))
      in
      default_layout_from_field_set (Map.key_set fields))


and decode_row ({ fields; layout } : Type.row) ~raise subst =
  let fields = Map.map ~f:(fun row_elem -> decode row_elem ~raise subst) fields in
  let layout = decode_layout fields layout ~raise subst in
  { fields; layout }


let decode type_ ~raise subst =
  (* Attempt to lift the error to the originally decoded type (improves error reporting) *)
  Trace.try_with
    (fun ~raise ~catch:_ -> decode type_ ~raise subst)
    (fun ~catch:_ (`Typer_cannot_decode_texists (_type, loc)) ->
      raise.error
        (cannot_decode_texists
           type_
           (if (* pick the best location! *) Location.is_dummy_or_generated loc
           then type_.location
           else loc)))


let decode_attribute (attr : Context.Attrs.Value.t) : O.sig_item_attribute =
  { entry = attr.entry; view = attr.view }


let rec decode_sig_item (item : Context.Signature.item) ~raise subst : O.sig_item option =
  match item with
  | S_value (var, type_, attr) ->
    Some (S_value (var, decode ~raise type_ subst, decode_attribute attr))
  | S_type (var, type_, _attr) -> Some (S_type (var, decode ~raise type_ subst))
  | S_module (var, sig_, _attr) ->
    Some (S_module (var, decode_signature ~raise sig_ subst))
  | S_module_type _ -> None


and decode_sig_sort (sort : Context.Signature.sort) ~raise subst : O.signature_sort =
  match sort with
  | Ss_module -> Ss_module
  | Ss_contract { storage; parameter } ->
    Ss_contract
      { storage = decode ~raise storage subst; parameter = decode ~raise parameter subst }


and decode_signature (sig_ : Context.Signature.t) ~raise subst : O.signature =
  let sig_items =
    List.filter_map ~f:(fun item -> decode_sig_item item ~raise subst) sig_.items
  in
  let sig_sort = decode_sig_sort sig_.sort ~raise subst in
  { sig_items; sig_sort }


let check_anomalies ~syntax ~loc eqs matchee_type ~raise _subst =
  Pattern_anomalies.check_anomalies ~raise ~syntax ~loc eqs matchee_type


let run t ~raise subst = t ~raise subst
