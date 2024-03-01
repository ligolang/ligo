module Location = Simple_utils.Location
module Trace = Simple_utils.Trace
open Trace
open Ligo_prim
open Errors
module I = Type
module O = Ast_typed

type error = Errors.typer_error
type warning = Main_warnings.all

type 'a t =
  options:Compiler_options.middle_end
  -> path:Module_var.t list
  -> raise:(error, warning) raise
  -> Substitution.t
  -> 'a

include Monad.Make (struct
  type nonrec 'a t = 'a t

  let return result ~options:_ ~path:_ ~raise:_ _subst = result

  let bind t ~f ~options ~path ~raise subst =
    (f (t ~options ~path ~raise subst)) ~options ~path ~raise subst


  let map = `Define_using_bind
end)

let all_lmap (lmap : 'a t Label.Map.t) : 'a Label.Map.t t =
 fun ~options ~path ~raise subst ->
  Map.map ~f:(fun t -> t ~options ~path ~raise subst) lmap


let all_lmap_unit (lmap : unit t Label.Map.t) : unit t =
 fun ~options ~path ~raise subst ->
  Label.Map.iter ~f:(fun t -> t ~options ~path ~raise subst) lmap


include Let_syntax

let rec decode
    (type_ : Type.t)
    ~(options : Compiler_options.middle_end)
    ~path
    ~raise
    subst
  =
  let decode type_ = decode type_ ~options ~raise ~path subst in
  let decode_row row = decode_row row ~options ~path ~raise subst in
  let return type_content : O.type_expression =
    { type_content
    ; abbrev =
        Option.map type_.abbrev ~f:(fun { orig_var; applied_types } ->
            O.{ orig_var; applied_types = List.map ~f:decode applied_types })
    ; location = type_.location
    }
  in
  match type_.content with
  | I.T_variable tvar -> return @@ O.T_variable tvar
  | I.T_exists tvar ->
    (match Substitution.find_texists_eq subst tvar with
    | Some (_, type_) -> decode type_
    | None ->
      let error = cannot_decode_texists tvar type_ type_.location in
      if options.typer_error_recovery
      then (
        raise.log_error error;
        return @@ O.T_exists tvar)
      else raise.error error)
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
  | I.T_sum (row, orig_label) ->
    let row = decode_row row in
    return @@ O.T_sum (row, orig_label)
  | I.T_record row ->
    let row = decode_row row in
    return @@ O.T_record row


and decode_layout
    (fields : O.type_expression Label.Map.t)
    (layout : Type.layout)
    ~options
    ~path
    ~raise
    subst
  =
  match layout with
  | L_concrete layout -> layout
  | L_exists lvar ->
    (match Substitution.find_lexists_eq subst lvar with
    | Some (_, layout) -> decode_layout fields layout ~options ~path ~raise subst
    | None ->
      let default_layout_from_field_set fields =
        O.default_layout
          (fields
          |> Set.to_list
          |> List.map ~f:(fun name -> { Layout.name; annot = None }))
      in
      default_layout_from_field_set (Map.key_set fields))


and decode_row ({ fields; layout } : Type.row) ~options ~path ~raise subst =
  let fields =
    Map.map ~f:(fun row_elem -> decode row_elem ~options ~path ~raise subst) fields
  in
  let layout = decode_layout fields layout ~options ~path ~raise subst in
  { fields; layout }


let decode type_ ~(options : Compiler_options.middle_end) ~path ~raise subst =
  (* Attempt to lift the error to the originally decoded type (improves error reporting) *)
  Trace.try_with
    ~fast_fail:raise.fast_fail
    (fun ~raise ~catch:_ -> decode type_ ~options ~path ~raise subst)
    (fun ~catch:_ (`Typer_cannot_decode_texists (tvar, _type, loc)) ->
      let loc =
        if (* pick the best location! *) Location.is_dummy_or_generated loc
        then type_.location
        else loc
      in
      let error = cannot_decode_texists tvar type_ loc in
      if options.typer_error_recovery
      then (
        raise.log_error error;
        O.t_exists tvar ~loc ())
      else raise.error error)


let decode_attribute (attr : Context.Attrs.Value.t) : Sig_item_attr.t =
  { entry = attr.entry
  ; dyn_entry = attr.dyn_entry
  ; view = attr.view
  ; optional = attr.optional
  ; leading_comments = attr.leading_comments
  }


let rec decode_sig_item
    (item : Context.Signature.item Location.wrap)
    ~options
    ~path
    ~raise
    subst
    : O.sig_item option
  =
  Option.map ~f:(Location.wrap ~loc:(Location.get_location item))
  @@
  match Location.unwrap item with
  | S_value (var, type_, attr) ->
    Some
      (O.S_value (var, decode ~options ~path ~raise type_ subst, decode_attribute attr))
  | S_type (var, type_, attr) ->
    Some
      (S_type
         ( var
         , decode ~options ~path ~raise type_ subst
         , { leading_comments = attr.leading_comments } ))
  | S_type_var (var, attr) ->
    Some (S_type_var (var, { leading_comments = attr.leading_comments }))
  | S_module (var, sig_, _attr) ->
    Some
      (S_module (var, decode_signature ~options ~path:(path @ [ var ]) ~raise sig_ subst))
  | S_module_type (var, sig_, attr) ->
    Some
      (S_module_type
         (var, decode_signature ~options ~path:(path @ [ var ]) ~raise sig_ subst))


and decode_sig_sort (sort : Context.Signature.sort) ~options ~path ~raise subst
    : O.signature_sort
  =
  match sort with
  | Ss_module -> Ss_module
  | Ss_contract { storage; parameter } ->
    Ss_contract
      { storage = decode ~options ~path ~raise storage subst
      ; parameter = decode ~options ~path ~raise parameter subst
      }


and decode_signature (sig_ : Context.Signature.t) ~options ~path ~raise subst
    : O.signature
  =
  let sig_items =
    List.filter_map
      ~f:(fun item -> decode_sig_item item ~options ~path ~raise subst)
      sig_.items
  in
  let sig_sort = decode_sig_sort sig_.sort ~options ~path ~raise subst in
  { sig_items; sig_sort }


let check_anomalies ~syntax ~loc eqs matchee_type ~options:_ ~path:_ ~raise _subst =
  Pattern_anomalies.check_anomalies ~raise ~syntax ~loc eqs matchee_type


let run t ~options ~path ~raise subst = t ~options ~path ~raise subst
