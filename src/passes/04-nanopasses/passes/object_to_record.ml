open Ast_unified
open Pass_type
open Simple_utils.Trace

(* open Simple_utils *)
open Errors
module Location = Simple_utils.Location

let label_of_var x =
  let loc = Variable.get_location x in
  Location.wrap ~loc @@ Label.of_string (Variable.to_name_exn x)


let field_of_property ~raise : expr Object_.property -> (Label.t, expr) Field.t =
 fun p ->
  match p with
  | Property (l, r) ->
    (match get_e l with
    | E_variable v ->
      let label = label_of_var v in
      Complete (label.wrap_content, r)
    | _ -> raise.error @@ unsupported_object_field l)
  | Punned_property x ->
    (match get_e x with
    | E_variable v -> Punned (label_of_var v)
    | _ -> raise.error @@ unsupported_object_field x)
  | Property_rest x -> raise.error @@ unsupported_rest_property x


let field_update_of_property ~raise : expr Object_.property -> expr Update.field =
 fun p ->
  match p with
  | Property (l, r) ->
    (match get_e l with
    | E_variable x ->
      Full_field
        { field_lhs = [ FieldName (Label.of_string @@ Variable.to_name_exn x) ]
        ; field_lens = Lens_Id
        ; field_rhs = r
        }
    | _ ->
      (* could match on E_proj ? maybe ? *)
      raise.error @@ unsupported_update l)
  | Punned_property x ->
    (match get_e x with
    | E_variable x -> Pun (label_of_var x)
    | _ -> raise.error @@ unsupported_update x)
  | Property_rest x -> raise.error @@ unsupported_rest_property x


let compile ~raise ~syntax =
  let expr : (expr, ty_expr, pattern, _, _) expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_object (Property_rest structure, fields) ->
      let update = List.map ~f:(field_update_of_property ~raise) fields in
      e_update ~loc { structure; update }
    | E_object fields ->
      let fields = List.Ne.map (field_of_property ~raise) fields in
      e_record_pun ~loc (List.Ne.to_list fields)
    | e -> make_e ~loc e
  in
  if Syntax_types.equal syntax JsLIGO
  then `Cata { idle_cata_pass with expr }
  else `Cata idle_cata_pass


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_object _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let pass ~raise ~syntax =
  morph
    ~name:__MODULE__
    ~compile:(compile ~raise ~syntax)
    ~decompile:`None (* TODO*)
    ~reduction_check:(reduction ~raise)
