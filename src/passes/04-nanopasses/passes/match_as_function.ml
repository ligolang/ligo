open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

(* Pattern matching for JsLIGO is implemented as a 'built-in function' as
    JavaScript and TypeScript don't have native pattern matching. *)
include Flag.No_arg ()

let expr_in_block : expr -> block =
 fun body ->
  let loc = get_e_loc body in
  block_of_statements (List.Ne.singleton @@ s_instr ~loc (i_return ~loc (Some body)))


let object_to_matching_clause ~raise
    : expr Object_.property -> (pattern, block) Case.clause
  =
  let pattern_of_params : Variable.t -> pattern Param.t list -> ty_expr option -> pattern =
   fun ctor parameters ret_type ->
    ignore ret_type;
    (* TODO: we ignore the types here.. emit a warning ?*)
    let loc = Variable.get_location ctor in
    let params =
      match parameters with
      | [] -> None
      | [ p ] -> Some p.pattern
      | _ -> Some (p_tuple ~loc (List.map ~f:(fun x -> x.pattern) parameters))
    in
    p_variant
      ~loc:(Variable.get_location ctor)
      (Label.of_string (Variable.to_name_exn ctor))
      params
  in
  function
  | Property (name, value) ->
    (match get_e name, get_e value with
    | E_variable ctor, E_poly_fun { parameters; ret_type; body; type_params = None } ->
      { pattern = pattern_of_params ctor parameters ret_type; rhs = expr_in_block body }
    | E_variable ctor, E_block_poly_fun { parameters; ret_type; body; type_params = None }
      -> { pattern = pattern_of_params ctor parameters ret_type; rhs = body }
    | _, _ -> raise.error (invalid_case name))
  | Punned_property e | Property_rest e ->
    raise.error (unsupported_match_object_property e)


let list_to_matching_clause ~raise : expr -> (pattern, block) Case.clause =
 fun e ->
  match get_e e with
  | E_poly_fun { parameters = [ { pattern; _ } ]; ret_type; body; type_params = None } ->
    ignore ret_type;
    let rhs = expr_in_block body in
    { pattern; rhs }
  | E_block_poly_fun
      { parameters = [ { pattern; _ } ]; ret_type; body; type_params = None } ->
    ignore ret_type;
    (* TODO: warning here, this type is ignored *)
    { pattern; rhs = body }
  | _ -> raise.error (invalid_list_pattern_match (get_e_loc e))


let compile ~raise =
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    let same = make_e ~loc e.wrap_content in
    match Location.unwrap e with
    | E_call (f, { wrap_content = [ matchee; cases ]; location = _ }) ->
      (match get_e f, get_e cases with
      | E_variable v, E_object args when Variable.is_name v "match" ->
        let cases = Simple_utils.List.Ne.map (object_to_matching_clause ~raise) args in
        e_match_block ~loc { expr = matchee; cases }
      | E_variable v, E_list args when Variable.is_name v "match" ->
        let cases = List.map ~f:(list_to_matching_clause ~raise) args in
        (match cases with
        | [] -> raise.error (invalid_list_pattern_match loc)
        | hd :: tl -> e_match_block ~loc { expr = matchee; cases = hd, tl })
      | _ -> same)
    | _ -> same
  in
  Fold { idle_fold with expr }


let reduction ~raise =
  let fail () = raise.error (wrong_reduction __MODULE__) in
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_call (f, _); _ }
        when Option.value_map ~default:false (get_e_variable f) ~f:(fun x ->
                 Variable.is_name x "match") -> fail ()
      | _ -> ())
  }


let name = __MODULE__
let decompile ~raise:_ = Nothing
