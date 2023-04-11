open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

(* Pattern matching for JsLIGO is implemented as a 'built-in function' as
    JavaScript and TypeScript don't have native pattern matching. *)

let block_to_expr : block -> expr =
 fun block ->
  let last_s, stmts = Simple_utils.List.Ne.rev (get_b block) in
  let last =
    let loc = get_s_loc last_s in
    match get_s last_s with
    | S_instr x ->
      (match get_i x with
      | I_return x -> Option.value_map ~default:(e_unit ~loc) ~f:Fun.id x
      (* see https://tezos-dev.slack.com/archives/GMHV0U3Q9/p1670852612146059 *)
      | I_expr x -> x
      | _ -> e_unit ~loc)
    | _ -> e_unit ~loc
  in
  let body =
    let loc =
      List.fold
        ~init:(get_s_loc last_s)
        ~f:(fun acc x -> Location.cover acc (get_s_loc x))
        (last_s :: stmts)
    in
    match List.rev stmts with
    | [] -> last
    | hd :: tl -> e_block_with ~loc { block = block_of_statements (hd, tl); expr = last }
  in
  body


let object_to_matching_clause ~raise
    : expr Object_.property -> (pattern, expr) Case.clause
  =
  let tocase
      ctor
      (parameters : pattern Param.t list)
      (ret_type : ty_expr option)
      (body : expr)
      : (pattern, expr) Case.clause
    =
    ignore ret_type;
    (* TODO: we ignore the types here.. emit a warning ?*)
    let pattern =
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
    { pattern; rhs = body }
  in
  function
  | Property (name, value) ->
    (match get_e name, get_e value with
    | E_variable ctor, E_poly_fun { parameters; ret_type; body; type_params = None } ->
      tocase ctor parameters ret_type body
    | E_variable ctor, E_block_poly_fun { parameters; ret_type; body; type_params = None }
      -> tocase ctor parameters ret_type (block_to_expr body)
    | _, _ -> raise.error (invalid_case name))
  | Punned_property e | Property_rest e ->
    raise.error (unsupported_match_object_property e)


let list_to_matching_clause ~raise : expr -> (pattern, expr) Case.clause =
 fun e ->
  match get_e e with
  | E_poly_fun { parameters = [ { pattern; _ } ]; ret_type; body; type_params = None } ->
    ignore ret_type;
    { pattern; rhs = body }
  | E_block_poly_fun
      { parameters = [ { pattern; _ } ]; ret_type; body; type_params = None } ->
    ignore ret_type;
    (* TODO: warning here, this type is ignored *)
    let rhs = block_to_expr body in
    { pattern; rhs }
  | _ -> raise.error (invalid_list_pattern_match (get_e_loc e))


let compile ~raise ~syntax =
  ignore raise;
  let pass_expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    let same = make_e ~loc e.wrap_content in
    match Location.unwrap e with
    | E_call (f, { wrap_content = [ matchee; cases ]; location = _ }) ->
      (match get_e f, get_e cases with
      | E_variable v, E_object args when Variable.is_name v "match" ->
        let cases = Simple_utils.List.Ne.map (object_to_matching_clause ~raise) args in
        e_match ~loc { expr = matchee; cases }
      | E_variable v, E_list args when Variable.is_name v "match" ->
        let cases = List.map ~f:(list_to_matching_clause ~raise) args in
        (match cases with
        | [] -> raise.error (invalid_list_pattern_match loc)
        | hd :: tl -> e_match ~loc { expr = matchee; cases = hd, tl })
      | _ -> same)
    | _ -> same
  in
  if Syntax_types.equal syntax JsLIGO
  then `Cata { idle_cata_pass with expr = pass_expr }
  else `Cata idle_cata_pass


let reduction ~raise ~syntax =
  let fail () = raise.error (wrong_reduction __MODULE__) in
  if Syntax_types.equal syntax JsLIGO
  then
    { Iter.defaults with
      expr =
        (function
        | { wrap_content = E_call (f, _); _ }
          when Option.value_map ~default:false (get_e_variable f) ~f:(fun x ->
                   Variable.is_name x "match") -> fail ()
        | _ -> ())
    }
  else Iter.defaults


let decompile ~syntax =
  ignore syntax;
  `Cata idle_cata_pass (* TODO ? mmh  *)


let pass ~raise ~syntax =
  morph
    ~name:__MODULE__
    ~compile:(compile ~raise ~syntax)
    ~decompile:(decompile ~syntax)
    ~reduction_check:(reduction ~raise ~syntax)
