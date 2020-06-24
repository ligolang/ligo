open Trace
open Types

let get_binder_name : 'a Var.t -> string = fun (v: _ Var.t) ->
  try Var.to_name v with _ -> "generated"

let make_def_id name i =
  (name ^ (string_of_int i), i+1)

let add_shadowing_def : (int * _ Var.t) -> def -> def_map -> (int * def_map) =  fun (i,var) def env -> 
  let name = get_binder_name var in
  let (definition_id,i) = make_def_id name i in
  let shadow = Def_map.filter
    (fun _ s_def -> match def, s_def with
      | Variable _ , Variable _ | Type _ , Type _ ->
        not @@ String.equal (get_def_name s_def) name
      | _ -> true )
    env in
  let env = Def_map.add definition_id def shadow in
  (i,env)

let make_v_def_from_core : string -> string -> string -> Ast_core.expression -> Location.t -> Location.t -> def =
  fun source_file syntax name exp range body_range ->
    let t = to_option @@
      let%bind typed_prg,state = Compile.Utils.type_file source_file syntax Env in
      let env = Ast_typed.program_environment Environment.default typed_prg in
      let%bind (e,_) = Compile.Of_core.compile_expression ~env ~state exp in
      ok e.type_expression
    in
      (* TODO : the source_file is given here but it should only be the declarations seen so far,
                otherwise nothing will be typed if an error occurs later in the file *)
    make_v_def name t range body_range

let make_v_def_option_type : string -> string -> string -> Ast_core.type_expression option -> Location.t -> Location.t -> def =
  fun source_file syntax name maybe_typed range body_range ->
    match maybe_typed with
    | Some t ->
      let t' = to_option @@
        let%bind typed_prg,_ = Compile.Utils.type_file source_file syntax Env in
        let env = Ast_typed.program_environment Environment.default typed_prg in
        Compile.Of_core.evaluate_type env t in
      make_v_def name t' range body_range
    | None -> make_v_def name None range body_range

let make_v_def_ppx_type : 
  string -> string -> string -> (Ast_typed.type_expression -> Ast_typed.type_expression) ->
  Ast_core.expression -> Location.t -> Location.t -> def =
  fun source_file syntax name f exp range body_range ->
    let t = to_option @@
      let%bind typed_prg,state = Compile.Utils.type_file source_file syntax Env in
      let env = Ast_typed.program_environment Environment.default typed_prg in
      let%bind (e,_) = Compile.Of_core.compile_expression ~env ~state exp in
      let v = f e.type_expression in ok v
    in
    make_v_def name t range body_range