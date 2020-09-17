open Trace
open Types

let get_binder_name : 'a Var.t -> string = fun (v: _ Var.t) ->
  if Var.is_generated v
  then "generated"
  else Var.to_name v

let make_def_id name i =
  (name ^ "#" ^ (string_of_int i), i+1)

let add_shadowing_def : (int * _ Var.t) -> def -> def_map -> (int * def_map) =  fun (i,var) def env -> 
  if Var.is_generated var then (i,env)
  else
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

let make_v_def_from_core : with_types:bool -> string -> string -> ('a Var.t) -> Ast_core.expression -> Location.t -> Location.t -> def =
  fun ~with_types source_file syntax name exp range body_range ->
    let name = get_binder_name name in
    let t = to_option @@
      let%bind typed_prg,state = Compile.Utils.type_file source_file syntax Env in
      let env = Ast_typed.program_environment Environment.default typed_prg in
      let%bind (e,_) = Compile.Of_core.compile_expression ~env ~state exp in
      ok e.type_expression
    in
      (* TODO : the source_file is given here but it should only be the declarations seen so far,
                otherwise nothing will be typed if an error occurs later in the file *)
    make_v_def ~with_types name t range body_range

let make_v_def_option_type : with_types:bool -> string -> string -> ('a Var.t) -> Ast_core.type_expression -> Location.t -> Location.t -> def =
  fun ~with_types source_file syntax name maybe_typed range body_range ->
    let name = get_binder_name name in
    match maybe_typed.type_content with
    | T_wildcard -> make_v_def ~with_types name None range body_range
    | _ ->
      let t' = to_option @@
        let%bind typed_prg,_ = Compile.Utils.type_file source_file syntax Env in
        let env = Ast_typed.program_environment Environment.default typed_prg in
        Compile.Of_core.evaluate_type env maybe_typed in
      make_v_def ~with_types name t' range body_range

let make_v_def_ppx_type : 
  with_types:bool -> string -> string -> ('a Var.t) -> (Ast_typed.type_expression -> Ast_typed.type_expression) ->
  Ast_core.expression -> Location.t -> Location.t -> def =
  fun ~with_types source_file syntax name f exp range body_range ->
    let name = get_binder_name name in
    let t = to_option @@
      let%bind typed_prg,state = Compile.Utils.type_file source_file syntax Env in
      let env = Ast_typed.program_environment Environment.default typed_prg in
      let%bind (e,_) = Compile.Of_core.compile_expression ~env ~state exp in
      let v = f e.type_expression in ok v
    in
    make_v_def ~with_types name t range body_range
