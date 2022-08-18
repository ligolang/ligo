module Location = Simple_utils.Location
type interpreter_error = Main_errors.all

let target_lang_failwith : Location.t -> (int, string) Tezos_micheline.Micheline.node -> interpreter_error =
  fun loc e -> `Main_interpret_target_lang_failwith (loc,e)

let target_lang_error : Location.t -> Ligo_interpreter.Types.calltrace -> Tezos_error_monad__TzCore.error list -> interpreter_error =
  fun loc calltrace e -> `Main_interpret_target_lang_error (loc,calltrace,e)

let meta_lang_eval : Location.t -> Ligo_interpreter.Types.calltrace -> Ligo_interpreter.Types.value -> interpreter_error =
  fun loc calltrace v -> `Main_interpret_meta_lang_eval (loc,calltrace,v)

let meta_lang_failwith : Location.t -> Ligo_interpreter.Types.calltrace -> Ligo_interpreter.Types.value -> interpreter_error =
  fun loc calltrace v -> `Main_interpret_meta_lang_failwith (loc,calltrace,v)

let bootstrap_not_enough : Location.t -> interpreter_error = fun l ->
  `Main_interpret_boostrap_not_enough l

let generic_error ?(calltrace = []) : Location.t -> string -> interpreter_error = fun loc desc ->
  let is_dummy_location loc =
    Location.is_dummy_or_generated loc ||
      match Location.get_file loc with
      | Some r -> String.equal r#file ""
      | None -> true in
  let locs = List.filter ~f:(fun l -> not (is_dummy_location l)) @@ loc :: calltrace in
  let loc = if List.is_empty locs then loc else List.hd_exn locs in
  `Main_interpret_generic (loc,desc)

let not_enough_initial_accounts : Location.t -> Memory_proto_alpha.Protocol.Alpha_context.Tez.tez -> interpreter_error = fun loc max ->
  `Main_interpret_not_enough_initial_accounts (loc,max)

let literal : Location.t -> Stage_common.Literal_value.t -> interpreter_error = fun s l ->
  `Main_interpret_literal (s, l)

let corner_case ?(loc = Location.generated) () = generic_error loc "Corner case, please report to devs."
