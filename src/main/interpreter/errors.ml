type interpreter_error = Main_errors.all

let target_lang_error : Location.t -> Tezos_error_monad__TzCore.error list -> interpreter_error =
  fun loc e -> `Main_interpret_target_lang_error (loc,e)

let meta_lang_eval : Location.t -> string -> interpreter_error =
  fun loc s -> `Main_interpret_meta_lang_eval (loc,s)

let meta_lang_failwith : Location.t -> Ligo_interpreter.Types.value -> interpreter_error =
  fun loc v -> `Main_interpret_meta_lang_failwith (loc,v)

let test_entry_not_found : string -> interpreter_error = fun s ->
  `Main_interpret_test_entry_not_found s

let bootstrap_not_enough : Location.t -> interpreter_error = fun l ->
  `Main_interpret_boostrap_not_enough l

let generic_error : Location.t -> string -> interpreter_error = fun loc desc ->
  `Main_interpret_generic (loc,desc)

let literal : Location.t -> Ast_typed.literal -> interpreter_error = fun s l ->
  `Main_interpret_literal (s, l)

let modules_not_supported : Location.t -> interpreter_error = fun l ->
  `Main_interpret_modules_not_supported l
