open Simple_utils.Display

type interpreter_error = [
  | `Ligo_interpret_contract_not_found of Location.t
  | `Ligo_interpret_constant_not_supported of Location.t * Ast_typed.constant'
  | `Ligo_interpret_test_entry_not_found of string
  | `Ligo_interpret_bad_test of Ligo_interpreter.Types.value * string
  | `Ligo_interpret_dummy
  | `Ligo_interpret_failwith of string
]

let contract_not_found_extern : Location.t -> interpreter_error =
  fun loc -> `Ligo_interpret_contract_not_found loc
let dummy : _ -> interpreter_error = fun _ -> `Ligo_interpret_dummy
let failwith : string -> interpreter_error = fun s -> `Ligo_interpret_failwith s
let constant_not_supported : Location.t -> Ast_typed.constant' -> interpreter_error =
  fun loc c -> `Ligo_interpret_constant_not_supported (loc,c)
let test_entry_not_found : string -> interpreter_error = fun s ->
  `Ligo_interpret_test_entry_not_found s
let bad_test : Ligo_interpreter.Types.value -> string -> interpreter_error = fun v s ->
  `Ligo_interpret_bad_test (v,s)


let error_ppformat : display_format:string display_format ->
  Format.formatter -> interpreter_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Ligo_interpret_contract_not_found loc ->
      Format.fprintf f
        "@[<hv>%a@.Contract not found in the current context@]"
        Location.pp loc
    | `Ligo_interpret_constant_not_supported (loc, cst) ->
      Format.fprintf f "@[<hv>%a@.LIGO interpreter does not support constant %a yet@]"
      Location.pp loc
      Ast_typed.PP.constant' cst
    | `Ligo_interpret_test_entry_not_found s ->
      Format.fprintf f "Test entry '%s' not found" s
    | `Ligo_interpret_bad_test (v,s) ->
      Format.fprintf f "Evaluation of '%s' returned %a, but a boolean result was expected"
        s Ligo_interpreter.PP.pp_value v
    | `Ligo_interpret_dummy -> ()
    | `Ligo_interpret_failwith s ->
      Format.fprintf f "@[<hv>Evaluation failed with:@.%s@]" s
  )

(**
  `contract_failure` must be used for contract failure only,
  other exeptions are used for errors which should be caught
  by the earlier passes
**)
let contract_failure : string -> _ = fun s ->
  raise (Ligo_interpreter.Types.Temporary_hack s)

let contract_not_found : string -> _ = fun addr -> 
  contract_failure @@
    Format.asprintf
    "Could not find address %s in the context" addr

let trace_alpha_contract_failure : string -> 'a Proto_alpha_utils.Trace.AE.Error_monad.tzresult -> 'a = fun s f ->
  let v  = Trace.to_option @@ Proto_alpha_utils.Trace.trace_alpha_tzresult dummy f in
  match v with Some x -> x | None -> contract_failure s 