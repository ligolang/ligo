(* exceptions for meta and object languages *)

exception Object_lang_ex of Location.t * Tezos_error_monad.TzCore.error list

type fail_reason = Val of Ligo_interpreter.Types.value | Reason of string

exception Meta_lang_ex of { location : Location.t ; reason : fail_reason }

let throw_obj_exc loc =
  fun x ->
    let errs = List.map ~f:( fun e -> match e with `Tezos_alpha_error a -> a) x in
    raise (Object_lang_ex (loc, errs))