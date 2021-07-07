(* exceptions for meta and object languages *)

exception Exc of Ligo_interpreter.Types.exception_type

let throw_obj_exc loc =
  fun x ->
    let errs = List.map ~f:( fun e -> match e with `Tezos_alpha_error a -> a) x in
    raise (Exc (Object_lang_ex (loc, errs)))
