open Trace
open Mini_c
open Environment
open Michelson

module Stack = Meta_michelson.Stack

let get : environment -> string -> michelson result = fun e s ->
  let%bind (_ , position) =
    let error =
      let title () = "Environment.get" in
      let content () = Format.asprintf "%s in %a"
          s PP.environment e in
      error title content in
    generic_try error @@
    (fun () -> Environment.get_i s e) in
  let rec aux = fun n ->
    match n with
    | 0 -> i_dup
    | n -> seq [
        dip @@ aux (n - 1) ;
        i_swap ;
      ]
  in
  let code = aux position in

  ok code

let set : environment -> string -> michelson result = fun e s ->
  let%bind (_ , position) =
    generic_try (simple_error "Environment.get") @@
    (fun () -> Environment.get_i s e) in
  let rec aux = fun n ->
    match n with
    | 0 -> dip i_drop
    | n -> seq [
        i_swap ;
        dip (aux (n - 1)) ;
      ]
  in
  let code = aux position in

  ok code
