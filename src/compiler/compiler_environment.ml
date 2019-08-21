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

let pack_closure : environment -> selector -> michelson result = fun e lst ->
  let%bind () = Assert.assert_true (e <> []) in

  (* Tag environment with selected elements. Only the first occurence
     of each name from the selector in the environment is kept. *)
  let e_lst =
    let e_lst = Environment.to_list e in
    let aux selector (s , _) =
      match List.mem s selector with
      | true -> List.remove_element s selector , true
      | false -> selector , false in
    let e_lst' = List.fold_map_right aux lst e_lst in
    let e_lst'' = List.combine e_lst e_lst' in
    e_lst''
  in

  let (_ , code) =
    let aux = fun (first , code) (_ , b) ->
      match b with
      | false -> (first , seq [dip code ; i_swap])
      | true -> (false ,
                 match first with
                 | true -> i_dup
                 | false -> seq [dip code ; i_dup ; dip i_pair ; i_swap]
                )
    in
    List.fold_right' aux (true , seq []) e_lst in

  ok code

let unpack_closure : environment -> michelson result = fun e ->
  let aux = fun code _ -> seq [ i_unpair ; dip code ] in
  ok (List.fold_right' aux (seq []) e)
