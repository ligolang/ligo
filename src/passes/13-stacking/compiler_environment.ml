open Errors
open Trace
open Mini_c
open Environment
open Michelson

let empty : environment = []

let get : environment -> expression_variable -> (michelson, stacking_error) result = fun e s ->
  let%bind (_ , position) =
    generic_try (get_env s e) @@
    (fun () -> Environment.get_i s e) in
  let aux_dig = fun n -> seq [
      i_dig n ;
      i_dup ;
      i_dug (n + 1) ;
    ]
  in
  let code =
    if position < 1
    then i_dup
    else aux_dig position in

  ok code

let pack_closure : environment -> selector -> (michelson, stacking_error) result = fun e lst ->
  let%bind () = Assert.assert_true (corner_case ~loc:__LOC__ "pack closure") (e <> []) in

  (* Tag environment with selected elements. Only the first occurence
     of each name from the selector in the environment is kept. *)
  let e_lst =
    let e_lst = Environment.to_list e in
    let aux selector (s , _) =
      let var_compare = fun (a:var_name) (b:var_name) -> Var.compare a.wrap_content b.wrap_content in
      match List.mem ~compare:var_compare s selector with
      | true -> List.remove_element ~compare:var_compare s selector , true
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

let unpack_closure : environment -> (michelson , stacking_error) result = fun e ->
  match e with
  | [] -> ok @@ seq []
  | _ :: tl -> (
      let aux = fun code _ -> seq [ i_unpair ; dip code ] in
      let unpairs = (List.fold_right' aux (seq []) tl) in
      ok @@ seq [ i_unpiar ; dip unpairs ]
    )
  (* let aux = fun code _ -> seq [ i_unpair ; dip code ] in
   * ok (List.fold_right' aux (seq []) e) *)
