open Core
open Ast_unified
open Pass_type
open Errors
module Trace = Simple_utils.Trace
module Location = Simple_utils.Location
module Ligo_pair = Simple_utils.Ligo_pair
include Flag.No_arg ()

let compile ~raise:_ =
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    let op cons_name arguments = e_constant ~loc { cons_name; arguments } in
    let mk_list lst =
      let init = op C_LIST_EMPTY [] in
      List.fold_right lst ~f:(fun x acc -> op C_CONS [ x; acc ]) ~init
    in
    (* let mk_pair_list lst =
        let init = op C_LIST_EMPTY [] in
        List.fold
          lst
          ~f:(fun acc (k, v) ->
            let el = e_tuple ~loc (k, [ v ]) in
            op C_CONS [ el; acc ])
          ~init
      in *)
    let compare_expr_bindings = Ligo_pair.compare compare_expr compare_expr in
    match Location.unwrap e with
    | E_map kvlst ->
      (* REMITODO: this would be smarter.. but makes the tests fail..
          let lst = mk_pair_list kvlst in
          op C_MAP_LITERAL [ lst ] *)
      kvlst
      |> List.dedup_and_sort ~compare:compare_expr_bindings
      |> List.fold_right ~init:(op C_MAP_EMPTY []) ~f:(fun (k, v) acc ->
             op C_MAP_ADD [ k; v; acc ])
    | E_big_map kvlst ->
      (* let lst = mk_pair_list kvlst in
        op C_BIG_MAP_LITERAL [ lst ] *)
      kvlst
      |> List.dedup_and_sort ~compare:compare_expr_bindings
      |> List.fold_right ~init:(op C_BIG_MAP_EMPTY []) ~f:(fun (k, v) acc ->
             op C_MAP_ADD [ k; v; acc ])
    | E_list lst -> mk_list lst
    | E_set set ->
      set
      |> List.dedup_and_sort ~compare:compare_expr
      |> List.fold ~init:(op C_SET_EMPTY []) ~f:(fun acc el -> op C_SET_ADD [ el; acc ])
    | e -> make_e ~loc e
  in
  Fold { idle_fold with expr }


let reduction ~(raise : _ Trace.raise) =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_map _ | E_big_map _ | E_list _ | E_set _; _ } ->
        raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let name = __MODULE__
let decompile ~raise:_ = Nothing
