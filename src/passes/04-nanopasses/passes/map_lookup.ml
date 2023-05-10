open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location
include Flag.No_arg ()

let compile ~raise:_ =
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_map_lookup { map; keys } ->
      let last_proj, mk =
        List.fold
          (List.Ne.to_list keys)
          ~init:(map, Fun.id)
          ~f:(fun (last_proj, mk) key ->
            let matchee = e_map_find_opt ~loc key last_proj in
            let v_proj = Variable.fresh ~loc () in
            let some_proj = e_variable ~loc v_proj in
            some_proj, fun x -> mk @@ e_unopt ~loc matchee (e_none ~loc) (v_proj, x))
      in
      mk (e_some ~loc last_proj)
    | e -> make_e ~loc e
  in
  Fold { idle_fold with expr }


let reduction ~raise =
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_map_lookup _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let name = __MODULE__
let decompile ~raise:_ = Nothing
