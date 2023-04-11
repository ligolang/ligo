open Ast_unified
open Pass_type
open Simple_utils
module Location = Simple_utils.Location

(* split default case from other cases in case of a single switch in a block *)

let last_is_return (b : block option) =
  Option.value_map b ~default:false ~f:(fun b ->
      let stmt = get_b b in
      let is_return s =
        match get_s_instr s with
        | None -> false
        | Some i -> Option.is_some @@ get_i_return i
      in
      is_return (List.Ne.last stmt))


let compile =
  let block : (block, statement) block_ -> block =
   fun b ->
    let loc = Location.get_location b in
    match Location.unwrap b with
    | ( { fp =
            { wrap_content =
                S_instr
                  { fp =
                      { wrap_content = I_switch { cases; switchee }; location = loc_sw }
                  }
            ; _
            }
        }
      , [] ) ->
      let cases = List.Ne.to_list cases in
      let default_opt =
        List.find_map cases ~f:(function
            | Switch_default_case x when last_is_return x -> Some x
            | _ -> None)
      in
      (match default_opt with
      | None | Some None -> make_b ~loc b.wrap_content
      | Some (Some default_block) ->
        let default_block = get_b default_block in
        let cases_no_default =
          List.filter cases ~f:(function
              | Switch_case _ -> true
              | Switch_default_case _ -> false)
        in
        (match List.Ne.of_list_opt cases_no_default with
        | None -> block_of_statements default_block
        | Some cases ->
          let switch = s_instr ~loc:loc_sw @@ i_switch ~loc:loc_sw { cases; switchee } in
          block_of_statements (List.Ne.cons switch default_block)))
    | _ -> make_b ~loc b.wrap_content
  in
  `Cata { idle_cata_pass with block }


let pass = morph ~name:__MODULE__ ~compile ~decompile:`None ~reduction_check:Iter.defaults
