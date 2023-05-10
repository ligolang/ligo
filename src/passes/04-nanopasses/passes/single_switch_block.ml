open Ast_unified
open Pass_type
open Simple_utils
open Unit_test_helpers
module Location = Simple_utils.Location

(* In case of a single switch in a block, remove the default case from the switch if it holds a return
   statement. This simplifies further reduction of switches
*)
include Flag.No_arg ()

let last_is_return (b : block option) =
  Option.value_map b ~default:false ~f:(fun b ->
      let stmt = get_b b in
      let is_return s =
        match get_s_instr s with
        | None -> false
        | Some i -> Option.is_some @@ get_i_return i
      in
      is_return (List.Ne.last stmt))


let compile ~raise:_ =
  let block : (block, statement) block_ -> block =
   fun b ->
    let loc = Location.get_location b in
    match Location.unwrap b with
    | one, [] ->
      let single_switch_opt =
        let open Option in
        let* instr = get_s_instr one in
        get_i_switch instr
      in
      (match single_switch_opt with
      | Some Switch.{ cases; switchee } ->
        let loc_sw = get_s_loc one in
        let cases = List.Ne.to_list cases in
        let default_opt =
          List.find_map cases ~f:(function
              | Switch_default_case x when last_is_return x -> x
              | _ -> None)
        in
        (match default_opt with
        | Some default_block ->
          let default_block = get_b default_block in
          let cases_no_default =
            List.filter cases ~f:(function
                | Switch_case _ -> true
                | Switch_default_case _ -> false)
          in
          (match List.Ne.of_list_opt cases_no_default with
          | None -> block_of_statements default_block
          | Some cases ->
            let switch =
              s_instr ~loc:loc_sw @@ i_switch ~loc:loc_sw { cases; switchee }
            in
            block_of_statements (List.Ne.cons switch default_block))
        | None -> make_b ~loc b.wrap_content)
      | None -> make_b ~loc b.wrap_content)
    | _ -> make_b ~loc b.wrap_content
  in
  Fold { idle_fold with block }


let name = __MODULE__
let decompile ~raise:_ = Nothing
let reduction ~raise:_ = Iter.defaults

let%expect_test _ =
  Block.(
    {|
      ((S_instr
        (I_switch
        ((switchee (E_variable n))
          (cases
          ((Switch_case (EXPR) ((BLOCK1)))
            (Switch_default_case (((STATEMENT1) (S_instr (I_return ())))))))))))
    |}
    |-> compile;
    [%expect
      {|
        ((S_instr
          (I_switch
           ((switchee (E_variable n)) (cases ((Switch_case (EXPR) ((BLOCK1))))))))
         (STATEMENT1) (S_instr (I_return ())))
      |}])
