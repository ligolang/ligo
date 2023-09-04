open Ast_unified
open Pass_type
open Simple_utils
open Unit_test_helpers
module Location = Simple_utils.Location

(* In case of a single switch in a block, remove the default case from the switch if it holds a return
   statement. This simplifies further reduction of switches
*)
include Flag.No_arg ()

let last_is_return =
  Option.value_map ~default:false ~f:(fun b ->
      let stmt = get_b b in
      let is_return s =
        match get_s_instr s with
        | None -> false
        | Some i -> Option.is_some @@ get_i_return i
      in
      is_return (List.Ne.last stmt))


let block_of_default_case ~loc block_opt =
  let s_return = List.Ne.singleton @@ s_instr ~loc (i_return ~loc None) in
  Option.value_map block_opt ~default:(make_b ~loc s_return) ~f:(fun block ->
      make_b ~loc (List.Ne.append (get_b block) s_return))


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
      | Some { subject; cases = Switch.AllCases (cases, Some def) }
        when last_is_return def ->
        let sw =
          s_instr ~loc @@ i_switch ~loc { subject; cases = AllCases (cases, None) }
        in
        let def_block = block_of_default_case ~loc def in
        make_b ~loc (sw, List.Ne.to_list @@ get_b def_block)
      | Some { subject; cases = Switch.Default def } when last_is_return def ->
        block_of_default_case ~loc def
      | Some _ | None -> make_b ~loc b.wrap_content)
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
        ((subject (E_variable n))
          (cases
            (AllCases
              (((expr (EXPR)) (case_body (BLOCK1))))
              ((((STATEMENT1) (S_instr (I_return ())))))))))))
    |}
    |-> compile;
    [%expect
      {|
        ((S_instr
          (I_switch
           ((subject (E_variable n))
            (cases (AllCases (((expr (EXPR)) (case_body (BLOCK1)))) ())))))
         (STATEMENT1) (S_instr (I_return ())) (S_instr (I_return ())))
      |}])
