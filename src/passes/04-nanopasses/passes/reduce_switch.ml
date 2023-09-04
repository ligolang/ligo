open Ast_unified
open Pass_type
open Simple_utils.Trace
open Simple_utils.Function
open Simple_utils
open Errors
module Location = Simple_utils.Location

(* Reduce the switch instruction to a series of if statements *)
include Flag.No_arg ()

let name = __MODULE__

type group =
  { case_values : expr list
  ; body : statement list
  }

type default_case = statement list option

let loc_of_statement_list lst =
  List.fold ~init:Location.dummy ~f:Location.cover (List.map ~f:get_s_loc lst)


let split_break stmts : statement list * bool =
  let f x =
    match get_s x with
    | S_instr x -> (not <@ is_i_break) x
    | _ -> true
  in
  let bef, _ = List.split_while ~f stmts in
  let has_break =
    let folder =
      Catamorphism.
        { statement =
            (function
            | { wrap_content = S_instr x; _ } -> x
            | _ -> false)
        ; instruction =
            (function
            | { wrap_content = I_break; _ } -> true
            | { wrap_content = I_cond { ifso; ifnot; _ }; _ } ->
              let f = function
                | Test_clause.ClauseInstr x -> x
                | Test_clause.ClauseBlock x -> x
              in
              f ifso || Option.value_map ~default:false ~f ifnot
            | _ -> false)
        ; block =
            (function
            | { wrap_content; _ } -> List.exists (List.Ne.to_list wrap_content) ~f:Fun.id)
        ; expr = (fun _ -> ())
        ; ty_expr = (fun _ -> ())
        ; pattern = (fun _ -> ())
        ; mod_expr = (fun _ -> ())
        ; declaration = (fun _ -> ())
        ; program_entry = (fun _ -> ())
        ; program = (fun _ -> ())
        ; sig_expr = (fun _ -> ())
        ; sig_entry = (fun _ -> ())
        }
    in
    if List.is_empty stmts
    then false
    else
      Catamorphism.cata_block
        ~f:folder
        (make_b ~loc:Location.generated (List.Ne.of_list stmts))
  in
  bef, has_break


(*
  [group_cases] groups each consecutive group cases that are not followed by statements.
  the `default:` case is also returned if any.

  this function will throw if a default case is
  not in last position, but parser would not accept that
*)
let group_cases (cases : (expr, block) Switch.switch_case list) : group list =
  let open Switch in
  let groups =
    let break x y =
      match x.case_body, y.case_body with
      | None, None | None, Some _ -> false
      | Some _, _ -> true
    in
    List.group ~break cases
  in
  (* aggregate groups value and statement *)
  List.map groups ~f:(fun lst ->
      let rec agg (values, stmts) = function
        | { expr = value; case_body = None } :: tl -> agg (value :: values, stmts) tl
        | { expr = value; case_body = Some block } :: tl ->
          if List.is_empty tl
          then value :: values, List.Ne.to_list (get_b block)
          else assert false
        | [] -> values, stmts
      in
      let case_values, body = agg ([], []) lst in
      { case_values; body })


(* return all the statements before a break instruction, and append
  a true/false assignment on the ft (fallthrough) variable.
  if no break statement is present : `ft := true` otherwise `ft := false`
*)
let until_break ~loc ft block =
  let stmts_before_break, has_break = split_break block in
  let ft_assign =
    let rhs_expr = if has_break then e_false ~loc else e_true ~loc in
    s_instr ~loc (i_struct_assign ~loc { lhs_expr = ft; rhs_expr })
  in
  let break_witness = if has_break then [ s_instr ~loc (i_break ~loc) ] else [] in
  stmts_before_break @ [ ft_assign ] @ break_witness


let default_stmt ~loc = i_expr ~loc (e_unit ~loc)

let test_clause_of_statements ~loc stmts : _ Test_clause.t =
  match List.Ne.of_list_opt stmts with
  | Some (one, []) ->
    (match get_s_instr one with
    | Some i -> ClauseInstr i
    | None -> ClauseBlock (block_of_statements (one, [])))
  | Some lst -> ClauseBlock (block_of_statements lst)
  | None -> ClauseInstr (default_stmt ~loc)


let switch_to_decl loc Switch.{ subject; cases } =
  let cases, default_stmts_opt =
    match cases with
    | Switch.AllCases (cases, default) ->
      let default_stmts_opt =
        Option.value_map default ~default:None ~f:(fun x ->
            Some (Option.value_map x ~default:[] ~f:(List.Ne.to_list <@ get_b)))
      in
      List.Ne.to_list cases, default_stmts_opt
    | Switch.Default default ->
      let default_stmts_opt =
        Option.value_map default ~default:[] ~f:(List.Ne.to_list <@ get_b)
      in
      [], Some default_stmts_opt
  in
  let eq a b = e_constant ~loc { cons_name = C_EQ; arguments = [ a; b ] } in
  let or_ a b = e_constant ~loc { cons_name = C_OR; arguments = [ a; b ] } in
  let not_ a = e_constant ~loc { cons_name = C_NOT; arguments = [ a ] } in
  let cases = group_cases cases in
  let ft = Variable.fresh ~loc ~name:"fallthrough" () in
  let ft_var = e_variable ~loc ft in
  let ft_decl =
    (* declare fallthrough variable which decides if the control flow needs
       to be given to following case (it's conditionned to the presence of break)
    *)
    simpl_var_decl ~loc ft (e_false ~loc)
  in
  let grouped_switch_cases : (expr * statement list) list =
    List.mapi
      ~f:(fun i group ->
        let test_ = Variable.fresh ~loc ~name:("g" ^ string_of_int i) () in
        let test_var = e_variable ~loc test_ in
        let test_decl =
          (* build one matching condition for each groups:
            `subject == v1 || <...> || subject == vN || ft`
          *)
          let test =
            (* don't take fallthrough into account for the first case *)
            let init = if Int.equal i 0 then e_false ~loc else ft_var in
            List.fold group.case_values ~init ~f:(fun acc val_ ->
                or_ (eq subject val_) acc)
          in
          simpl_const_decl ~loc test_ test
        in
        (* generate one condition for each group:
          `if (ft || gN) then <statement> ; ft = <true/false>`
        *)
        let cond =
          let ifso =
            let loc = loc_of_statement_list group.body in
            test_clause_of_statements ~loc (until_break ~loc ft_var group.body)
          in
          s_instr ~loc (i_cond ~loc { test = test_var; ifso; ifnot = None })
        in
        test_var, [ test_decl; cond ])
      cases
  in
  let default_statement =
    (* if present, the default case is executed under the following condition:
      - always if there is no other case,
      - `ft || !(g1 || .. || gN)` otherwise
    *)
    Option.value_map default_stmts_opt ~default:[] ~f:(fun default_stmts ->
        let tests = List.map ~f:fst grouped_switch_cases in
        match tests with
        | [] -> until_break ~loc ft_var default_stmts
        | hd :: tl ->
          let ortests = List.fold tl ~init:hd ~f:(fun acc val_ -> or_ val_ acc) in
          let test = or_ ft_var (not_ ortests) in
          let ifso =
            let loc = loc_of_statement_list default_stmts in
            test_clause_of_statements ~loc (until_break ~loc ft_var default_stmts)
          in
          [ s_instr ~loc @@ i_cond ~loc { test; ifso; ifnot = None } ])
  in
  let case_statements =
    List.concat @@ ([ ft_decl ] :: List.map ~f:snd grouped_switch_cases)
  in
  case_statements @ default_statement


let reduce_clause : _ Test_clause.t -> _ Test_clause.t = function
  | ClauseBlock b -> ClauseBlock b
  | ClauseInstr i ->
    (match get_i_switch i with
    | Some sw ->
      ClauseBlock
        (block_of_statements @@ List.Ne.of_list @@ switch_to_decl (get_i_loc i) sw)
    | None -> ClauseInstr i)


let compile ~raise:_ =
  let block : _ block_ -> block =
   fun block ->
    let loc = Location.get_location block in
    let lst = Location.unwrap block in
    let expanded =
      List.Ne.map
        (fun stmt ->
          match get_s stmt with
          | S_instr i ->
            (match get_i i with
            | I_switch s -> switch_to_decl (get_i_loc i) s
            | I_cond ({ ifso; ifnot; _ } as cond) ->
              let stmt =
                s_instr ~loc
                @@ i_cond
                     ~loc:(get_i_loc i)
                     { cond with
                       ifso = reduce_clause ifso
                     ; ifnot = Option.map ~f:reduce_clause ifnot
                     }
              in
              [ stmt ]
            | I_case ({ cases; _ } as case) ->
              let cases =
                List.Ne.map (fun x -> Case.{ x with rhs = reduce_clause x.rhs }) cases
              in
              let stmt = s_instr ~loc @@ i_case ~loc:(get_i_loc i) { case with cases } in
              [ stmt ]
            | _ -> [ stmt ])
          | _ -> [ stmt ])
        lst
    in
    make_b ~loc (List.Ne.of_list (List.Ne.concat expanded))
  in
  Fold { idle_fold with block }


let reduction ~raise =
  { Iter.defaults with
    instruction =
      (function
      | { wrap_content = I_switch _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let decompile ~raise:_ = Nothing
