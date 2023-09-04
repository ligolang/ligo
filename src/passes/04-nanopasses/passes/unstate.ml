open Ast_unified
open Pass_type
open Simple_utils.Trace
open Simple_utils
open Errors
module Location = Simple_utils.Location
include Flag.No_arg ()

(* Note 1: interesting, this pass is too verbose because we can't just morph blocks to expressions .. 
   which would only be possible if we had "real" nanopass (as in, one type for each intermediary AST)

  Note 2: the following is a translation of the old code in abstractors (modified to take AST unified as input)
  re-implementing at some point is necessary (see unsupported_control_flow)
*)

let name = __MODULE__

module Statement_result = struct
  (* a statement result is the temporary representation of a statement, awaiting to be morphed into an expression *)
  open S_exp

  type t =
    (* terminal statement *)
    | Return of expr
    (* continuation to elaborate bindings such as `let X = Y in <>` ; `let () = X in <>`
       from declarative statement *)
    | Binding of (expr -> expr)
    (* continuation to elaborate conditional expression such as `if T then X else Y` ; `case x with P1 -> X | P2 -> Y`
      from control flow statements.
      It operates on a statement result so that each statement in the original branches can be merged
      within the client logic : sometimes one branch is terminal 

      those 2 statements:
      ```
      if (T) { return } else { x += 1 } ;
      return
      ```

      will result in 2 statement results:
      ```
      [ Control_flow (<hole> -> if (T) then <merge Return <hole>> else <merge (Binding ..) <hole> ;
      ; Return
      ]
      ```
      since the merge happens within the control flow continuation, it allowed the positive branch to ignore any
      in incoming results (`<merge Return <hole>>` is always `Return`)
      
      if the control_flow was operating on an expression, it would be hard to decide if an expression is terminal
    *)
    | Control_flow of (t -> expr)
  [@@deriving sexp]

  (* merge two consecutive statement result *)
  let rec merge : t -> t -> t =
   fun bef aft ->
    let open Simple_utils.Function in
    match bef, aft with
    | Binding a, Binding b -> Binding (a <@ b)
    | Binding a, Return b -> Return (a b)
    | Binding a, Control_flow f -> Control_flow (fun t -> a @@ f t)
    | Control_flow f, x -> Control_flow (fun t -> f (merge x t))
    | Return a, _ -> Return a


  let merge_block : t List.Ne.t -> t = fun (hd, tl) -> List.fold ~f:merge ~init:hd tl

  (* morph a statement_result into an expression *)
  let to_expression : t -> expr =
   fun statement_result ->
    match statement_result with
    | Binding b -> b (e_unit ~loc:Location.generated)
    | Return r -> r
    | Control_flow f -> f (Return (e_unit ~loc:Location.generated))


  (* merge one flow branch *)
  let merge_flow (x : t) (hole : t) =
    to_expression
    @@
    match x with
    | Return _ -> x
    | _ -> merge x hole


  let is_not_returning (x : t) =
    match x with
    | Binding _ -> true
    | Control_flow _ -> false
    | Return e ->
      (match get_e_literal e with
      | Some Literal_unit -> true
      | _ -> false)
end

(* temprorary until pass 'expand_polymorphism' is written and E_fun added *)
let e_fun ~loc x = e_poly_fun ~loc x

let rec decl : declaration -> Statement_result.t =
 fun d ->
  let loc = get_d_loc d in
  match get_d d with
  | D_directive _ -> Binding Fun.id
  | D_attr (attr, d) ->
    let d = decl d in
    Statement_result.merge (Binding (fun x -> e_attr ~loc:(get_e_loc x) (attr, x))) d
  | D_import (Import_rename { alias; module_path }) ->
    Binding
      (fun x ->
        e_mod_in
          ~loc:(get_e_loc x)
          { module_name = alias; rhs = m_path ~loc module_path; body = x })
  | D_export d ->
    (* weird .. *)
    decl (d_attr ~loc (Attribute.{ key = "public"; value = None }, d))
  | D_var { type_params; pattern; rhs_type; let_rhs } ->
    Binding
      (fun x ->
        e_let_mut_in
          ~loc
          { is_rec = false
          ; type_params
          ; lhs = List.Ne.singleton pattern
          ; rhs_type
          ; rhs = let_rhs
          ; body = x
          })
  | D_const { type_params; pattern; rhs_type; let_rhs } ->
    Binding
      (fun x ->
        e_let_in
          ~loc
          { is_rec = false
          ; type_params
          ; lhs = List.Ne.singleton pattern
          ; rhs_type
          ; rhs = let_rhs
          ; body = x
          })
  | D_let { is_rec; type_params; pattern; rhs_type; let_rhs } ->
    Binding
      (fun x ->
        e_let_in
          ~loc
          { is_rec; type_params; lhs = pattern; rhs_type; rhs = let_rhs; body = x })
  | D_fun { is_rec; fun_name; type_params; parameters; ret_type; return } ->
    Binding
      (fun x ->
        e_let_in
          ~loc
          { is_rec
          ; type_params
          ; lhs = List.Ne.singleton (p_var ~loc:(Variable.get_location fun_name) fun_name)
          ; rhs_type = None
          ; rhs = e_fun ~loc { type_params; parameters; ret_type; body = return }
          ; body = x
          })
  | D_module { name; mod_expr; annotation = _TODO } ->
    Binding (fun x -> e_mod_in ~loc { module_name = name; rhs = mod_expr; body = x })
  | D_signature { name = _; sig_expr = _ } -> Binding Fun.id
  | D_type { name; type_expr } ->
    Binding
      (fun x ->
        e_type_in ~loc { type_decl = { name; type_expr; params = None }; body = x })
  | D_import (Import_all_as _ | Import_selected _) | D_module_include _ ->
    failwith "can't parse"
  | D_multi_var _ | D_multi_const _ | D_type_abstraction _ ->
    failwith "removed in previous passes"
  | D_irrefutable_match { pattern; expr } ->
    let lhs = List.Ne.singleton pattern in
    Binding
      (fun x ->
        e_let_in
          ~loc
          { is_rec = false
          ; type_params = None
          ; lhs
          ; rhs_type = None
          ; rhs = expr
          ; body = x
          })


and clause ~raise clause =
  let open Test_clause in
  match clause with
  | ClauseInstr instruction -> instr ~raise instruction
  | ClauseBlock block ->
    let block = get_b block in
    Statement_result.merge_block List.Ne.(map (statement ~raise) block)


and instr ~raise : instruction -> Statement_result.t =
 fun i ->
  let loc = get_i_loc i in
  match get_i i with
  | I_return expr_opt ->
    Return (Option.value expr_opt ~default:(e_unit ~loc:Location.generated))
  | I_block block' ->
    let block = get_b block' in
    (match Statement_result.merge_block (List.Ne.map (statement ~raise) block) with
    | Binding f ->
      Binding (fun hole -> let_ignore_in (f (e_unit ~loc:Location.generated)) hole)
    | Return _ as res -> res
    | Control_flow _ ->
      raise.error (unsupported_control_flow block')
      (* AS in :
      ```jsligo  
      const g = (n:int) => {
        let output = n;
        
        {
          let x = 1 ;
          output += x ;
          if (n > 1) {
            return (output + 12)
          } else {
            output += x;
          }
        }
        
        return output + x // x should not be visible here
      }
      ```
      *))
  | I_skip -> Binding Fun.id
  | I_call (f, args) -> Binding (fun x -> let_unit_in (e_call ~loc f args) x)
  | I_case { expr; cases } ->
    if List.for_all (List.Ne.to_list cases) ~f:(fun x ->
           Statement_result.is_not_returning (clause ~raise x.rhs))
    then (
      let match_ =
        let cases =
          List.Ne.map
            (fun Case.{ pattern; rhs } ->
              let rhs = Statement_result.to_expression (clause ~raise rhs) in
              Case.{ pattern; rhs })
            cases
        in
        e_match ~loc { expr; cases }
      in
      Binding (fun hole -> let_unit_in match_ hole))
    else
      Control_flow
        (fun hole ->
          let cases =
            List.Ne.map
              (fun Case.{ pattern; rhs } ->
                let rhs = Statement_result.merge_flow (clause ~raise rhs) hole in
                Case.{ pattern; rhs })
              cases
          in
          e_match ~loc { expr; cases })
  | I_cond { test; ifso; ifnot } ->
    let ifso_res = clause ~raise ifso in
    let ifnot_res_opt = Option.map ~f:(clause ~raise) ifnot in
    (match ifso_res, ifnot_res_opt with
    (* optim cases *)
    | x, None when Statement_result.(is_not_returning x) ->
      let cond =
        e_cond ~loc { test; ifso = Statement_result.to_expression ifso_res; ifnot = None }
      in
      Binding (fun hole -> let_unit_in cond hole)
    | x, Some ifnot_res
      when Statement_result.(is_not_returning x && is_not_returning ifnot_res) ->
      let cond =
        e_cond
          ~loc
          { test
          ; ifso = Statement_result.to_expression ifso_res
          ; ifnot = Some (Statement_result.to_expression ifnot_res)
          }
      in
      Binding (fun hole -> let_unit_in cond hole)
    (* normal case *)
    | _ ->
      Control_flow
        (fun hole ->
          let ifnot_res = Option.value ifnot_res_opt ~default:hole in
          e_cond
            ~loc
            { test
            ; ifso = Statement_result.merge_flow ifso_res hole
            ; ifnot = Some (Statement_result.merge_flow ifnot_res hole)
            }))
  | I_assign (v, e) ->
    Binding
      (fun hole ->
        let_unit_in
          (e_assign_unitary
             ~loc
             { binder = Ligo_prim.Binder.make v None; expression = e })
          hole)
  | I_expr ({ fp = { wrap_content = E_assign_chainable _ | E_simple_let_in _; _ } } as e)
    -> Binding (fun hole -> let_ignore_in e hole)
  | I_expr e ->
    Binding (fun hole -> if Combinators.is_e_unit hole then e else let_unit_in e hole)
  | I_for for_ ->
    let for_ = For_int.map Fun.id (block_to_expression ~raise) for_ in
    Binding (fun hole -> let_unit_in (e_for ~loc for_) hole)
  | I_for_in for_ ->
    let for_ = For_collection.map Fun.id Fun.id (block_to_expression ~raise) for_ in
    Binding (fun hole -> let_unit_in (e_for_in ~loc for_) hole)
  | I_while w ->
    let w = While.map Fun.id (block_to_expression ~raise) w in
    Binding (fun hole -> let_unit_in (e_while ~loc w) hole)
  | I_break -> Binding (fun hole -> let_unit_in (e_unit ~loc:Location.generated) hole)
  | I_continue -> raise.error (unsupported_continue loc)
  | I_struct_assign _ | I_remove _ | I_patch _ | I_switch _ | I_for_of _ | I_for_stmt _ ->
    failwith "removed"


and block_to_expression ~raise block =
  Statement_result.(
    to_expression @@ merge_block (List.Ne.map (statement ~raise) (get_b block)))


and statement ~raise : statement -> Statement_result.t =
 fun s ->
  match get_s s with
  | S_attr (attr, x) ->
    let s = statement ~raise x in
    Statement_result.merge (Binding (fun x -> e_attr ~loc:(get_e_loc x) (attr, x))) s
  | S_export d -> decl d (* export is ignored, here it should not happen? *)
  | S_instr i -> instr ~raise i
  | S_decl d -> decl d
  | S_directive () -> Binding (fun x -> x)


let compile ~raise =
  let expr : (_, _, _, _, _) expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_match_block m ->
      let m = Case.map Fun.id Fun.id (block_to_expression ~raise) m in
      e_match ~loc m
    | E_block_poly_fun ({ body; _ } as block_fun) ->
      let body = block_to_expression ~raise body in
      e_poly_fun ~loc { block_fun with body }
    | E_block_with { block; expr } ->
      let block_with_res =
        let open List.Ne in
        let block_res = map (statement ~raise) (get_b block) in
        append block_res (singleton @@ Statement_result.Return expr)
      in
      let res = Statement_result.(to_expression (merge_block block_with_res)) in
      { fp = { res.fp with location = loc } }
    | E_do block ->
      let res = block_to_expression ~raise block in
      { fp = { res.fp with location = loc } }
    | e -> make_e ~loc e
  in
  Fold { idle_fold with expr }


let reduction ~raise =
  let fail () = raise.error (wrong_reduction __MODULE__) in
  { Iter.defaults with
    instruction = (fun _ -> fail ())
  ; statement = (fun _ -> fail ())
  ; block = (fun _ -> fail ())
  ; expr =
      (function
      | { wrap_content = E_match_block _; _ }
      | { wrap_content = E_block_poly_fun _; _ }
      | { wrap_content = E_block_with _; _ } -> fail ()
      | _ -> ())
  }


let decompile ~raise:_ = Nothing
