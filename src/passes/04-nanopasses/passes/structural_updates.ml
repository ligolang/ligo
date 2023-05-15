open Ast_unified
open Pass_type
open Simple_utils.Trace
open Simple_utils
open Errors
module Location = Simple_utils.Location

(*
  this pass morph "structural assignments" where lhs is an expression,
  to simple assigments where lhs is a variable.
  
  `m.["foo"].y := baz`
  |->
  ```
  m := (match MAP_FIND_OPT m "foo" with | Some r -> MAP_ADD "foo" {r with x = baz} m | None -> m
  ```
*)
let name = __MODULE__

include Flag.No_arg ()

let label_of_access =
  let open Selection in
  function
  | FieldName l -> l
  | Component_num (l, _) -> Label.of_string l
  | Component_expr _ -> failwith "raise.error unsupported map access ?"


(*
    `path_of_lvalue [lvalue]` extracts the path and the left-end-side out of an expression to be used in an assignment.
    The path is extracted as an access list, and can be used to construct the right side of the assigment
      - ((r.x).y).z         |-> (r, [x;y;z])
      - (m.["foo"]).["bar"] |-> (m, ["foo";"bar"])
  
    Restrictions on [lvalue]:
      - the left-most accessed element must be a variable (e.g. `{ x = 1 ; y = 2}.x = 2` is rejected)
      - module access are forbidden (we do not support effects on module declarations)
      - any expression that is not a record/map/variable access is rejected
  *)
let path_of_lvalue ~raise : expr -> Variable.t * expr Selection.t list =
 fun expr ->
  let rec aux (lhs : expr) (cpath : expr Selection.t list) =
    match get_e lhs with
    | E_variable v -> v, cpath
    | E_proj { struct_; path } -> aux struct_ (path :: cpath)
    | E_map_lookup { map; keys } ->
      let sels =
        List.map ~f:(fun e -> Selection.Component_expr e) (List.Ne.to_list keys)
      in
      aux map (sels @ cpath)
    | E_module_open_in _ -> (* maybe in future ? *) raise.error (wrong_lvalue expr)
    | _ -> raise.error (wrong_lvalue expr)
  in
  aux expr []


(*
    `compile_assignment [~loc] [~last_proj_update] [~lhs] [~path] [~default_rhs]` build the assignment of [lhs] accessed by [path].
  
    This function is used in case of patches (`patch <X> with <Y>`) ; assignments (`<X> := <Y>`) or removals (`remove <X> from <Y>`).
  
    The produced assignment will only update [lhs] if all the accessed map element in [path] are already present, i.e. we match
    on all the elements in [path] : `match Map.find_opt .. with | None -> <lhs> -> Some -> ...`.
  
    [default_rhs] is used as a default assigned value when path is empty, if the path isn't empty [last_proj_update] will be used as follow:
      - Internally, compile_assignment accumulate a "context" (updator: expression -> expression) building the whole access expression on
        the right-end side while processing [path]
      - when all elements of path have been processed, [last_proj_update] is executed on top of `updator` to build the final structure
      ```
        v := match MAP_FIND_OPT ("x",v) with | Some <last_accessed_element> -> <last_proj_update last_accessed_element> | None -> v
      ```
  *)
let build_update ~loc ~last_proj_update ~init lhs path =
  let rec aux : expr * (expr -> expr) -> expr Selection.t list -> expr =
   fun (last_proj, updator) lst ->
    (* [last_proj] is an accessor to the projection in [path] (i.e. [lhs].path(0).path(1)...path(n) *)
    match lst with
    | [] -> updator (last_proj_update last_proj)
    | access :: tl ->
      (match access with
      | FieldName _ | Component_num _ ->
        let label = label_of_access access in
        let updator hole =
          updator (e_record_update ~loc { struct_ = last_proj; label; update = hole })
        in
        let prev_access = e_record_access ~loc { struct_ = last_proj; label } in
        aux (prev_access, updator) tl
      | Component_expr k ->
        let matchee = e_map_find_opt ~loc k last_proj in
        let none_body = e_variable ~loc lhs in
        (* TOOD: could be failwith ? *)
        let some_proj = Variable.fresh ~loc () in
        let some_body =
          let updator hole = updator (e_map_add ~loc k hole last_proj) in
          let last_proj' = e_variable ~loc some_proj in
          aux (last_proj', updator) tl
        in
        e_unopt ~loc matchee none_body (some_proj, some_body))
  in
  aux (init, Fun.id) path


let compile_assignment_rhs
    :  loc:Location.t -> last_proj_update:(expr -> expr) -> lhs:Variable.t
    -> path:expr Selection.t list -> default_rhs:expr -> expr
  =
 fun ~loc ~last_proj_update ~lhs ~path ~default_rhs ->
  if List.is_empty path
  then default_rhs
  else (
    let init = e_variable ~loc lhs in
    build_update ~loc ~last_proj_update ~init lhs path)


let compile_assignment ~raise ~loc : expr -> expr -> Variable.t * expr =
 fun lhs_expr rhs_expr ->
  let var, path = path_of_lvalue ~raise lhs_expr in
  match List.rev path with
  | [] -> var, rhs_expr
  | last_access :: path ->
    let default_rhs, last_proj_update =
      match last_access with
      | Component_expr k ->
        let default_rhs = e_map_add ~loc k rhs_expr (e_variable ~loc var) in
        let last_proj_update last_proj = e_map_add ~loc k rhs_expr last_proj in
        default_rhs, last_proj_update
      | (Component_num _ | FieldName _) as access ->
        let default_rhs =
          e_record_update
            ~loc
            { struct_ = e_variable ~loc var
            ; label = label_of_access access
            ; update = rhs_expr
            }
        in
        let last_proj_update last_proj =
          e_record_update
            ~loc
            { struct_ = last_proj; label = label_of_access access; update = rhs_expr }
        in
        default_rhs, last_proj_update
    in
    let path = List.rev path in
    let rhs = compile_assignment_rhs ~loc ~last_proj_update ~lhs:var ~path ~default_rhs in
    var, rhs


let compile ~raise =
  let instruction : _ instruction_ -> instruction =
   fun i ->
    let loc = Location.get_location i in
    match Location.unwrap i with
    | I_struct_assign { lhs_expr; rhs_expr } ->
      let var, rhs = compile_assignment ~raise ~loc lhs_expr rhs_expr in
      i_assign ~loc var rhs
    | I_remove { item_expr; remove_kind; collection } ->
      let v, path = path_of_lvalue ~raise collection in
      let remove_func =
        match remove_kind with
        | `Set -> e_set_remove ~loc
        | `Map -> e_map_remove ~loc
      in
      let default_rhs = remove_func item_expr (e_variable ~loc v) in
      let last_proj_update prev_proj = remove_func item_expr prev_proj in
      let rhs = compile_assignment_rhs ~loc ~last_proj_update ~lhs:v ~path ~default_rhs in
      i_assign ~loc v rhs
    | I_patch { collection; patch_kind; patch } ->
      let v, path = path_of_lvalue ~raise collection in
      let last_proj_update, default_rhs =
        match get_e patch, patch_kind with
        | E_map kvl, `Map ->
          let f acc (k, v) = e_map_add ~loc k v acc in
          ( (fun last_proj -> List.fold kvl ~f ~init:last_proj)
          , List.fold kvl ~f ~init:(e_variable ~loc v) )
        | E_record_pun kl, `Record ->
          let f acc = function
            | Field.Punned l ->
              let l = Location.unwrap l in
              let update = e_variable ~loc v in
              e_record_update
                ~loc
                { struct_ = acc; label = label_of_access (FieldName l); update }
            | Complete (l, update) ->
              e_record_update
                ~loc
                { struct_ = acc; label = label_of_access (FieldName l); update }
          in
          ( (fun last_proj -> List.fold kl ~f ~init:last_proj)
          , List.fold kl ~f ~init:(e_variable ~loc v) )
        | E_set lst, `Set ->
          let f acc v = e_set_add ~loc v acc in
          ( (fun last_proj -> List.fold lst ~f ~init:last_proj)
          , List.fold lst ~f ~init:(e_variable ~loc v) )
        | _ -> failwith "impossible: won't parse"
      in
      let rhs = compile_assignment_rhs ~loc ~last_proj_update ~lhs:v ~path ~default_rhs in
      i_assign ~loc v rhs
    | x -> make_i ~loc x
  in
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_struct_assign_chainable { expr1 = lhs_expr; op; expr2 = rhs_expr } ->
      let var, rhs = compile_assignment ~raise ~loc lhs_expr rhs_expr in
      e_assign_chainable ~loc { var; op; rhs; returned = lhs_expr }
    | E_update { structure; update } ->
      let struct_var, wrapping =
        match get_e_variable structure with
        | Some v -> v, Fun.id
        | None ->
          let loc = get_e_loc structure in
          let v = Variable.fresh ~loc () in
          let f upd =
            e_simple_let_in
              ~loc
              { binder = p_var ~loc v; rhs = structure; let_result = upd }
          in
          v, f
      in
      let lens_upd (rhs : expr) (lens : Update.field_lens) (lhs : expr) =
        let op cons_name = e_constant ~loc { cons_name; arguments = [ lhs; rhs ] } in
        match lens with
        | Lens_Id -> rhs
        | Lens_Add -> op C_POLYMORPHIC_ADD
        | Lens_Sub -> op C_POLYMORPHIC_SUB
        | Lens_Mult -> op C_MUL
        | Lens_Div -> op C_DIV
        | Lens_Fun -> failwith "no idea"
      in
      let updates =
        List.map update ~f:(function
            | Pun { wrap_content = l; location = loc } ->
              l, e_variable ~loc (Variable.of_input_var ~loc (Label.to_string l))
            | Full_field { field_lhs = hd :: tl; field_lens; field_rhs } ->
              let last_proj_update = lens_upd field_rhs field_lens in
              let lhs = struct_var in
              let init =
                e_record_access
                  ~loc
                  { struct_ = e_variable ~loc struct_var; label = label_of_access hd }
              in
              (match hd with
              | FieldName l -> l, build_update ~loc ~last_proj_update ~init lhs tl
              | Component_num (l, _) ->
                Label.of_string l, build_update ~loc ~last_proj_update ~init lhs tl
              | Component_expr _ -> failwith "impossible")
            | _ -> failwith "impossible")
      in
      let upd =
        List.fold updates ~init:structure ~f:(fun acc (label, update) ->
            e_record_update ~loc { struct_ = acc; label; update })
      in
      wrapping upd
    | e -> make_e ~loc e
  in
  Fold { idle_fold with instruction; expr }


let reduction ~raise =
  { Iter.defaults with
    instruction =
      (function
      | { wrap_content = I_struct_assign _ | I_remove _ | I_patch _; _ } ->
        raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  ; expr =
      (function
      | { wrap_content = E_update _ | E_struct_assign_chainable _; _ } ->
        raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let decompile ~raise:_ = Nothing

open Unit_test_helpers.Instruction

let%expect_test "compile" =
  {|
    (I_struct_assign
      ((lhs_expr
        (E_proj
          ((struct_ (E_map_lookup ((map (E_variable m)) (keys ((EXPR1))))))
           (path (FieldName (Label bar))))))
       (rhs_expr (EXPR2))))
  |}
  |-> compile;
  [%expect
    {|
    (I_assign m
     (E_match
      ((expr
        (E_constant
         ((cons_name C_MAP_FIND_OPT) (arguments ((EXPR1) (E_variable m))))))
       (cases
        (((pattern (P_variant (Label Some) ((P_var gen))))
          (rhs
           (E_constant
            ((cons_name C_MAP_ADD)
             (arguments
              ((EXPR1)
               (E_record_update
                ((struct_ (E_variable gen)) (label (Label bar)) (update (EXPR2))))
               (E_variable m)))))))
         ((pattern (P_variant (Label None) ())) (rhs (E_variable m))))))))
|}]

let%expect_test "compile_wrong_lvalue" =
  {|
    (I_struct_assign
      ((lhs_expr (E_tuple ((EXPR))))
       (rhs_expr (EXPR))))
  |}
  |->! compile;
  [%expect {|
    Err : (Small_passes_wrong_lvalue (E_tuple ((E_variable #EXPR))))
    |}]
