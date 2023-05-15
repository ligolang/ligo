open Errors
open Ast_unified
open Pass_type
open Simple_utils.Trace

(* in expression;
```
let x =
  type 'a t = 'a * 'a in
  ((1,2) : int t)
```

this parses incorectly (the 'a binder is ignored) : See with christian
*)
let name = __MODULE__

include Flag.No_arg ()

let abstract_type params ty_expr =
  List.Ne.fold_right params ~init:ty_expr ~f:(fun ty_binder acc ->
      t_abstraction ~loc:(get_t_loc ty_expr) { ty_binder; kind = Type; type_ = acc })


let compile ~raise:_ =
  let declaration : _ declaration_ -> declaration =
   fun d ->
    let loc = Location.get_location d in
    match Location.unwrap d with
    | D_type_abstraction { name; params = None; type_expr } ->
      d_type ~loc { name; type_expr }
    | D_type_abstraction { name; params = Some params; type_expr } ->
      let type_expr = abstract_type params type_expr in
      d_type ~loc { name; type_expr }
    | d -> make_d ~loc d
  in
  let expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | E_type_in { type_decl = { name; params = Some params; type_expr }; body } ->
      let type_expr = abstract_type params type_expr in
      e_type_in ~loc { type_decl = { name; params = None; type_expr }; body }
    | e -> make_e ~loc e
  in
  Fold { idle_fold with declaration; expr }


let reduction ~raise =
  { Iter.defaults with
    declaration =
      (function
      | { wrap_content = D_type_abstraction _; _ } ->
        raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  ; expr =
      (function
      | { wrap_content = E_type_in { type_decl = { params = Some _; _ }; _ }; _ } ->
        raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let decompile ~raise:_ =
  let pass_declaration : _ declaration_ -> declaration =
   fun decl ->
    { fp =
        Location.map
          (function
            | D_type { name; type_expr }
            | D_type_abstraction { name; params = None; type_expr } ->
              let rec aux tv e =
                match get_t_abstraction e with
                | None -> tv, e
                | Some { ty_binder; kind = _; type_ } -> aux (tv @ [ ty_binder ]) type_
              in
              let params, tail = aux [] type_expr in
              let params = params |> List.Ne.of_list_opt in
              D_type_abstraction { name; params; type_expr = tail }
            | x -> x)
          decl
    }
  in
  Fold { idle_fold with declaration = pass_declaration }


open Unit_test_helpers.Declaration

let%expect_test "compile" =
  {|
    (D_type_abstraction ((name my_t) (params ((a b))) (type_expr (TY_EXPR))))
  |}
  |-> compile;
  [%expect
    {|
    (D_type
     ((name my_t)
      (type_expr
       (T_abstraction
        ((ty_binder a) (kind Type)
         (type_ (T_abstraction ((ty_binder b) (kind Type) (type_ (TY_EXPR)))))))))) |}]

let%expect_test "decompile" =
  {|
    (D_type
      ((name my_t)
       (type_expr
        (T_abstraction
          ((ty_binder a)
           (kind Type)
           (type_ 
            (T_abstraction
              ((ty_binder b)
               (kind Type)
               (type_ (TY_EXPR))))))))))
  |}
  |-> decompile;
  [%expect
    {| (D_type_abstraction ((name my_t) (params ((a b))) (type_expr (TY_EXPR)))) |}]
