open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

(*
    Certain types should translate directly to specific Michelson types
    and thus have their own AST node. (e.g. michelson_or, michelson_pair).
  *)

include Flag.With_arg (struct
  type flag = Syntax_types.t
end)

let t_michelson_or ~loc (l : ty_expr) l_ann (r : ty_expr) r_ann =
  t_sum_raw
    ~loc
    (Non_linear_rows.make
       [ Label "M_left", Some l, [ l_ann ]; Label "M_right", Some r, [ r_ann ] ])


let t_michelson_pair ~loc l l_ann r r_ann =
  t_record_raw
    ~loc
    (Non_linear_rows.make [ Label "0", Some l, [ l_ann ]; Label "1", Some r, [ r_ann ] ])


let compile ~raise =
  let syntax = get_flag () in
  let pass_ty : ty_expr ty_expr_ -> ty_expr =
   fun t ->
    let loc = t.location in
    let return_self () = make_t ~loc t.wrap_content in
    let () = ignore raise in
    match Location.unwrap t with
    | T_app { constr; type_args } ->
      (match get_t constr with
      | T_var tv ->
        let type_args =
          (* this comes from the weird jsligo syntax for michelson types *)
          match type_args with
          | prod, [] when Syntax_types.equal syntax JsLIGO ->
            (match get_t_prod prod with
            | Some tys -> tys
            | None -> type_args)
          | _ -> type_args
        in
        let make_michelson_type name make_t =
          match type_args with
          | t1, [ t2; t3; t4 ] ->
            (match get_t t2, get_t t4 with
            | T_string s1, T_string s2 ->
              make_t t1 (Attribute.make "annot" s1) t3 (Attribute.make "annot" s2)
            | _ -> raise.error @@ michelson_type_wrong name constr)
          | _ -> raise.error @@ michelson_type_wrong_arity name constr
        in
        if Ty_variable.is_name tv "michelson_or"
        then make_michelson_type "michelson_or" (t_michelson_or ~loc) None
        else if Ty_variable.is_name tv "michelson_pair"
        then make_michelson_type "michelson_pair" (t_michelson_pair ~loc)
        else return_self ()
      | _ -> return_self ())
    | _ -> return_self ()
  in
  Fold { idle_fold with ty_expr = pass_ty }


let reduction ~raise =
  let () = ignore raise in
  let ty_expr : ty_expr ty_expr_ -> unit =
   fun te_ ->
    match Location.unwrap te_ with
    | T_app { constr; type_args = _ } ->
      (match get_t constr with
      | T_var tv ->
        if Ty_variable.is_name tv "michelson_or"
           || Ty_variable.is_name tv "michelson_pair"
        then raise.error (wrong_reduction __MODULE__)
        else ()
      | _ -> ())
    | _ -> ()
  in
  { Iter.defaults with ty_expr }


let name = __MODULE__
let decompile ~raise:_ = Nothing

open Unit_test_helpers.Ty_expr

let flag_bef = !flag
let () = flag := Some (true, CameLIGO)

let%expect_test "compile_michelson_pair" =
  {|
      (T_app
        ((constr (T_var michelson_pair))
         (type_args
           ((TY_EXPR1) (T_string w) (TY_EXPR2) (T_string v)))))
  |}
  |-> compile;
  [%expect
    {|
        (T_record_raw
         (((Label 0)
           ((associated_type ((TY_EXPR1))) (attributes (((key annot) (value w))))
            (decl_pos 0)))
          ((Label 1)
           ((associated_type ((TY_EXPR2))) (attributes (((key annot) (value v))))
            (decl_pos 1))))) |}]

let%expect_test "compile_michelson_or" =
  {|
      (T_app
        ((constr (T_var michelson_or))
         (type_args
           ((TY_EXPR1) (T_string w) (TY_EXPR2) (T_string v)))))
  |}
  |-> compile;
  [%expect
    {|
        (T_sum_raw
         (((Label M_left)
           ((associated_type ((TY_EXPR1))) (attributes (((key annot) (value w))))
            (decl_pos 0)))
          ((Label M_right)
           ((associated_type ((TY_EXPR2))) (attributes (((key annot) (value v))))
            (decl_pos 1))))
         ()) |}]

let%expect_test "compile_michelson_or_wrong_arity" =
  {|
    (T_app
      ((constr (T_var michelson_or))
       (type_args ((T_string w) (TY_EXPR) (T_string v)))))
  |}
  |->! compile;
  [%expect
    {|
        Err : (Small_passes_michelson_type_wrong_arity
                  (michelson_or (T_var michelson_or))) |}]

let%expect_test "compile_michelson_or_type_wrong" =
  {|
    (T_app
      ((constr (T_var michelson_or))
       (type_args ((T_var int) (TY_EXPR1) (TY_EXPR2) (T_string v)))))
  |}
  |->! compile;
  [%expect
    {| Err : (Small_passes_michelson_type_wrong (michelson_or (T_var michelson_or))) |}]

let () = flag := flag_bef
