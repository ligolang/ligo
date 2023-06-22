open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

(* generate helpers for sum_types : enum ; setter and getter
   possible TODOs:
   - use attribute value to allow chosing enum/get/set/all
   - add more type annotations in the generated function (for safety)
*)
include Flag.No_arg ()

let name = __MODULE__

let prefix_let_lhs ~loc prefix str =
  p_var ~loc (Variable.of_input_var ~loc (prefix ^ "_" ^ String.uncapitalize str))


let simple_decl ~loc lhs args let_rhs =
  d_let
    ~loc
    { is_rec = false; type_params = None; pattern = lhs, args; rhs_type = None; let_rhs }


let gen_enum : ty_expr option Non_linear_rows.t * Location.t -> declaration list =
 fun (rows, loc) ->
  rows
  |> List.mapi ~f:(fun i (Label.Label x, _) -> i, x)
  |> List.map ~f:(fun (i, str) ->
         simple_decl ~loc (prefix_let_lhs ~loc "enum" str) [] (e_nat ~loc i))


let gen_getters : ty_expr option Non_linear_rows.t * Location.t -> declaration list =
 fun (rows, loc) ->
  let v_x = Variable.of_input_var ~loc "x" in
  let ev_x = e_variable ~loc v_x in
  let p_x = p_var ~loc v_x in
  rows
  |> List.map ~f:fst
  |> List.map ~f:(fun label ->
         let match_expr =
           e_match
             ~loc
             { expr = ev_x
             ; cases =
                 ( { pattern = p_variant ~loc label (Some p_x); rhs = e_some ~loc ev_x }
                 , [ { pattern = p_var ~loc (Variable.fresh ~loc ()); rhs = e_none ~loc }
                   ] )
             }
         in
         simple_decl
           ~loc
           (prefix_let_lhs ~loc "get" (Label.to_string label))
           [ p_x ]
           match_expr)


let gen_makers : ty_expr option Non_linear_rows.t * Location.t -> declaration list =
 fun (rows, loc) ->
  let v_x = Variable.of_input_var ~loc "x" in
  rows
  |> List.map ~f:fst
  |> List.map ~f:(fun label ->
         let ctor =
           e_applied_constructor
             ~loc
             { constructor = label; element = e_variable ~loc v_x }
         in
         simple_decl
           ~loc
           (prefix_let_lhs ~loc "make" (Label.to_string label))
           [ p_var ~loc v_x ]
           ctor)


let gen_helpers (decl, _name, sum, loc) =
  let x = sum, loc in
  [ gen_makers x; gen_getters x; gen_enum x ]
  |> List.join
  |> List.cons decl
  |> List.map ~f:(fun d -> make_pe (PE_declaration d))


let compile ~raise:_ =
  let program : _ program_ -> program =
   fun p ->
    let extended =
      List.fold p ~init:[] ~f:(fun acc pe : program_entry list ->
          let default = acc @ [ pe ] in
          let type_sum_decl_opt =
            let open Simple_utils.Option in
            let* d = get_pe_declaration pe in
            let* { key; value }, decl = get_d_attr d in
            let* { name; type_expr } = get_d_type decl in
            let* sum = get_t_sum_raw type_expr in
            if String.equal key "ppx_helpers" && Option.is_none value
            then Some (decl, name, sum, get_t_loc type_expr)
            else None
          in
          Option.value_map type_sum_decl_opt ~default ~f:gen_helpers)
    in
    make_prg extended
  in
  Fold { idle_fold with program }


let reduction ~raise =
  { Iter.defaults with
    declaration =
      (function
      | { wrap_content = D_attr ({ key = "ppx_helpers"; value = None }, _); _ } ->
        raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let decompile ~raise:_ = Nothing

open Unit_test_helpers.Program

let%expect_test "compile" =
  {|
  ((PE_declaration
   (D_attr
    (((key ppx_helpers))
     (D_type
      ((name dyn_param)
       (type_expr
        (T_sum_raw
         (((Label Foo) ((associated_type ((TY_EXPR1))) (decl_pos 0)))
          ((Label Bar) ((associated_type ()) (decl_pos 1))))))))))))
  |}
  |-> compile;
  [%expect
    {|
    ((PE_declaration
      (D_type
       ((name dyn_param)
        (type_expr
         (T_sum_raw
          (((Label Foo) ((associated_type ((TY_EXPR1))) (decl_pos 0)))
           ((Label Bar) ((associated_type ()) (decl_pos 1)))))))))
     (PE_declaration
      (D_let
       ((pattern ((P_var make_foo) (P_var x)))
        (let_rhs
         (E_applied_constructor
          ((constructor (Label Foo)) (element (E_variable x))))))))
     (PE_declaration
      (D_let
       ((pattern ((P_var make_bar) (P_var x)))
        (let_rhs
         (E_applied_constructor
          ((constructor (Label Bar)) (element (E_variable x))))))))
     (PE_declaration
      (D_let
       ((pattern ((P_var get_foo) (P_var x)))
        (let_rhs
         (E_match
          ((expr (E_variable x))
           (cases
            (((pattern (P_variant (Label Foo) ((P_var x))))
              (rhs
               (E_applied_constructor
                ((constructor (Label Some)) (element (E_variable x))))))
             ((pattern (P_var gen))
              (rhs
               (E_applied_constructor
                ((constructor (Label None)) (element (E_literal Literal_unit))))))))))))))
     (PE_declaration
      (D_let
       ((pattern ((P_var get_bar) (P_var x)))
        (let_rhs
         (E_match
          ((expr (E_variable x))
           (cases
            (((pattern (P_variant (Label Bar) ((P_var x))))
              (rhs
               (E_applied_constructor
                ((constructor (Label Some)) (element (E_variable x))))))
             ((pattern (P_var gen))
              (rhs
               (E_applied_constructor
                ((constructor (Label None)) (element (E_literal Literal_unit))))))))))))))
     (PE_declaration
      (D_let
       ((pattern ((P_var enum_foo))) (let_rhs (E_literal (Literal_nat 0))))))
     (PE_declaration
      (D_let
       ((pattern ((P_var enum_bar))) (let_rhs (E_literal (Literal_nat 1))))))) |}]
