open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
open Syntax_types

(* 
This pass handles the special cases of type annotation in JsLIGO.
These are the cases where a E_annot remain a E_annot

1. The first such case is nat and tez/mutez annotations.

2. The second case is type annotation of code injection.
  
*)

let compile ~raise ~syntax =
  let () = ignore raise in
  (* TODO : Retrict pass to JsLIGO syntax *)
  let pass_expr : (expr, ty_expr, pattern, block, mod_expr) expr_ -> expr =
   fun expr ->
    let loc = Location.get_location expr in
    let expr = Location.unwrap expr in
    let unchanged () = make_e ~loc expr in
    match expr with
    | E_annot (e, t) ->
      (match get_e e, get_t t with
      (* Conversion of number literals s*)
      | E_literal (Literal_int i), T_var tv ->
        if Ty_variable.is_name tv "nat"
        then e_nat_z ~loc i
        else if Ty_variable.is_name tv "tez"
        then (
          let mutez = Z.mul (Z.of_int 1_000_000) i in
          e_mutez_z ~loc mutez)
        else if Ty_variable.is_name tv "mutez"
        then e_mutez_z ~loc i
        else unchanged ()
      (* Type-annotated code injection *)
      | E_raw_code { language; code }, _ ->
        e_raw_code ~loc { language; code = e_annot ~loc (code, t) }
      | _ -> unchanged ())
    | _ -> unchanged ()
  in
  match syntax with
  | JsLIGO -> `Cata { idle_cata_pass with expr = pass_expr }
  | _ -> `Cata idle_cata_pass


let reduction ~raise =
  let expr : _ expr_ -> unit =
   fun e ->
    match Location.unwrap e with
    | E_annot (e, t) ->
      (match get_e e, get_t t with
      | E_literal (Literal_int _), T_var tv ->
        if Ty_variable.is_name tv "nat"
           || Ty_variable.is_name tv "tez"
           || Ty_variable.is_name tv "mutez"
        then raise.error (wrong_reduction __MODULE__)
        else ()
      | _ -> ())
    | _ -> ()
  in
  { Iter.defaults with expr }


let pass ~raise ~syntax =
  morph
    ~name:__MODULE__
    ~compile:(compile ~raise ~syntax)
    ~decompile:`None
    ~reduction_check:(reduction ~raise)


open Unit_test_helpers

let%expect_test "number_42_as_nat" =
  {|
  ((PE_declaration
    (D_var (
      (pattern (P_var y))
      (let_rhs (
        E_annot (
          (E_literal (Literal_int 42))
          (T_var nat)
        )
      ))
    ))
  ))
  |}
  |-> pass ~raise ~syntax:JsLIGO;
  [%expect
    {|
    ((PE_declaration
      (D_var ((pattern (P_var y)) (let_rhs (E_literal (Literal_nat 42))))))) |}]

let%expect_test "number_42_as_mutez" =
  {|
  ((PE_declaration
    (D_var (
      (pattern (P_var y))
      (let_rhs (
        E_annot (
          (E_literal (Literal_int 42))
          (T_var mutez)
        )
      ))
    ))
  ))
  |}
  |-> pass ~raise ~syntax:JsLIGO;
  [%expect
    {|
    ((PE_declaration
      (D_var ((pattern (P_var y)) (let_rhs (E_literal (Literal_mutez 42))))))) |}]

let%expect_test "number_42_as_tez" =
  {|
  ((PE_declaration
    (D_var (
      (pattern (P_var y))
      (let_rhs (
        E_annot (
          (E_literal (Literal_int 42))
          (T_var tez)
        )
      ))
    ))
  ))
  |}
  |-> pass ~raise ~syntax:JsLIGO;
  [%expect
    {|
    ((PE_declaration
      (D_var
       ((pattern (P_var y)) (let_rhs (E_literal (Literal_mutez 42000000))))))) |}]

let%expect_test "code_inj" =
  {|
  ((PE_declaration
    (D_var (
      (pattern (P_var y))
      (let_rhs
        (E_annot (
          (E_raw_code (
            (language Michelson)
            (code (E_literal (Literal_string (Verbatim "{ UNPAIR ; ADD }") )))
          ))
          (T_fun (
            (T_prod ((T_var nat) (T_var nat)))
            (T_var nat)
          ))
        ))
      )
    ))
  ))
  |}
  |-> pass ~raise ~syntax:JsLIGO;
  [%expect
    {|
    ((PE_declaration
      (D_var
       ((pattern (P_var y))
        (let_rhs
         (E_raw_code
          ((language Michelson)
           (code
            (E_annot
             ((E_literal (Literal_string (Verbatim "{ UNPAIR ; ADD }")))
              (T_fun ((T_prod ((T_var nat) (T_var nat))) (T_var nat))))))))))))) |}]
