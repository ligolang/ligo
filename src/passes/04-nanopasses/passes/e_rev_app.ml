(* 
The reverse application operator '|>' is syntactic sugar for function application.
For example, this :
   let res = x \> f \> g |> h
Is equivalent to 
  let res = h (g (f x))
This pass unsugars the E_rev_app operator into normal function application E_application
*)

open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

let compile ~raise =
  let pass_expr : _ expr_ -> expr =
   fun e ->
    let loc = e.location in
    let return_self () = make_e ~loc e.wrap_content in
    let () = ignore raise in
    match Location.unwrap e with
    | E_rev_app { x; f } -> e_call ~loc f (Location.wrap ~loc:(get_e_loc f) [ x ])
    | _ -> return_self ()
  in
  `Cata { idle_cata_pass with expr = pass_expr }


let reduction ~raise =
  let () = ignore raise in
  let expr : _ expr_ -> unit =
   fun e ->
    match Location.unwrap e with
    | E_rev_app _ -> raise.error (wrong_reduction __MODULE__)
    | _ -> ()
  in
  { Iter.defaults with expr }


let pass ~raise =
  morph
    ~name:__MODULE__
    ~compile:(compile ~raise)
    ~decompile:`None
    ~reduction_check:(reduction ~raise)


open Unit_test_helpers

let%expect_test "compile_rev_app_simple" =
  {|
  ((PE_declaration
    (D_let
      ((is_rec false) (type_params ()) (pattern ((P_var res)))
        (rhs_type ())
        (let_rhs (E_rev_app ((x (E_variable x)) (f (E_variable f)))))))))
  |}
  |-> pass ~raise;
  [%expect
    {|
    ((PE_declaration
      (D_let
       ((is_rec false) (type_params ()) (pattern ((P_var res))) (rhs_type ())
        (let_rhs (E_call (E_variable f) ((E_variable x)))))))) |}]

let%expect_test "compile_rev_app_successive" =
  {|
  ((PE_declaration
    (D_let
      ((is_rec false) (type_params ()) (pattern ((P_var res)))
        (rhs_type ())
        (let_rhs
          (E_rev_app
            ((x
               (E_rev_app
                 ((x
                    (E_rev_app
                      ((x (E_variable x)) (f (E_variable f)))))
                   (f (E_variable g)))))
              (f (E_variable h)))))))))
  |}
  |-> pass ~raise;
  [%expect
    {|
    ((PE_declaration
      (D_let
       ((is_rec false) (type_params ()) (pattern ((P_var res))) (rhs_type ())
        (let_rhs
         (E_call (E_variable h)
          ((E_call (E_variable g) ((E_call (E_variable f) ((E_variable x)))))))))))) |}]

let%expect_test "compile_rev_app_precedence" =
  {|
  ((PE_declaration
    (D_let
      ((is_rec false) (type_params ()) (pattern ((P_var res)))
        (rhs_type ())
        (let_rhs
          (E_rev_app
            ((x
               (E_constant
                 ((cons_name C_POLYMORPHIC_ADD)
                   (arguments
                     ((E_variable x) (E_literal (Literal_int 1)))))))
              (f (E_call (E_variable f) ((E_variable y)))))))))))
   |}
  |-> pass ~raise;
  [%expect
    {|
    ((PE_declaration
      (D_let
       ((is_rec false) (type_params ()) (pattern ((P_var res))) (rhs_type ())
        (let_rhs
         (E_call (E_call (E_variable f) ((E_variable y)))
          ((E_constant
            ((cons_name C_POLYMORPHIC_ADD)
             (arguments ((E_variable x) (E_literal (Literal_int 1))))))))))))) |}]
