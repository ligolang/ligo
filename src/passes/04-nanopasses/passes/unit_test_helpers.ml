open Simple_utils.Trace
open Simple_utils.Function
open Ast_unified
open Pass_type

let try_with f in_ =
  try_with
    (fun ~raise ~catch:_ ->
      let _v = f ~raise in_ in
      print_endline "This test should have failed")
    (fun ~catch:_ e ->
      Format.fprintf
        Format.std_formatter
        "Err : %a"
        Errors.(error_ppformat ~display_format:Dev ~no_colour:false)
        e)


module Dummies = struct
  (* rewrite dummy S-exp atom to neutral sorts (expr,ty_expr,...),
     better use 'change' @ https://github.com/janestreet/sexp ?
  *)
  type t = string * string

  let lst : t list =
    let loc = Location.generated in
    let dummy name f =
      let dummy str = "(" ^ name ^ str ^ ")", f (name ^ str) in
      dummy "" :: List.map ~f:dummy (List.map ~f:string_of_int (List.range 0 5))
    in
    let dummy_ty_expr =
      let f str =
        S_exp.sexp_of_ty_expr @@ t_var ~loc (Ty_variable.of_input_var ~loc ("#" ^ str))
      in
      dummy "TY_EXPR" f
    in
    let dummy_expr =
      let f str =
        S_exp.sexp_of_expr @@ e_variable ~loc (Variable.of_input_var ~loc ("#" ^ str))
      in
      dummy "EXPR" f
    in
    let dummy_declaration =
      let f str =
        S_exp.sexp_of_declaration
        @@ d_var
             ~loc
             { type_params = None
             ; pattern = p_unit ~loc
             ; rhs_type = None
             ; let_rhs = e_variable ~loc (Variable.of_input_var ~loc ("#" ^ str))
             }
      in
      dummy "DECLARATION" f
    in
    let dummy_block =
      let f str =
        S_exp.sexp_of_block
        @@ block_of_statements
             (List.Ne.singleton
             @@ s_decl ~loc
             @@ d_var
                  ~loc
                  { type_params = None
                  ; pattern = p_unit ~loc
                  ; rhs_type = None
                  ; let_rhs = e_variable ~loc (Variable.of_input_var ~loc ("#" ^ str))
                  })
      in
      dummy "BLOCK" f
    in
    let dummy_statement =
      let f str =
        S_exp.sexp_of_statement
          (s_decl ~loc
          @@ d_var
               ~loc
               { type_params = None
               ; pattern = p_unit ~loc
               ; rhs_type = None
               ; let_rhs = e_variable ~loc (Variable.of_input_var ~loc ("#" ^ str))
               })
      in
      dummy "STATEMENT" f
    in
    List.map
      ~f:(Simple_utils.Pair.map_snd ~f:Sexp.to_string)
      (dummy_ty_expr @ dummy_expr @ dummy_statement @ dummy_block @ dummy_declaration)


  let in_output ((dummy, sexp) : t) = dummy, sexp
  let in_input ((dummy, sexp) : t) = sexp, dummy

  let replace : (t -> t) -> string -> string =
   fun direction init ->
    List.fold lst ~init ~f:(fun acc dummy ->
        let with_, pattern = direction dummy in
        String.substr_replace_all acc ~pattern ~with_)


  let replace_in_input = replace in_input
  let replace_in_output = replace in_output
  let post_proc_re_indent = replace
end

let raise : (Errors.t, Main_warnings.all) raise = raise_failwith "test"

let expected_failure
    (morphers : _ Morphing.morphers)
    (input : 'a)
    (pass : raise:_ -> pass_kind)
    : unit
  =
  let f ~raise = Morphing.mk_code_transformation (pass ~raise) morphers in
  try_with f input


let expected_sucess
    (morphers : _ Morphing.morphers)
    (input : 'a)
    (pass : raise:raise_t -> pass_kind)
    (to_str : 'a -> string)
    : unit
  =
  let f = Morphing.mk_code_transformation (pass ~raise) morphers in
  Format.printf "%s" (to_str @@ f input)


module type S = sig
  type a

  val morphers : a Morphing.morphers
  val t_of_sexp : Sexp.t -> a
  val sexp_of_t : a -> Sexp.t
end

module Make (X : S) = struct
  let replace_dummies_in =
    (* we go back and forth to string representation to avoid the problem of dummies splitted on multiple line :( *)
    Sexp.to_string_hum ~indent:1
    <@ Sexp.of_string
    <@ Dummies.replace_in_output
    <@ Sexp.to_string
    <@ X.sexp_of_t


  let replace_dummies_out = X.t_of_sexp <@ Sexp.of_string <@ Dummies.replace_in_input
  let expected_failure i pass = expected_failure X.morphers (replace_dummies_out i) pass

  let expected_sucess i pass =
    expected_sucess X.morphers (replace_dummies_out i) pass replace_dummies_in


  let ( |-> ) = expected_sucess
  let ( |->! ) = expected_failure
end

module Expr = Make (struct
  type a = expr

  let morphers = Morphing.expr_morphers
  let t_of_sexp = S_exp.expr_of_sexp
  let sexp_of_t = S_exp.sexp_of_expr
end)

module Ty_expr = Make (struct
  type a = ty_expr

  let morphers = Morphing.ty_expr_morphers
  let t_of_sexp = S_exp.ty_expr_of_sexp
  let sexp_of_t = S_exp.sexp_of_ty_expr
end)

module Program = Make (struct
  type a = program

  let morphers = Morphing.program_morphers
  let t_of_sexp = S_exp.program_of_sexp
  let sexp_of_t = S_exp.sexp_of_program
end)

module Pattern = Make (struct
  type a = pattern

  let morphers = Morphing.pattern_morphers
  let t_of_sexp = S_exp.pattern_of_sexp
  let sexp_of_t = S_exp.sexp_of_pattern
end)

module Block = Make (struct
  type a = block

  let morphers = Morphing.block_morphers
  let t_of_sexp = S_exp.block_of_sexp
  let sexp_of_t = S_exp.sexp_of_block
end)

module Declaration = Make (struct
  type a = declaration

  let morphers = Morphing.declaration_morphers
  let t_of_sexp = S_exp.declaration_of_sexp
  let sexp_of_t = S_exp.sexp_of_declaration
end)

module Instruction = Make (struct
  type a = instruction

  let morphers = Morphing.instruction_morphers
  let t_of_sexp = S_exp.instruction_of_sexp
  let sexp_of_t = S_exp.sexp_of_instruction
end)
