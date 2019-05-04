module Run_mini_c = Run_mini_c

open Trace
module Parser = Parser
module AST_Raw = Parser.Pascaligo.AST
module AST_Simplified = Ast_simplified
module AST_Typed = Ast_typed
module Mini_c = Mini_c
module Typer = Typer
module Transpiler = Transpiler
(* module Parser_multifix = Multifix
 * module Simplify_multifix = Simplify_multifix *)


let simplify (p:AST_Raw.t) : Ast_simplified.program result = Simplify.Pascaligo.simpl_program p
let simplify_expr (e:AST_Raw.expr) : Ast_simplified.annotated_expression result = Simplify.Pascaligo.simpl_expression e
let unparse_simplified_expr (e:AST_Simplified.annotated_expression) : string result =
  ok @@ Format.asprintf "%a" AST_Simplified.PP.annotated_expression e

let type_ (p:AST_Simplified.program) : AST_Typed.program result = Typer.type_program p
let type_expression ?(env:Typer.Environment.t = Typer.Environment.full_empty)
    (e:AST_Simplified.annotated_expression) : AST_Typed.annotated_expression result =
  Typer.type_annotated_expression env e
let untype_expression (e:AST_Typed.annotated_expression) : AST_Simplified.annotated_expression result = Typer.untype_annotated_expression e

let transpile (p:AST_Typed.program) : Mini_c.program result = Transpiler.translate_program p
let transpile_entry (p:AST_Typed.program) (name:string) : Mini_c.anon_function result = Transpiler.translate_entry p name
let transpile_expression ?(env:Mini_c.Environment.t = Mini_c.Environment.empty)
    (e:AST_Typed.annotated_expression) : Mini_c.expression result = Transpiler.translate_annotated_expression env e
let transpile_value
    (e:AST_Typed.annotated_expression) : Mini_c.value result =
  let%bind f =
    let open Transpiler in
    let (f, t) = functionalize e in
    let%bind main = translate_main f t in
    ok main
  in

  let input = Mini_c.Combinators.d_unit in
  let%bind r = Run_mini_c.run_entry f input in
  ok r

let untranspile_value (v : Mini_c.value) (e:AST_Typed.type_value) : AST_Typed.annotated_expression result =
  Transpiler.untranspile v e

let compile : Mini_c.program -> string -> Compiler.Program.compiled_program result = Compiler.Program.translate_program

let type_file ?(debug_simplify = false) ?(debug_typed = false)
    (path:string) : AST_Typed.program result =
  let%bind raw = Parser.parse_file path in
  let%bind simpl =
    trace (simple_error "simplifying") @@
    simplify raw in
  (if debug_simplify then
     Format.(printf "Simplified : %a\n%!" AST_Simplified.PP.program simpl)
  ) ;
  let%bind typed =
    trace (simple_error "typing") @@
    type_ simpl in
  (if debug_typed then (
      Format.(printf "Typed : %a\n%!" AST_Typed.PP.program typed)
    )) ;
  ok typed


let easy_evaluate_typed (entry:string) (program:AST_Typed.program) : AST_Typed.annotated_expression result =
  let%bind result =
    let%bind mini_c_main =
      transpile_entry program entry in
    Run_mini_c.run_entry mini_c_main (Mini_c.Combinators.d_unit) in
  let%bind typed_result =
    let%bind typed_main = Ast_typed.get_entry program entry in
    untranspile_value result typed_main.type_annotation in
  ok typed_result

let easy_evaluate_typed_simplified (entry:string) (program:AST_Typed.program) : Ast_simplified.annotated_expression result =
  let%bind result =
    let%bind mini_c_main =
      transpile_entry program entry in
    Run_mini_c.run_entry mini_c_main (Mini_c.Combinators.d_unit) in
  let%bind typed_result =
    let%bind typed_main = Ast_typed.get_entry program entry in
    untranspile_value result typed_main.type_annotation in
  let%bind annotated_result = untype_expression typed_result in
  ok annotated_result

let easy_evaluate_typed = trace_f_2_ez easy_evaluate_typed (thunk "easy evaluate typed")

let easy_run_typed
    ?(debug_mini_c = false) ?amount (entry:string)
    (program:AST_Typed.program) (input:AST_Typed.annotated_expression) : AST_Typed.annotated_expression result =
  let%bind () =
    let open Ast_typed in
    let%bind (Declaration_constant (d , _)) = get_declaration_by_name program entry in
    let%bind (arg_ty , _) =
      trace_strong (simple_error "entry-point doesn't have a function type") @@
      get_t_function @@ get_type_annotation d.annotated_expression in
    Ast_typed.assert_type_value_eq (arg_ty , (Ast_typed.get_type_annotation input))
  in

  let%bind mini_c_main =
    trace (simple_error "transpile mini_c entry") @@
    transpile_entry program entry in
  (if debug_mini_c then
     Format.(printf "Mini_c : %a\n%!" Mini_c.PP.function_ mini_c_main)
  ) ;

  let%bind mini_c_value = transpile_value input in

  let%bind mini_c_result =
    let error =
      let title () = "run Mini_c" in
      let content () =
        Format.asprintf "\n%a" Mini_c.PP.function_ mini_c_main
      in
      error title content in
    trace error @@
    Run_mini_c.run_entry ?amount mini_c_main mini_c_value in
  let%bind typed_result =
    let%bind main_result_type =
      let%bind typed_main = Ast_typed.get_functional_entry program entry in
      match (snd typed_main).type_value' with
      | T_function (_, result) -> ok result
      | _ -> simple_fail "main doesn't have fun type" in
    untranspile_value mini_c_result main_result_type in
  ok typed_result

let easy_run_typed_simplified
    ?(debug_mini_c = false) ?(debug_michelson = false) ?amount (entry:string)
    (program:AST_Typed.program) (input:Ast_simplified.annotated_expression) : Ast_simplified.annotated_expression result =
  let%bind mini_c_main =
    trace (simple_error "transpile mini_c entry") @@
    transpile_entry program entry in
  (if debug_mini_c then
     Format.(printf "Mini_c : %a\n%!" Mini_c.PP.function_ mini_c_main)
  ) ;

  let%bind typed_value =
    let env =
      let last_declaration = Location.unwrap List.(hd @@ rev program) in
      match last_declaration with
      | Declaration_constant (_ , env) -> env
    in
    type_expression ~env input in
  let%bind mini_c_value = transpile_value typed_value in

  let%bind mini_c_result =
    let error =
      let title () = "run Mini_c" in
      let content () =
        Format.asprintf "\n%a" Mini_c.PP.function_ mini_c_main
      in
      error title content in
    trace error @@
    Run_mini_c.run_entry ~debug_michelson ?amount mini_c_main mini_c_value in
  let%bind typed_result =
    let%bind main_result_type =
      let%bind typed_main = Ast_typed.get_functional_entry program entry in
      match (snd typed_main).type_value' with
      | T_function (_, result) -> ok result
      | _ -> simple_fail "main doesn't have fun type" in
    untranspile_value mini_c_result main_result_type in
  let%bind annotated_result = untype_expression typed_result in
  ok annotated_result

let easy_run_main_typed
    ?(debug_mini_c = false)
    (program:AST_Typed.program) (input:AST_Typed.annotated_expression) : AST_Typed.annotated_expression result =
  easy_run_typed ~debug_mini_c "main" program input

let easy_run_main (path:string) (input:string) : AST_Typed.annotated_expression result =
  let%bind typed = type_file path in

  let%bind raw_expr = Parser.parse_expression input in
  let%bind simpl_expr = simplify_expr raw_expr in
  let%bind typed_expr = type_expression simpl_expr in
  easy_run_main_typed typed typed_expr

let compile_file (source: string) (entry_point:string) : Micheline.Michelson.t result =
  let%bind raw =
    trace (simple_error "parsing") @@
    Parser.parse_file source in
  let%bind simplified =
    trace (simple_error "simplifying") @@
    simplify raw in
  let%bind typed =
    trace (simple_error "typing") @@
    type_ simplified in
  let%bind mini_c =
    trace (simple_error "transpiling") @@
    transpile typed in
  let%bind {body = michelson} =
    trace (simple_error "compiling") @@
    compile mini_c entry_point in
  ok michelson

module Contract = Contract
