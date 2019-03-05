[@@@warning "-30"]

module I = AST

open Region

module SMap = Map.Make(String)

module O = struct
  type type_name = string
  type ast = {
      types        : type_decl list;
      parameter    : typed_var;
      storage      : typed_var;
      operations   : typed_var;
      declarations : decl list;
      prev         : I.ast;
    }
  and typed_var = { name:string; ty:type_expr }
  and type_decl = { name:string; ty:type_expr }
  and decl = { name:string; ty:type_expr; value: expr }
  and type_expr =
    Prod     of type_expr list
  | Sum      of (type_name * type_expr) list
  | Record   of (type_name * type_expr) list
  | TypeApp  of type_name * type_expr list
  | Function of { args: type_expr list; ret: type_expr }
  | Ref      of type_expr
  | Unit
  | Lambda   of {
      parameters:   type_expr SMap.t;
      declarations: decl list;
      instructions: instr list;
      body:         expr;
    }
  | TODO
  and expr =
    Binary   of { operator: string; left: expr; right: expr }
  | Variable of string
  | UnitExpr
  and instr =
    Fail
end

(* open Sanity: *)
let (|>) v f = f v       (* pipe f to v *)
let (@@) f v = f v       (* apply f on v *)
let (@.) f g x = f (g x) (* compose *)
let map f l = List.rev (List.rev_map f l)
let fold_map f a l =
  let f (acc, l) elem =
    let acc', elem' = f acc elem
    in acc', (elem' :: l) in
  let last_acc, last_l = List.fold_left f (a, []) l
  in last_acc, List.rev last_l

(* Simplify the AST *)

let s_nsepseq : ('a,'sep) Utils.nsepseq -> 'a list =
  fun (first, rest) -> first :: (map snd rest)

let s_sepseq : ('a,'sep) Utils.sepseq -> 'a list =
  function
    None         -> []
  | Some nsepseq -> s_nsepseq nsepseq

let s_name {value=name; region} : string =
  let _ = region in
  name

let s_sum_type {value=sequence; region} : O.type_expr =
  let _ = region in
  let _todo = sequence in
(*  Sum (List.map s_type_expr (s_nsepseq sequence)) *)
  TODO

and s_record_type {value=(kwd_record, field_decls, kwd_end); region} : O.type_expr =
  let _ = kwd_record,region,kwd_end in
  let _todo = (* s_field_decls *) field_decls in
  TODO

and s_type_app {value=node; region} : O.type_expr =
  let _ = region in
  let _todo = node in
  TODO
  (* let type_name, type_tuple = node in *)
  (* s_var        type_name; *)
  (* s_type_tuple type_tuple *)

and s_par_type {value=node; region} : O.type_expr =
  let _ = region in
  let _todo = node in
  TODO

and s_var {region; value=lexeme} : O.type_expr =
  let _ = region in
  let _todo = lexeme in
  TODO

(*  let lpar, type_expr, rpar = node in
  s_token     lpar "(";
  s_type_expr type_expr;
  s_token     rpar ")"*)

(* I.{value=sequence; region} *)
(*   (\* let _ = region in *\) *)
(* (\*  Prod (List.map s_type_expr (s_nsepseq sequence)) *\) *)

let s_cartesian   _x = O.TODO
let s_sum_type    _x = O.TODO
and s_record_type _x = O.TODO
and s_type_app    _x = O.TODO
and s_par_type    _x = O.TODO
and s_var         _x = O.TODO


and s_type_expr : I.type_expr -> O.type_expr = function
  Prod    cartesian   -> s_cartesian   cartesian
| Sum     sum_type    -> s_sum_type    sum_type
| Record  record_type -> s_record_type record_type
| TypeApp type_app    -> s_type_app    type_app
| ParType par_type    -> s_par_type    par_type
| TAlias  type_alias  -> s_var         type_alias


(* let s_ast (ast : I.ast) : O.ast = *)
(*   let I.{types;constants;parameter;storage;operations;lambdas;block;eof} = ast in *)
(*   let _ = eof in *)
(*   O.{ *)
(*       types        = List.map s_type_decl types; *)
(*       parameter    = s_parameter parameter; *)
(*       storage      = s_storage storage; *)
(*       operations   = s_operations operations; *)
(*       declarations = List.append (List.map s_const_decl  constants) *)
(*                                  (List.map s_lambda_decl lambdas) *)
(*                                  [s_main_block block]; *)
(*       prev         = ast *)
(*   } *)

(* and s_type_decl I.{value={kwd_type;name;kwd_is;type_expr;terminator}; region} : O.type_decl = *)
(*   let _ = kwd_type,kwd_is,terminator,region in *)
(*   O.{ name = s_name name; ty = s_type_expr type_expr } *)

(* and s_parameter_decl I.{value={kwd_parameter;name;colon;param_type;terminator};region} : O.typed_var = *)
(*   let _ = region in *)
(*   O.{ name = s_name name; ty = s_type_expr param_type } *)

(* and s_storage_decl I.{value={kwd_storage; store_type; terminator}; region} : O.typed_var = *)
(*   let _ = kwd_storage,terminator,region in *)
(*   O.{ name = "storage"; ty = s_type_expr store_type } *)

(* and s_operations_decl I.{value={kwd_operations;op_type;terminator}; region} : O.typed_var = *)
(*   let _ = kwd_operations,terminator,region in *)
(*   O.{ name = "operations"; ty = s_type_expr op_type } *)

(* and s_const_decl I.{value={kwd_const;name;colon;vtype;equal;init;terminator}; region} : O.decl = *)
(*   let _ = kwd_const,colon,equal,terminator in *)
(*   O.{ name = s_name name; ty = s_type_expr vtype; value = s_expr init } *)

(* and s_lambda_decl : I.lambda_decl -> O.decl = function *)
(*   FunDecl   fun_decl -> s_fun_decl fun_decl *)
(* | ProcDecl proc_decl -> s_proc_decl proc_decl *)

(* and s_fun_decl I.{value={kwd_function;name;param;colon;ret_type;kwd_is;local_decls;block;kwd_with;return;terminator}; region} : O.decl = *)
(*   let _ = kwd_function,colon,kwd_is,kwd_with,terminator in *)
(*   O.{ *)
(*       name = s_name name; *)
(*       ty = Function { args = s_type_expr param; ret = s_type_expr ret_type }; *)
(*       value = Lambda { *)
(*                   parameters   = s_type_expr param; *)
(*                   declarations = List.map s_local_decls local_decls; *)
(*                   instructions = s_block block; *)
(*                   body         = s_expr return *)
(*                 } *)
(*   } *)

(* and s_proc_decl I.{value={kwd_procedure;name;param;kwd_is;local_decls;block;terminator}; region} = *)
(*   let _ = kwd_procedure,kwd_is,terminator in *)
(*   O.{ *)
(*       name = s_name name; *)
(*       ty = Function { args = s_type_expr param; ret = Unit }; *)
(*       value = Lambda { *)
(*                   parameters   = s_type_expr param; *)
(*                   declarations = List.map s_local_decls local_decls; *)
(*                   instructions = s_block block; *)
(*                   body         = O.UnitExpr *)
(*                 } *)
(*   } *)





(* let s_token region lexeme = *)
(*   printf "%s: %s\n"(compact region) lexeme *)

(* and s_var {region; value=lexeme} = *)
(*   printf "%s: Ident \"%s\"\n" (compact region) lexeme *)

(* and s_constr {region; value=lexeme} = *)
(*   printf "%s: Constr \"%s\"\n" *)
(*          (compact region) lexeme *)

(* and s_string {region; value=lexeme} = *)
(*   printf "%s: String \"%s\"\n" *)
(*          (compact region) lexeme *)

(* and s_bytes {region; value = lexeme, abstract} = *)
(*   printf "%s: Bytes (\"%s\", \"0x%s\")\n" *)
(*          (compact region) lexeme *)
(*          (MBytes.to_hex abstract |> Hex.to_string) *)

(* and s_int {region; value = lexeme, abstract} = *)
(*   printf "%s: Int (\"%s\", %s)\n" *)
(*          (compact region) lexeme *)
(*          (Z.to_string abstract) *)

(* and s_cartesian {value=sequence; _} = *)
(*   s_nsepseq "*" s_type_expr sequence *)

(* and s_variant {value=node; _} = *)
(*   let constr, kwd_of, cartesian = node in *)
(*   s_constr    constr; *)
(*   s_token     kwd_of "of"; *)
(*   s_cartesian cartesian *)

(* and s_field_decls sequence = *)
(*   s_nsepseq ";" s_field_decl sequence *)

(* and s_field_decl {value=node; _} = *)
(*   let var, colon, type_expr = node in *)
(*   s_var       var; *)
(*   s_token     colon ":"; *)
(*   s_type_expr type_expr *)

(* and s_type_tuple {value=node; _} = *)
(*   let lpar, sequence, rpar = node in *)
(*   s_token lpar "("; *)
(*   s_nsepseq "," s_var sequence; *)
(*   s_token rpar ")" *)


(* and s_parameters {value=node; _} = *)
(*   let lpar, sequence, rpar = node in *)
(*   s_token lpar "("; *)
(*   s_nsepseq ";" s_param_decl sequence; *)
(*   s_token rpar ")" *)

(* and s_param_decl = function *)
(*   ParamConst param_const -> s_param_const param_const *)
(* | ParamVar   param_var   -> s_param_var   param_var *)

(* and s_param_const {value=node; _} = *)
(*   let kwd_const, variable, colon, type_expr = node in *)
(*   s_token     kwd_const "const"; *)
(*   s_var       variable; *)
(*   s_token     colon ":"; *)
(*   s_type_expr type_expr *)

(* and s_param_var {value=node; _} = *)
(*   let kwd_var, variable, colon, type_expr = node in *)
(*   s_token     kwd_var "var"; *)
(*   s_var       variable; *)
(*   s_token     colon ":"; *)
(*   s_type_expr type_expr *)

(* and s_block {value=node; _} = *)
(*   s_token        node.opening "begin"; *)
(*   s_instructions node.instr; *)
(*   s_terminator   node.terminator; *)
(*   s_token        node.close "end" *)

(* and s_local_decls sequence = *)
(*   List.iter s_local_decl sequence *)

(* and s_local_decl = function *)
(*   LocalLam   decl -> s_lambda_decl decl *)
(* | LocalConst decl -> s_const_decl  decl *)
(* | LocalVar   decl -> s_var_decl    decl *)

(* and s_var_decl {value={kwd_var;name;colon;vtype;ass;init;terminator}; region} = *)


(* and s_instructions {value=sequence; _} = *)
(*   s_nsepseq ";" s_instruction sequence *)

(* and s_instruction = function *)
(*   Single instr -> s_single_instr instr *)
(* |  Block block -> s_block block *)

(* and s_single_instr = function *)
(*   Cond     {value; _} -> s_conditional value *)
(* | Match    {value; _} -> s_match_instr value *)
(* | Ass      instr      -> s_ass_instr instr *)
(* | Loop     loop       -> s_loop loop *)
(* | ProcCall fun_call   -> s_fun_call fun_call *)
(* | Null     kwd_null   -> s_token kwd_null "null" *)
(* | Fail     {value; _} -> s_fail value *)

(* and s_fail (kwd_fail, expr) = *)
(*   s_token kwd_fail "fail"; *)
(*   s_expr expr *)

(* and s_conditional node ={kwd_if;test;kwd_then;ifso;kwd_else;ifnot} *)


(* and s_regionmatch_instr node ={kwd_match;expr;kwd_with;cases;kwd_end} *)


(* and s_region_cases {value=sequence; _} = *)
(*   s_nsepseq "|" s_case sequence *)

(* and s_case {value=node; _} = *)
(*   let pattern, arrow, instruction = node in *)
(*   s_pattern pattern; *)
(*   s_token arrow "->"; *)
(*   s_instruction instruction *)

(* and s_ass_instr {value=node; _} = *)
(*   let variable, ass, expr = node in *)
(*   s_var variable; *)
(*   s_token ass ":="; *)
(*   s_expr expr *)

(* and s_loop = function *)
(*   While while_loop -> s_while_loop while_loop *)
(* | For     for_loop -> s_for_loop for_loop *)

(* and s_while_loop {value=node; _} = *)
(*   let kwd_while, expr, block = node in *)
(*   s_token kwd_while "while"; *)
(*   s_expr expr; *)
(*   s_block block *)

(* and s_for_loop = function *)
(*   ForInt     for_int     -> s_for_int for_int *)
(* | ForCollect for_collect -> s_for_collect for_collect *)

(* and s_for_int ({value={kwd_for;ass;down;kwd_to;bound;step;block}; region} : for_int reg) = *)


(* and s_down = function *)
(*   Some kwd_down -> s_token kwd_down "down" *)
(* | None -> () *)

(* and s_step = function *)
(*   Some (kwd_step, expr) -> *)
(*     s_token kwd_step "step"; *)
(*     s_expr expr *)
(* | None -> () *)

(* and s_for_collect ({value=node; _} : for_collect reg) = *)
(*   s_token   node.kwd_for "for"; *)
(*   s_var     node.var; *)
(*   s_bind_to node.bind_to; *)
(*   s_token   node.kwd_in "in"; *)
(*   s_expr    node.expr; *)
(*   s_block   node.block *)

(* and s_bind_to = function *)
(*   Some (arrow, variable) -> *)
(*     s_token arrow "->"; *)
(*     s_var   variable *)
(* | None -> () *)

(* and s_expr = function *)
(*   Or {value = expr1, bool_or, expr2; _} -> *)
(*     s_expr expr1; s_token bool_or "||"; s_expr expr2 *)
(* | And {value = expr1, bool_and, expr2; _} -> *)
(*     s_expr expr1; s_token bool_and "&&"; s_expr expr2 *)
(* | Lt {value = expr1, lt, expr2; _} -> *)
(*     s_expr expr1; s_token lt "<"; s_expr expr2 *)
(* | Leq {value = expr1, leq, expr2; _} -> *)
(*     s_expr expr1; s_token leq "<="; s_expr expr2 *)
(* | Gt {value = expr1, gt, expr2; _} -> *)
(*     s_expr expr1; s_token gt ">"; s_expr expr2 *)
(* | Geq {value = expr1, geq, expr2; _} -> *)
(*     s_expr expr1; s_token geq ">="; s_expr expr2 *)
(* | Equal {value = expr1, equal, expr2; _} -> *)
(*     s_expr expr1; s_token equal "="; s_expr expr2 *)
(* | Neq {value = expr1, neq, expr2; _} -> *)
(*     s_expr expr1; s_token neq "=/="; s_expr expr2 *)
(* | Cat {value = expr1, cat, expr2; _} -> *)
(*     s_expr expr1; s_token cat "^"; s_expr expr2 *)
(* | Cons {value = expr1, cons, expr2; _} -> *)
(*     s_expr expr1; s_token cons "<:"; s_expr expr2 *)
(* | Add {value = expr1, add, expr2; _} -> *)
(*     s_expr expr1; s_token add "+"; s_expr expr2 *)
(* | Sub {value = expr1, sub, expr2; _} -> *)
(*     s_expr expr1; s_token sub "-"; s_expr expr2 *)
(* | Mult {value = expr1, mult, expr2; _} -> *)
(*     s_expr expr1; s_token mult "*"; s_expr expr2 *)
(* | Div {value = expr1, div, expr2; _} -> *)
(*     s_expr expr1; s_token div "/"; s_expr expr2 *)
(* | Mod {value = expr1, kwd_mod, expr2; _} -> *)
(*     s_expr expr1; s_token kwd_mod "mod"; s_expr expr2 *)
(* | Neg {value = minus, expr; _} -> *)
(*     s_token minus "-"; s_expr expr *)
(* | Not {value = kwd_not, expr; _} -> *)
(*     s_token kwd_not "not"; s_expr expr *)
(* | Int i            -> s_int i *)
(* | Var var          -> s_var var *)
(* | String s         -> s_string s *)
(* | Bytes b          -> s_bytes b *)
(* | False region     -> s_token region "False" *)
(* | True region      -> s_token region "True" *)
(* | Unit region      -> s_token region "Unit" *)
(* | Tuple tuple      -> s_tuple tuple *)
(* | List list        -> s_list list *)
(* | EmptyList elist  -> s_empty_list elist *)
(* | Set set          -> s_set set *)
(* | EmptySet eset    -> s_empty_set eset *)
(* | NoneExpr nexpr   -> s_none_expr nexpr *)
(* | FunCall fun_call -> s_fun_call fun_call *)
(* | ConstrApp capp   -> s_constr_app capp *)
(* | SomeApp sapp     -> s_some_app sapp *)
(* | MapLookUp lookup -> s_map_lookup lookup *)
(* | ParExpr pexpr    -> s_par_expr pexpr *)

(* and s_tuple {value=node; _} = *)
(*   let lpar, sequence, rpar = node in *)
(*   s_token lpar "("; *)
(*   s_nsepseq "," s_expr sequence; *)
(*   s_token rpar ")" *)

(* and s_list {value=node; _} = *)
(*   let lbra, sequence, rbra = node in *)
(*   s_token lbra "["; *)
(*   s_nsepseq "," s_expr sequence; *)
(*   s_token rbra "]" *)

(* and s_empty_list {value=node; _} = *)
(*   let lpar, (lbracket, rbracket, colon, type_expr), rpar = node in *)
(*   s_token     lpar "("; *)
(*   s_token     lbracket "["; *)
(*   s_token     rbracket "]"; *)
(*   s_token     colon ":"; *)
(*   s_type_expr type_expr; *)
(*   s_token     rpar ")" *)

(* and s_set {value=node; _} = *)
(*   let lbrace, sequence, rbrace = node in *)
(*   s_token lbrace "{"; *)
(*   s_nsepseq "," s_expr sequence; *)
(*   s_token rbrace "}" *)

(* and s_empty_set {value=node; _} = *)
(*   let lpar, (lbrace, rbrace, colon, type_expr), rpar = node in *)
(*   s_token     lpar "("; *)
(*   s_token     lbrace "{"; *)
(*   s_token     rbrace "}"; *)
(*   s_token     colon ":"; *)
(*   s_type_expr type_expr; *)
(*   s_token     rpar ")" *)

(* and s_none_expr {value=node; _} = *)
(*   let lpar, (c_None, colon, type_expr), rpar = node in *)
(*   s_token     lpar "("; *)
(*   s_token     c_None "None"; *)
(*   s_token     colon ":"; *)
(*   s_type_expr type_expr; *)
(*   s_token     rpar ")" *)

(* and s_fun_call {value=node; _} = *)
(*   let fun_name, arguments = node in *)
(*   s_var   fun_name; *)
(*   s_tuple arguments *)

(* and s_constr_app {value=node; _} = *)
(*   let constr, arguments = node in *)
(*   s_constr constr; *)
(*   s_tuple  arguments *)

(* and s_some_app {value=node; _} = *)
(*   let c_Some, arguments = node in *)
(*   s_token c_Some "Some"; *)
(*   s_tuple arguments *)

(* and s_map_lookup {value=node; _} = *)
(*   let {value = lbracket, expr, rbracket; _} = node.index in *)
(*   s_var   node.map_name; *)
(*   s_token node.selector "."; *)
(*   s_token lbracket "["; *)
(*   s_expr  expr; *)
(*   s_token rbracket "]" *)

(* and s_par_expr {value=node; _} = *)
(*   let lpar, expr, rpar = node in *)
(*   s_token lpar "("; *)
(*   s_expr  expr; *)
(*   s_token rpar ")" *)

(* and s_pattern {value=sequence; _} = *)
(*   s_nsepseq "<:" s_core_pattern sequence *)

(* and s_core_pattern = function *)
(*   PVar var      -> s_var var *)
(* | PWild wild    -> s_token wild "_" *)
(* | PInt i        -> s_int i *)
(* | PBytes b      -> s_bytes b *)
(* | PString s     -> s_string s *)
(* | PUnit region  -> s_token region "Unit" *)
(* | PFalse region -> s_token region "False" *)
(* | PTrue region  -> s_token region "True" *)
(* | PNone region  -> s_token region "None" *)
(* | PSome psome   -> s_psome psome *)
(* | PList pattern -> s_list_pattern pattern *)
(* | PTuple ptuple -> s_ptuple ptuple *)

(* and s_psome {value=node; _} = *)
(*   let c_Some, patterns = node in *)
(*   s_token    c_Some "Some"; *)
(*   s_patterns patterns *)

(* and s_patterns {value=node; _} = *)
(*   let lpar, core_pattern, rpar = node in *)
(*   s_token lpar "("; *)
(*   s_core_pattern core_pattern; *)
(*   s_token rpar ")" *)

(* and s_list_pattern = function *)
(*   Sugar sugar -> s_sugar sugar *)
(* | Raw     raw -> s_raw raw *)

(* and s_sugar {value=node; _} = *)
(*   let lbracket, sequence, rbracket = node in *)
(*   s_token lbracket "["; *)
(*   s_sepseq "," s_core_pattern sequence; *)
(*   s_token rbracket "]" *)

(* and s_raw {value=node; _} = *)
(*   let lpar, (core_pattern, cons, pattern), rpar = node in *)
(*   s_token        lpar "("; *)
(*   s_core_pattern core_pattern; *)
(*   s_token        cons "<:"; *)
(*   s_pattern      pattern; *)
(*   s_token        rpar ")" *)

(* and s_ptuple {value=node; _} = *)
(*   let lpar, sequence, rpar = node in *)
(*   s_token lpar "("; *)
(*   s_nsepseq "," s_core_pattern sequence; *)
(*   s_token rpar ")" *)

(* and s_terminator = function *)
(*   Some semi -> s_token semi ";" *)
(* | None -> () *)
