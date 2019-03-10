[@@@warning "-30"]

exception TODO of string

module I = AST

open Region

module SMap = Map.Make(String)

module O = struct
  type type_name = string
  type var_name = string
  type ast = {
      types        : type_decl list;
      storage      : typed_var;
      operations   : typed_var;
      declarations : decl list;
      prev         : I.ast;
    }
  and typed_var = { name:var_name; ty:type_expr }
  and type_decl = { name:string; ty:type_expr }
  and decl = { name:var_name; ty:type_expr; value: expr }
  and type_expr =
    Prod     of type_expr list
  | Sum      of (type_name * type_expr) list
  | Record   of (type_name * type_expr) list
  | TypeApp  of type_name * (type_expr list)
  | Function of { args: type_expr list; ret: type_expr }
  | Ref      of type_expr
  | Unit
  | Int
  | TODO
  and expr =
    App      of { operator: operator; arguments: expr list }
  | Var of var_name
  | Constant of constant
  | Lambda   of {
      parameters:   type_expr SMap.t;
      declarations: decl list;
      instructions: instr list;
      result:       expr;
    }
  and operator =
    Or | And | Lt | Leq | Gt | Geq | Equal | Neq | Cat | Cons | Add | Sub | Mult | Div | Mod
    | Neg | Not
    | Function of string
  and constant =
    Unit | Int of Z.t | String of string | Bytes of MBytes.t | False | True
  and instr =
  | Assignment    of { name: var_name; value: expr }
  | While         of { condition: expr; body: instr list }
  | ForCollection of { list: expr; key: var_name; value: var_name option; body: instr list }
  | If            of { condition: expr; ifso: instr list; ifnot: instr list }
  | Match         of { expr: expr; cases: (pattern * instr list) list }
  | DropUnit      of expr       (* expr returns unit, drop the result. *)
  | Fail          of { expr: expr }
  and pattern =
    PVar    of var_name
  | PWild
  | PInt    of Z.t
  | PBytes  of MBytes.t
  | PString of string
  | PUnit
  | PFalse
  | PTrue
  | PNone
  | PSome   of pattern
  | Cons    of pattern * pattern
  | Null
  | PTuple  of pattern list
end

(* open Sanity: *)
let (|>) v f = f v       (* pipe f to v *)
let (@@) f v = f v       (* apply f on v *)
let (@.) f g x = f (g x) (* compose *)
let map f l = List.rev (List.rev_map f l)
(* TODO: check that List.to_seq, List.append and SMap.of_seq are not broken
   (i.e. check that they are tail-recursive) *)
let append_map f l = map f l |>  List.flatten
let append l1 l2 = List.append l1 l2
let list_to_map l = l |> List.to_seq |> SMap.of_seq
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

let s_name {value=name; region} : O.var_name =
  let () = ignore (region) in
  name

let rec s_cartesian {value=sequence; region} : O.type_expr =
  let () = ignore (region) in
  Prod (map s_type_expr (s_nsepseq sequence))

and s_sum_type {value=sequence; region} : O.type_expr =
  let () = ignore (region) in
  Sum (map s_variant (s_nsepseq sequence))

and s_variant {value=(constr, kwd_of, cartesian); region} =
  let () = ignore (kwd_of,region) in
  (s_name constr, s_cartesian cartesian)

and s_record_type {value=(kwd_record, field_decls, kwd_end); region} : O.type_expr =
  let () = ignore (kwd_record,region,kwd_end) in
  Record (map s_field_decl (s_nsepseq field_decls))

and s_field_decl {value=(var, colon, type_expr); region} =
  let () = ignore (colon,region) in
  (s_name var, s_type_expr type_expr)

and s_type_app {value=(type_name,type_tuple); region} : O.type_expr =
  let () = ignore (region) in
  TypeApp (s_name type_name, s_type_tuple type_tuple)

and s_type_tuple ({value=(lpar, sequence, rpar); region} : (I.type_name, I.comma) Utils.nsepseq I.par) : O.type_expr list =
  let () = ignore (lpar,rpar,region) in
  (* TODO: the grammar should allow any type expr, not just type_name in the tuple elements *)
  map s_type_expr (map (fun a -> I.TAlias a) (s_nsepseq sequence))

and s_par_type {value=(lpar, type_expr, rpar); region} : O.type_expr =
  let () = ignore (lpar,rpar,region) in
  s_type_expr type_expr

and s_type_alias name : O.type_expr =
  let () = ignore () in
  TypeApp (s_name name, [])

and s_type_expr : I.type_expr -> O.type_expr = function
  Prod    cartesian   -> s_cartesian   cartesian
| Sum     sum_type    -> s_sum_type    sum_type
| Record  record_type -> s_record_type record_type
| TypeApp type_app    -> s_type_app    type_app
| ParType par_type    -> s_par_type    par_type
| TAlias  type_alias  -> s_type_alias  type_alias


let s_type_decl I.{value={kwd_type;name;kwd_is;type_expr;terminator}; region} : O.type_decl =
  let () = ignore (kwd_type,kwd_is,terminator,region) in
  O.{ name = s_name name; ty = s_type_expr type_expr }

let s_storage_decl I.{value={kwd_storage; name; colon; store_type; terminator}; region} : O.typed_var =
  let () = ignore (kwd_storage,colon,terminator,region) in
  O.{ name = s_name name; ty = s_type_expr store_type }

let s_operations_decl I.{value={kwd_operations;name;colon;op_type;terminator}; region} : O.typed_var =
  let () = ignore (kwd_operations,colon,terminator,region) in
  O.{ name = s_name name; ty = s_type_expr op_type }

let rec bin l operator r = O.App { operator; arguments = [s_expr l; s_expr r] }
and     una operator v = O.App { operator; arguments = [s_expr v] }
and s_expr : I.expr -> O.expr =
  function
    Or        {value=(l, bool_or, r);     region} -> let () = ignore (region, bool_or) in bin l Or    r
  | And       {value=(l, bool_and, r);    region} -> let () = ignore (region,bool_and) in bin l And   r
  | Lt        {value=(l, lt, r);          region} -> let () = ignore (region,      lt) in bin l Lt    r
  | Leq       {value=(l, leq, r);         region} -> let () = ignore (region,     leq) in bin l Leq   r
  | Gt        {value=(l, gt, r);          region} -> let () = ignore (region,      gt) in bin l Gt    r
  | Geq       {value=(l, geq, r);         region} -> let () = ignore (region,     geq) in bin l Geq   r
  | Equal     {value=(l, equal, r);       region} -> let () = ignore (region,   equal) in bin l Equal r
  | Neq       {value=(l, neq, r);         region} -> let () = ignore (region,     neq) in bin l Neq   r
  | Cat       {value=(l, cat, r);         region} -> let () = ignore (region,     cat) in bin l Cat   r
  | Cons      {value=(l, cons, r);        region} -> let () = ignore (region,    cons) in bin l Cons  r
  | Add       {value=(l, plus, r);        region} -> let () = ignore (region,    plus) in bin l Add   r
  | Sub       {value=(l, minus, r);       region} -> let () = ignore (region,   minus) in bin l Sub   r
  | Mult      {value=(l, times, r);       region} -> let () = ignore (region,   times) in bin l Mult  r
  | Div       {value=(l, slash, r);       region} -> let () = ignore (region,   slash) in bin l Div   r
  | Mod       {value=(l, kwd_mod, r);     region} -> let () = ignore (region, kwd_mod) in bin l Mod   r
  | Neg       {value=(minus, expr);       region} -> let () = ignore (region,   minus) in una Neg expr
  | Not       {value=(kwd_not, expr);     region} -> let () = ignore (region, kwd_not) in una Not expr
  | Int       {value=(lexeme, z);         region} -> let () = ignore (region,  lexeme) in Constant (Int z)
  | Var       {value=lexeme;              region} -> let () = ignore (region)          in Var lexeme
  | String    {value=s;                   region} -> let () = ignore (region)          in Constant (String s)
  | Bytes     {value=(lexeme, mbytes);    region} -> let () = ignore (region,  lexeme) in Constant (Bytes mbytes)
  | False     c_False                             -> let () = ignore (c_False)         in Constant (False)
  | True      c_True                              -> let () = ignore (c_True)          in Constant (True)
  | Unit      c_Unit                              -> let () = ignore (c_Unit)          in Constant (Unit)
  | Tuple     tuple                               -> let _todo = tuple in raise (TODO "simplify tuple")
  | List      {value=(lbrkt,lst,rbrkt);   region} -> let () = ignore (lbrkt,rbrkt,region) in let _todo = lst in raise (TODO "simplify (expr,comma) list")
  | EmptyList empty_list                          -> let _todo = empty_list in raise (TODO "simplify (lbracket,rbracket,colon,type_expr) par")
  | Set       set                                 -> let _todo = set        in raise (TODO "simplify (expr, comma) nsepseq braces")
  | EmptySet  empty_set                           -> let _todo = empty_set  in raise (TODO "simplify empty_set")
  | NoneExpr  none_expr                           -> let _todo = none_expr  in raise (TODO "simplify (c_None,colon,type_expr) par")
  | FunCall   fun_call                            -> let _todo = fun_call   in raise (TODO "simplify FunCall")
  | ConstrApp constr_app                          -> let _todo = constr_app in raise (TODO "simplify ConstrApp")
  | SomeApp   {value=(c_Some, arguments); region} -> let _todo = arguments  in let () = ignore (region,c_Some) in raise (TODO "simplify SomeApp")
  | MapLookUp {value=map_lookup;          region} -> let _todo = map_lookup in let () = ignore (region) in raise (TODO "simplify MapLookUp")
  | ParExpr   {value=(lpar,expr,rpar);    region} -> let () = ignore (lpar,rpar,region) in s_expr expr

let s_case : I.case -> O.pattern * (O.instr list) = function
  | _ -> raise (TODO "simplify pattern matching cases")

let s_const_decl I.{value={kwd_const;name;colon;const_type;equal;init;terminator}; region} : O.decl =
  let () = ignore (kwd_const,colon,equal,terminator,region) in
  O.{ name = s_name name; ty = s_type_expr const_type; value = s_expr init }

let s_param_const {value=(kwd_const,variable,colon,type_expr); region} : string * O.type_expr =
  let () = ignore (kwd_const,colon,region) in
  s_name variable, s_type_expr type_expr

let s_param_var {value=(kwd_var,variable,colon,type_expr); region} : string * O.type_expr =
  let () = ignore (kwd_var,colon,region) in
  s_name variable, s_type_expr type_expr

let s_param_decl : I.param_decl -> string * O.type_expr = function
    ParamConst p ->  s_param_const p
  | ParamVar p ->    s_param_var p

let s_parameters ({value=(lpar,param_decl,rpar);region} : I.parameters) : (string * O.type_expr) list =
  let () = ignore (lpar,rpar,region) in
  let l = (s_nsepseq param_decl) in
  map s_param_decl l

let rec s_var_decl I.{value={kwd_var;name;colon;var_type;ass;init;terminator}; region} : O.decl =
  let () = ignore (kwd_var,colon,ass,terminator,region) in
  O.{
      name = s_name name;
      ty = s_type_expr var_type;
      value = s_expr init
  }

and s_local_decl : I.local_decl -> O.decl = function
  LocalLam   decl -> s_lambda_decl decl
| LocalConst decl -> s_const_decl  decl
| LocalVar   decl -> s_var_decl    decl

and s_instructions ({value=sequence; region} : I.instructions) : O.instr list =
  let () = ignore (region) in
  append_map s_instruction (s_nsepseq sequence)

and s_instruction : I.instruction -> O.instr list = function
  Single instr -> s_single_instr instr
|  Block block -> (s_block block)

and s_conditional I.{kwd_if;test;kwd_then;ifso;kwd_else;ifnot} : O.instr =
  let () = ignore (kwd_if,kwd_then,kwd_else) in
  If { condition = s_expr test; ifso = s_instruction ifso; ifnot = s_instruction ifnot }

and s_match_instr I.{kwd_match;expr;kwd_with;lead_vbar;cases;kwd_end} : O.instr =
  let {value=cases;region} = cases in
  let () = ignore (kwd_match,kwd_with,lead_vbar,kwd_end,region) in
  Match { expr = s_expr expr; cases = map s_case (s_nsepseq cases) }

and s_ass_instr {value=(variable,ass,expr); region} : O.instr =
  let () = ignore (ass,region) in
  Assignment { name = s_name variable; value = s_expr expr }

and s_while_loop {value=(kwd_while, expr, block); region} : O.instr list =
  let () = ignore (kwd_while,region) in
  [While {condition = s_expr expr; body = s_block block}]

and s_for_loop : I.for_loop -> O.instr list = function
  ForInt     for_int     -> s_for_int     for_int
| ForCollect for_collect -> s_for_collect for_collect

and s_for_int ({value={kwd_for;ass;down;kwd_to;bound;step;block}; region} : I.for_int reg) : O.instr list =
  let {value=(variable,ass_kwd,expr);region = ass_region} = ass in
  let () = ignore (kwd_for,ass_region,ass_kwd,kwd_to,region) in
  let name = s_name variable in
  let condition, operator = match down with Some kwd_down -> ignore kwd_down; O.Gt, O.Sub
                                          | None          ->                  O.Lt, O.Add in
  let step = s_step step
  in [
      Assignment { name; value = s_expr expr };
      (* TODO: lift the declaration of the variable *)
      While {
          condition = App { operator = condition;
                            arguments = [Var name; s_expr bound] };
          body = append (s_block block)
                        [O.Assignment { name;
                                        value = App { operator;
                                                      arguments = [Var name; step]}}]
        }
    ]

and s_for_collect ({value={kwd_for;var;bind_to;kwd_in;expr;block}; _} : I.for_collect reg) : O.instr list =
  let () = ignore (kwd_for,kwd_in) in
  [
    O.ForCollection {
        list = s_expr expr;
        key = s_name var;
        value = s_bind_to bind_to;
        body = s_block block
      }
  ]

and s_step : (I.kwd_step * I.expr) option -> O.expr = function
  Some (kwd_step, expr) -> let () = ignore (kwd_step) in s_expr expr
| None -> Constant (Int (Z.of_int 1))

and s_bind_to : (I.arrow * I.variable) option -> O.var_name option = function
    Some (arrow, variable) -> let () = ignore (arrow) in Some (s_name variable)
  | None -> None

and s_loop : I.loop -> O.instr list = function
    While while_loop -> s_while_loop while_loop
  | For     for_loop -> s_for_loop for_loop

and s_fun_call {value=(fun_name, arguments); region} : O.expr =
  let () = ignore (region) in
  App { operator = Function (s_name fun_name); arguments = s_arguments arguments }

and s_arguments {value=(lpar, sequence, rpar); region} =
  let () = ignore (lpar,rpar,region) in
  map s_expr (s_nsepseq sequence);

and s_fail ((kwd_fail, expr) : (I.kwd_fail * I.expr)) : O.instr =
  let () = ignore (kwd_fail) in
  Fail { expr = s_expr expr }




and s_single_instr : I.single_instr -> O.instr list = function
  Cond     {value; _} -> [s_conditional value]
| Match    {value; _} -> [s_match_instr value]
| Ass      instr      -> [s_ass_instr instr]
| Loop     loop       -> s_loop loop
| ProcCall fun_call   -> [DropUnit (s_fun_call fun_call)]
| Null     kwd_null   -> let () = ignore (kwd_null) in
                         []
| Fail     {value; _} -> [s_fail value]

and s_block I.{value={opening;instr;terminator;close}; _} : O.instr list =
  let () = ignore (opening,terminator,close) in
  s_instructions instr

and s_fun_decl I.{value={kwd_function;name;param;colon;ret_type;kwd_is;local_decls;block;kwd_with;return;terminator}; region} : O.decl =
  let () = ignore (kwd_function,colon,kwd_is,kwd_with,terminator,region) in
  O.{
      name = s_name name;
      ty = Function { args = map snd (s_parameters param); ret = s_type_expr ret_type };
      value = Lambda {
                  parameters   = s_parameters param |> list_to_map;
                  declarations = map s_local_decl local_decls;
                  instructions = s_block block;
                  result       = s_expr return
                }
  }

and s_proc_decl I.{value={kwd_procedure;name;param;kwd_is;local_decls;block;terminator}; region} =
  let () = ignore (kwd_procedure,kwd_is,terminator,region) in
  O.{
      name = s_name name;
      ty = Function { args = map snd (s_parameters param); ret = Unit };
      value = Lambda {
                  parameters   = s_parameters param |> list_to_map;
                  declarations = map s_local_decl local_decls;
                  instructions = s_block block;
                  result       = O.Constant O.Unit
                }
  }

and s_entry_decl I.{value={kwd_entrypoint;name;param;kwd_is;local_decls;block;terminator}; region} =
  let () = ignore (kwd_entrypoint,kwd_is,terminator,region) in
  O.{
      name = s_name name;
      ty = Function { args = map snd (s_parameters param); ret = Unit };
      value = Lambda {
                  parameters   = s_parameters param |> list_to_map;
                  declarations = map s_local_decl local_decls;
                  instructions = s_block block;
                  result       = O.Constant O.Unit
                }
  }

and s_lambda_decl : I.lambda_decl -> O.decl = function
  FunDecl     fun_decl -> s_fun_decl fun_decl
| EntryDecl entry_decl -> s_entry_decl entry_decl
| ProcDecl   proc_decl -> s_proc_decl proc_decl

let s_main_block (block: I.block reg) : O.decl =
  O.{
      name = "main";
      ty = Function { args = []; ret = Unit };
      value = Lambda {
                  parameters   = SMap.empty;
                  declarations = [];
                  instructions = s_block block;
                  result       = O.Constant O.Unit
                }
  }

let s_ast (ast : I.ast) : O.ast =
  let I.{types;constants;storage;operations;lambdas;block;eof} = ast in
  let () = ignore (eof) in
  O.{
      types        = map s_type_decl types;
      storage      = s_storage_decl storage;
      operations   = s_operations_decl operations;
      declarations = List.flatten [(map s_const_decl  constants);
                                   (map s_lambda_decl lambdas);
                                   [s_main_block block]];
      prev         = ast
  }





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


(* and s_parameters {value=node; _} = *)
(*   let lpar, sequence, rpar = node in *)
(*   s_token lpar "("; *)
(*   s_nsepseq ";" s_param_decl sequence; *)
(*   s_token rpar ")" *)

(* and s_param_decl = function *)
(*   ParamConst param_const -> s_param_const param_const *)
(* | ParamVar   param_var   -> s_param_var   param_var *)

(* and s_region_cases {value=sequence; _} = *)
(*   s_nsepseq "|" s_case sequence *)

(* and s_case {value=node; _} = *)
(*   let pattern, arrow, instruction = node in *)
(*   s_pattern pattern; *)
(*   s_token arrow "->"; *)
(*   s_instruction instruction *)

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
