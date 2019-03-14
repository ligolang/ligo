[@@@warning "-30"]

module I = AST

open Region

module SMap = Map.Make(String)

module O = struct
  type asttodo = [`TODO]

  type name_and_region = {name: string; orig: Region.t}
  type type_name  = name_and_region
  type var_name   = name_and_region
  type field_name = name_and_region

  type record_key = [`Field of field_name | `Component of int]

  type pattern =
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
  | PCons   of pattern * pattern
  | PNull
  | PRecord of record_key precord

  and 'key precord = ('key * pattern) list

  type type_constructor =
    Option
  | List
  | Set
  | Map

  type type_expr_case =
    Sum      of (type_name * type_expr) list
  | Record   of record_key type_record
  | TypeApp  of type_constructor * (type_expr list)
  | Function of { arg: type_expr; ret: type_expr }
  | Ref      of type_expr
  | String
  | Int
  | Unit
  | Bool
  and 'key type_record = ('key * type_expr) list

  and type_expr = { type_expr: type_expr_case; name: type_name option; orig: Region.t }

  type typed_var = { name:var_name; ty:type_expr }

  type type_decl = { name:type_name; ty:type_expr }

  type expr =
    App      of { operator: operator; arguments: expr list }
  | Var      of var_name
  | Constant of constant
  | Lambda   of lambda

  and decl = { name:var_name; ty:type_expr; value: expr }

  and lambda = {
      parameter:    typed_var;
      declarations: decl list;
      instructions: instr list;
      result:       expr;
    }

  and operator =
      Function    of var_name
    | Construcor  of var_name
    | UpdateField of record_key
    | GetField    of record_key
    | Or | And | Lt | Leq | Gt | Geq | Equal | Neq | Cat | Cons | Add | Sub | Mult | Div | Mod
    | Neg | Not
    | Tuple | Set | List
    | MapLookup

  and constant =
    Unit | Int of Z.t | String of string | Bytes of MBytes.t | False | True
    | Null of type_expr | EmptySet of type_expr | CNone of type_expr

  and instr =
    Assignment    of { name: var_name; value: expr }
  | While         of { condition: expr; body: instr list }
  | ForCollection of { list: expr; key: var_name; value: var_name option; body: instr list }
  | If            of { condition: expr; ifso: instr list; ifnot: instr list }
  | Match         of { expr: expr; cases: (pattern * instr list) list }
  | DropUnit      of expr       (* expr returns unit, drop the result. *)
  | Fail          of { expr: expr }

  type ast = {
      types           : type_decl list;
      storage_decl    : typed_var;
      operations_decl : typed_var;
      declarations    : decl list;
    }
end

(* open Sanity: *)
let (|>) v f = f v       (* pipe f to v *)
let (@@) f v = f v       (* apply f on v *)
let (@.) f g x = f (g x) (* compose *)
let map f l = List.rev (List.rev_map f l)
let mapi f l =
  let f (i, l) elem =
    (i + 1, (f i elem) :: l)
  in snd (List.fold_left f (0,[]) l)
(* TODO: check that List.append is not broken
   (i.e. check that it is tail-recursive) *)
let append_map f l = map f l |>  List.flatten
let append l1 l2 = List.append l1 l2
let list_to_map l = List.fold_left (fun m (k,v) -> SMap.add k v m) SMap.empty l
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
  {name;orig = region}

let name_to_string {value=name; region} : string =
  let () = ignore (region) in
  name

let type_expr (orig : Region.t) (e : O.type_expr_case) : O.type_expr =
  { type_expr = e; name = None; orig }

let s_type_constructor {value=name;region} : O.type_constructor =
  let () = ignore (region) in
  match name with
     "Option" -> Option
   | "List"   -> List
   | "Map"    -> Map
   | "Set"    -> Set
   (* TODO: escape the name, prevent any \x1b and other weird characters from appearing in the output *)
   | _ -> failwith ("Unknown type constructor: " ^ name)

let rec s_cartesian {value=sequence; region} : O.type_expr =
  let () = ignore (region) in
  s_nsepseq sequence
  |>map s_type_expr
  |> mapi (fun i p -> `Component i, p)
  |> (fun x -> (Record x : O.type_expr_case))
  |> type_expr region

and s_sum_type {value=sequence; region} : O.type_expr =
  let () = ignore (region) in
  type_expr region (Sum (map s_variant (s_nsepseq sequence)))

and s_variant {value=(constr, kwd_of, cartesian); region} =
  let () = ignore (kwd_of,region) in
  (s_name constr, s_cartesian cartesian)

and s_record_type {value=(kwd_record, field_decls, kwd_end); region} : O.type_expr =
  let () = ignore (kwd_record,region,kwd_end) in
  type_expr region (Record (map s_field_decl (s_nsepseq field_decls)))

and s_field_decl {value=(var, colon, type_expr); region} =
  let () = ignore (colon,region) in
  (`Field (s_name var), s_type_expr type_expr)

and s_type_app {value=(type_name,type_tuple); region} : O.type_expr =
  let () = ignore (region) in
  type_expr region (TypeApp (s_type_constructor type_name, s_type_tuple type_tuple))

and s_type_tuple ({value=(lpar, sequence, rpar); region} : (I.type_name, I.comma) Utils.nsepseq I.par) : O.type_expr list =
  let () = ignore (lpar,rpar,region) in
  (* TODO: the grammar should allow any type expr, not just type_name in the tuple elements *)
  map s_type_expr (map (fun a -> I.TAlias a) (s_nsepseq sequence))

and s_par_type {value=(lpar, type_expr, rpar); region} : O.type_expr =
  let () = ignore (lpar,rpar,region) in
  s_type_expr type_expr

and s_type_alias name : O.type_expr =
  let () = ignore () in
  type_expr name.region (TypeApp (s_type_constructor name, []))

and s_type_expr (orig : I.type_expr) : O.type_expr = match orig with
  Prod    cartesian   -> s_cartesian   cartesian
| Sum     sum_type    -> s_sum_type    sum_type
| Record  record_type -> s_record_type record_type
| TypeApp type_app    -> s_type_app    type_app
| ParType par_type    -> s_par_type    par_type
| TAlias  type_alias  -> s_type_alias  type_alias


let s_type_decl I.{value={kwd_type;name;kwd_is;type_expr;terminator}; region} : O.type_decl =
  let () = ignore (kwd_type,kwd_is,terminator,region) in
  let ty = s_type_expr type_expr in
  O.{ name = s_name name; ty = { ty with name = Some (s_name name) } }

let s_storage_decl I.{value={kwd_storage; name; colon; store_type; terminator}; region} : O.typed_var =
  let () = ignore (kwd_storage,colon,terminator,region) in
  O.{ name = s_name name; ty = s_type_expr store_type }

let s_operations_decl I.{value={kwd_operations;name;colon;op_type;terminator}; region} : O.typed_var =
  let () = ignore (kwd_operations,colon,terminator,region) in
  O.{ name = s_name name; ty = s_type_expr op_type }

let s_empty_list {value=(l, (lbracket, rbracket, colon, type_expr), r); region} : O.expr =
  let () = ignore (l, lbracket, rbracket, colon, r, region) in
  Constant (Null (s_type_expr type_expr))

let s_empty_set {value=(l, (lbrace, rbrace, colon, type_expr), r); region} : O.expr =
  let () = ignore (l, lbrace, rbrace, colon, r, region) in
  Constant (EmptySet (s_type_expr type_expr))

let s_none {value=(l, (c_None, colon, type_expr), r); region} : O.expr =
  let () = ignore (l, c_None, colon, r, region) in
  Constant (CNone (s_type_expr type_expr))

let parameters_to_tuple (parameters : (string * O.type_expr) list) : O.type_expr =
  (* TODO: use records with named fields to have named arguments. *)
  let parameter_tuple = O.Record (mapi (fun i (_name,ty) -> `Component i, ty) parameters) in
  O.{ type_expr = parameter_tuple; name = None; orig = Region.ghost }
and parameters_to_decls singleparam (parameters : (string * O.type_expr) list) : O.decl list =
  let f i (name,ty) =
    O.{ name = {name; orig=Region.ghost};
        ty = ty;
        value = App { operator = O.GetField (`Component i);
                      arguments = [Var singleparam] } }
  in mapi f parameters

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
  | Var       lexeme                              ->                                      Var (s_name lexeme)
  | String    {value=s;                   region} -> let () = ignore (region)          in Constant (String s)
  | Bytes     {value=(lexeme, mbytes);    region} -> let () = ignore (region,  lexeme) in Constant (Bytes mbytes)
  | False     c_False                             -> let () = ignore (c_False)         in Constant (False)
  | True      c_True                              -> let () = ignore (c_True)          in Constant (True)
  | Unit      c_Unit                              -> let () = ignore (c_Unit)          in Constant (Unit)
  | Tuple     {value=(l,tuple,r);         region} -> let () = ignore (l,r,region)      in App { operator = Tuple; arguments = map s_expr (s_nsepseq tuple)}
  | List      list                                -> s_list list
  | EmptyList empty_list                          -> s_empty_list empty_list
  | Set       set                                 -> s_set set
  | EmptySet  empty_set                           -> s_empty_set empty_set
  | NoneExpr  none_expr                           -> s_none none_expr
  | FunCall   fun_call                            -> s_fun_call fun_call
  | ConstrApp constr_app                          -> s_constr_app constr_app
  | SomeApp   some_app                            -> s_some_app some_app
  | MapLookUp map_lookup                          -> s_map_lookup map_lookup
  | ParExpr   {value=(lpar,expr,rpar);    region} -> let () = ignore (lpar,rpar,region) in s_expr expr

and s_map_lookup I.{value = {map_name; selector; index}; region} : O.expr =
  let {value = lbracket, index_expr, rbracket; region=region2} = index in
  let () = ignore (selector, lbracket, rbracket, region2, region) in
  App { operator = MapLookup; arguments = [Var (s_name map_name); s_expr index_expr] }

and s_some_app {value=(c_Some, {value=(l,arguments,r); region=region2}); region} : O.expr =
  let () = ignore (c_Some,l,r,region2,region) in
  match s_nsepseq arguments with
    [] -> failwith "tuple cannot be empty"
  | [a] -> s_expr a
  | l -> App { operator = Tuple; arguments = map s_expr l }

and s_list {value=(l, list, r); region} : O.expr =
  let () = ignore (l, r, region) in
  App { operator = List; arguments = map s_expr (s_nsepseq list) }

and s_set {value=(l, set, r); region} : O.expr =
  let () = ignore (l, r, region) in
  App { operator = Set; arguments = map s_expr (s_nsepseq set) }

and s_pattern {value=sequence; region} : O.pattern =
  let () = ignore (region) in
  s_pattern_conses (s_nsepseq sequence)

and s_pattern_conses : I.core_pattern list -> O.pattern = function
    [] -> assert false
  | [p] -> s_core_pattern p
  | hd :: tl -> PCons (s_core_pattern hd, s_pattern_conses tl)

and s_case ({value=(pattern, arrow, instruction); region} : I.case) : O.pattern * O.instr list =
  let () = ignore (arrow,region) in
  s_pattern pattern, s_instruction instruction

and s_core_pattern : I.core_pattern -> O.pattern = function
  PVar var                     -> PVar (s_name var)
| PWild wild                   -> let () = ignore      (wild) in PWild
| PInt {value=(si,i);region}   -> let () = ignore (si,region) in PInt i
| PBytes {value=(sb,b);region} -> let () = ignore (sb,region) in PBytes b
| PString {value=s;region}     -> let () = ignore    (region) in PString s
| PUnit region                 -> let () = ignore    (region) in PUnit
| PFalse region                -> let () = ignore    (region) in PFalse
| PTrue region                 -> let () = ignore    (region) in PTrue
| PNone region                 -> let () = ignore    (region) in PNone
| PSome psome                  -> s_psome psome
| PList pattern                -> s_list_pattern pattern
| PTuple ptuple                -> s_ptuple ptuple

and s_list_pattern = function
  Sugar sugar -> s_sugar sugar
| Raw     raw -> s_raw raw

and s_sugar {value=(lbracket, sequence, rbracket); region} : O.pattern =
  let () = ignore (lbracket, rbracket, region) in
  List.fold_left (fun acc p -> O.PCons (s_core_pattern p, acc))
                 O.PNull
                 (s_sepseq sequence);

and s_raw {value=(lpar, (core_pattern, cons, pattern), rpar); region} =
  let () = ignore (lpar, cons, rpar, region) in
  O.PCons (s_core_pattern core_pattern, s_pattern pattern)

and s_ptuple {value=(lpar, sequence, rpar); region} =
  let () = ignore (lpar, rpar, region) in
  s_nsepseq sequence
  |> map s_core_pattern
  |> mapi (fun i p -> `Component i, p)
  |> fun x -> O.PRecord x

and s_psome {value=(c_Some,{value=(l,psome,r);region=region2});region} : O.pattern =
  let () = ignore    (c_Some,l,r,region2,region) in
  PSome (s_core_pattern psome)

and s_const_decl I.{value={kwd_const;name;colon;const_type;equal;init;terminator}; region} : O.decl =
  let () = ignore (kwd_const,colon,equal,terminator,region) in
  O.{ name = s_name name; ty = s_type_expr const_type; value = s_expr init }

and s_param_const {value=(kwd_const,variable,colon,type_expr); region} : string * O.type_expr =
  let () = ignore (kwd_const,colon,region) in
  name_to_string variable, s_type_expr type_expr

and s_param_var {value=(kwd_var,variable,colon,type_expr); region} : string * O.type_expr =
  let () = ignore (kwd_var,colon,region) in
  name_to_string variable, s_type_expr type_expr

and s_param_decl : I.param_decl -> string * O.type_expr = function
    ParamConst p ->  s_param_const p
  | ParamVar p ->    s_param_var p

and s_parameters ({value=(lpar,param_decl,rpar);region} : I.parameters) : (string * O.type_expr) list =
  let () = ignore (lpar,rpar,region) in
  let l = (s_nsepseq param_decl) in
  map s_param_decl l

and s_var_decl I.{value={kwd_var;name;colon;var_type;ass;init;terminator}; region} : O.decl =
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
      (* TODO: lift the declaration of the variable, to avoid creating a nested scope here. *)
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

and s_constr_app {value=(constr, arguments); region} : O.expr =
  let () = ignore (region) in
  App { operator = Function (s_name constr); arguments = s_arguments arguments }

and s_arguments {value=(lpar, sequence, rpar); region} : O.expr list =
  (* TODO: should return a tuple *)
  let () = ignore (lpar,rpar,region) in
  match map s_expr (s_nsepseq sequence) with
    []                -> [Constant Unit]
  | [single_argument] -> [single_argument]
  | args              -> [App { operator = Tuple; arguments = args }] ;

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

and gensym =
  let i = ref 0 in
  fun ty ->
  i := !i + 1;
  (* TODO: Region.ghost *)
  ({name = {name=(string_of_int !i) ^ "gensym"; orig = Region.ghost}; ty} : O.typed_var)

and s_fun_decl I.{value={kwd_function;name;param;colon;ret_type;kwd_is;local_decls;block;kwd_with;return;terminator}; region} : O.decl =
  let () = ignore (kwd_function,colon,kwd_is,kwd_with,terminator,region) in
  let tuple_type = s_parameters param |> parameters_to_tuple in
  let single_argument = gensym tuple_type in
  let ({name = single_argument_xxx; ty = _} : O.typed_var) = single_argument in
  O.{
      name = s_name name;
      ty = type_expr region (Function { arg = tuple_type;
                                        ret = s_type_expr ret_type });
      value = Lambda {
                  parameter    = single_argument;
                  declarations = append
                                   (s_parameters param |> parameters_to_decls single_argument_xxx)
                                   (map s_local_decl local_decls);
                  instructions = s_block block;
                  result       = s_expr return
                }
  }

and s_proc_decl I.{value={kwd_procedure;name;param;kwd_is;local_decls;block;terminator}; region} =
  let () = ignore (kwd_procedure,kwd_is,terminator,region) in
  let tuple_type = s_parameters param |> parameters_to_tuple in
  let single_argument = gensym tuple_type in
  let ({name = single_argument_xxx; ty = _} : O.typed_var) = single_argument in
  O.{
      name = s_name name;
      ty = type_expr region (Function { arg = tuple_type;
                                        ret = type_expr region Unit });
      value = Lambda {
                  parameter    = single_argument;
                  declarations = append
                                   (s_parameters param |> parameters_to_decls single_argument_xxx)
                                   (map s_local_decl local_decls);
                  instructions = s_block block;
                  result       = O.Constant O.Unit
                }
  }

and s_entry_decl I.{value={kwd_entrypoint;name;param;kwd_is;local_decls;block;terminator}; region} =
  let () = ignore (kwd_entrypoint,kwd_is,terminator,region) in
  let tuple_type = s_parameters param |> parameters_to_tuple in
  let single_argument = gensym tuple_type in
  let ({name = single_argument_xxx; ty = _} : O.typed_var) = single_argument in
  O.{
      name = s_name name;
      ty = type_expr region (Function { arg = tuple_type;
                                        ret = type_expr region Unit });
      value = Lambda {
                  parameter    = single_argument;
                  declarations = append
                                   (s_parameters param |> parameters_to_decls single_argument_xxx)
                                   (map s_local_decl local_decls);
                  instructions = s_block block;
                  result       = O.Constant O.Unit
                }
  }

and s_lambda_decl : I.lambda_decl -> O.decl = function
  FunDecl     fun_decl -> s_fun_decl fun_decl
| EntryDecl entry_decl -> s_entry_decl entry_decl
| ProcDecl   proc_decl -> s_proc_decl proc_decl

type tmp_ast = {
    types           : O.type_decl list;
    storage_decl    : O.typed_var option;
    operations_decl : O.typed_var option;
    declarations    : O.decl list;
  }


let s_declaration (ast : tmp_ast) : I.declaration -> tmp_ast = function
    TypeDecl    t -> { ast with types           = (s_type_decl    t) :: ast.types }
  | ConstDecl   c -> { ast with declarations    = (s_const_decl   c) :: ast.declarations }
  | StorageDecl s -> { ast with storage_decl    = Some (s_storage_decl    s) }
  | OpDecl      o -> { ast with operations_decl = Some (s_operations_decl o) }
  | LambdaDecl  l -> { ast with declarations    = (s_lambda_decl  l) :: ast.declarations }

let s_ast (ast : I.ast) : O.ast =
  let I.{decl=(decl1,decls);eof} = ast in
  let () = ignore (eof) in
  let {types; storage_decl; operations_decl; declarations} =
    List.fold_left s_declaration
                   { types = [];
                     storage_decl = None;
                     operations_decl = None;
                     declarations = [] }
                   ( decl1 :: decls ) in
  let storage_decl = match storage_decl with
      Some x -> x
    | None -> failwith "Missing storage declaration" in
  let operations_decl = match operations_decl with
      Some x -> x
    | None -> failwith "Missing storage declaration"
  in {types; storage_decl; operations_decl; declarations}




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


(* and s_par_expr {value=node; _} = *)
(*   let lpar, expr, rpar = node in *)
(*   s_token lpar "("; *)
(*   s_expr  expr; *)
(*   s_token rpar ")" *)

(* and s_psome {value=node; _} = *)
(*   let c_Some, patterns = node in *)
(*   s_token    c_Some "Some"; *)
(*   s_patterns patterns *)


(* and s_terminator = function *)
(*   Some semi -> s_token semi ";" *)
(* | None -> () *)
