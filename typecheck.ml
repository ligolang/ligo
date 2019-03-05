(*
module I = AST (* In *)

module SMap = Map.Make(String)

module O = struct
  open AST (* TODO: for now, should disappear *)

  type t = ast

   and type_expr =
     Prod     of cartesian
   | Sum      of (variant, vbar) Utils.nsepseq
   | Record   of record_type
   | TypeApp  of (type_name * type_tuple)
   | ParType  of type_expr par
   | TAlias   of variable
   | Function of (type_expr list) * type_expr
   | Mutable  of type_expr
   | Unit
   | TODO     of string

   and te = type_expr list SMap.t

   and ve = type_expr list SMap.t

   and vte = ve * te

   and ast = {
       lambdas : lambda_decl list;
       block   : block
     }

   and lambda_decl =
     FunDecl of fun_decl
   | ProcDecl of proc_decl

   and fun_decl = {
       local_decls  : local_decls;
       kwd_function : kwd_function;
       name         : variable;
       param        : parameters;
       colon        : colon;
       ret_type     : type_expr;
       kwd_is       : kwd_is;
       body         : block;
       kwd_with     : kwd_with;
       return       : expr
     }

   and proc_decl = {
       kwd_procedure : kwd_procedure;
       name          : variable;
       param         : parameters;
       kwd_is        : kwd_is;
       local_decls   : local_decl list;
       block         : block reg
     }

   and block = {
       opening : kwd_begin;
       instr   : instructions;
       close   : kwd_end
     }

   and value_decls = var_decl list

   and var_decl = {
       kwd_var : kwd_var;
       name    : variable;
       colon   : colon;
       vtype   : type_expr;
       asgnmnt : Region.t;  (* "=" or ":=" *)
       init    : expr
     }

   and expr = {ty:type_expr;expr:expr}
end [@warning "-30"]

open O
open AST
open Region

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

let unreg : 'a reg -> 'a = fun {value; _} -> value
let unpar : 'a par -> 'a = (fun (_left_par, x, _right_par) -> x) @. unreg
let nsepseq_to_list : ('a,'sep) Utils.nsepseq -> 'a list =
  fun (first, rest) -> first :: (map snd rest)
let sepseq_to_list : ('a,'sep) Utils.sepseq -> 'a list =
  function
    None         -> []
  | Some nsepseq -> nsepseq_to_list nsepseq

let rec xty : I.type_expr -> O.type_expr =
  function
    I.Prod x                         -> O.Prod x
  | I.Sum x                          -> O.Sum (unreg x)
  | I.Record x                       -> O.Record x
  | I.TypeApp x                      -> O.TypeApp (unreg x)
  | I.ParType {region;value=(l,x,r)} -> O.ParType {region;value=(l, xty x, r)}
  | I.TAlias x                       -> O.TAlias x

let shadow (name : string) (typ : O.type_expr) (env : O.type_expr list SMap.t)
    : O.type_expr list SMap.t =
  SMap.update name (function None -> Some [typ] | Some tl -> Some (typ :: tl)) env

let shadow_list  (name_typ_list : (string * O.type_expr) list) (env : O.type_expr list SMap.t)
    : O.type_expr list SMap.t =
  List.fold_left (fun  acc (name, typ) -> shadow name typ acc) env name_typ_list

let type_decls_to_tenv (td : I.type_decl list) (te : te) : O.te =
  td
  |> List.map unreg
  |> List.map (fun (_, name, _, type_expr) -> (unreg name, xty type_expr))
  |> fun up -> shadow_list up te

let param_const_to_xty : 'todo -> O.type_expr = function
  (_kwd_const, _variable, _colon, type_expr) -> O.Mutable (xty type_expr)

let param_var_to_xty : 'todo -> O.type_expr = function
  (_kwd_var, _variable, _colon, type_expr) -> xty type_expr

let param_decl_to_xty : I.param_decl -> 'todo2 = function
    ParamConst pc -> pc |> unreg |> param_const_to_xty
  | ParamVar pv -> pv |> unreg |> param_var_to_xty

let params_to_xty (params : I.parameters) ret_type =
  unpar params
  |> nsepseq_to_list
  |> map param_decl_to_xty
  |> fun param_types -> O.Function (param_types, ret_type)

let type_equal t1 t2 = match t1,t2 with
  | O.Prod _x, O.Prod _y -> true (* TODO *)
  | O.Sum  _x, O.Sum  _y -> true (* TODO *)
  | _                    -> false

exception TypeError of string

let check_type expr expected_type =
  if type_equal expr.ty expected_type then expr
  else raise (TypeError "oops")

let tc_expr (_te,_ve) (expr : I.expr) (expected:O.type_expr) : O.expr = {ty=(TODO "all expressions");expr} (* TODO *)

let tc_var_decl : vte -> I.var_decl -> vte * O.var_decl =
  fun (ve,te) {kwd_var;name;colon;vtype;asgnmnt;init} ->
  let vtype = (xty vtype) in
  let init = tc_expr (ve,te) init vtype in
  let ve,te = shadow (unreg name) vtype ve, te in
  (ve,te), {kwd_var;name;colon;vtype;asgnmnt;init}

let tc_var_decls (ve,te) var_decls = fold_map tc_var_decl (ve,te) var_decls

let tc_block (te, ve : vte) (block : I.block) : vte * O.block =
  let opening,instr,close = block.opening, block.instr, block.close in
  (ve,te), O.{opening;instr;close} (* TODO *)

let tc_local_decl : I.local_decl -> 'todo =
  `TODO

let tc_proc_decl : vte -> I.proc_decl -> vte*O.proc_decl =
  fun vte {kwd_procedure;name;param;kwd_is;local_decls;block} ->
  let vte, local_decls = tc_var_decls vte (local_decls |> map tc_local_decl) in
  let vte, block = tc_block vte (unreg block)
  in vte,{kwd_procedure;name;param;kwd_is;local_decls;block}

let tc_fun_decl : vte -> I.fun_decl -> O.fun_decl =
  fun vte fun_decl ->
  let vte', block'    = tc_block vte (unreg fun_decl.body) in
  let return'         = tc_expr vte' fun_decl.return in
  let checked_return' = check_type return' (xty fun_decl.ret_type)
  in mk_fun_decl
  ~kwd_function: fun_decl.kwd_function
  ~colon:    fun_decl.colon
  ~kwd_is:   fun_decl.kwd_is
  ~kwd_with: fun_decl.kwd_with
  ~var:      fun_decl.var
  ~param:    fun_decl.param
  ~ret_type: (xty fun_decl.ret_type)
  ~body:     block'
  ~return:   checked_return'

let ve_lambda_decl : vte -> I.lambda_decl -> ve =
  fun (ve,_te) ->
  function
    FunDecl {value;_} -> shadow value.name.value (params_to_xty value.param (xty value.ret_type)) ve
  | ProcDecl {value;_} -> shadow value.name.value (params_to_xty value.param Unit) ve

let tc_lambda_decl (ve, te : vte) (whole : I.lambda_decl) : vte * O.lambda_decl =
  match whole with
    FunDecl  {value;_} -> ((ve_lambda_decl (ve, te) whole), te), O.FunDecl  (tc_fun_decl  (ve, te) value)
  | ProcDecl {value;_} -> ((ve_lambda_decl (ve, te) whole), te), O.ProcDecl (tc_proc_decl (ve, te) value)

let tc_ast : I.ast -> O.ast = fun
    {types;constants;parameter;storage;operations;lambdas;block;eof} ->
  (* te is the type environment, ve is the variable environment *)
  let te =
    SMap.empty
    |> type_decls_to_tenv types in
  let ve =
    SMap.empty
    |> (match parameter.value with (_,name,_,ty) -> shadow (unreg name) @@ xty ty)
    |> shadow "storage"    @@ xty (snd storage.value)
    |> shadow "operations" @@ xty (snd operations.value)
  in
  let (ve',te'), lambdas = fold_map tc_lambda_decl (ve, te) lambdas in
  let (ve'', te''), block = tc_block (ve', te') (unreg block) in
  let _ve'' = ve'' in (* not needed anymore *)
  let _te'' = te'' in (* not needed anymore *)
  mk_ast ~lambdas ~block
 *)
