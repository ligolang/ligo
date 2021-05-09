(* This module exports checks on scoping, called from the parser. *)

[@@@warning "-42"]

(* Dependencies *)

module Region = Simple_utils.Region
module CST    = Cst.Pascaligo

open Region
open Errors
open Trace

(* TODO don't *)
let ignore x =
  let* _ = x in
  ok ()

(* Useful modules *)

module SSet = Set.Make (String)

module Ord =
  struct
    type t = CST.variable
    let compare v1 v2 =
      String.compare v1.value v2.value
  end

module VarSet = Set.Make (Ord)

(* Checking the definition of reserved names (shadowing) *)

let reserved =
  let open SSet in
  empty
  |> add "get_force"
  |> add "get_chain_id"
  |> add "transaction"
  |> add "get_contract"
  |> add "get_entrypoint"
  |> add "size"
  |> add "int"
  |> add "abs"
  |> add "is_nat"
  |> add "amount"
  |> add "balance"
  |> add "now"
  |> add "unit"
  |> add "source"
  |> add "sender"
  |> add "failwith"
  |> add "bitwise_or"
  |> add "bitwise_and"
  |> add "bitwise_xor"
  |> add "string_concat"
  |> add "string_slice"
  |> add "crypto_check"
  |> add "crypto_hash_key"
  |> add "bytes_concat"
  |> add "bytes_slice"
  |> add "bytes_pack"
  |> add "bytes_unpack"
  |> add "set_empty"
  |> add "set_mem"
  |> add "set_add"
  |> add "set_remove"
  |> add "set_iter"
  |> add "set_fold"
  |> add "SET_FOLD_DESC"
  |> add "list_iter"
  |> add "list_fold"
  |> add "list_fold_left"
  |> add "list_fold_right"
  |> add "list_map"
  |> add "map_iter"
  |> add "map_map"
  |> add "map_fold"
  |> add "map_remove"
  |> add "map_update"
  |> add "map_get"
  |> add "map_mem"
  |> add "sha_256"
  |> add "sha_512"
  |> add "blake2b"
  |> add "cons"
  |> add "address"
  |> add "self_address"
  |> add "implicit_account"
  |> add "set_delegate"

let check_reserved_names vars =
  let is_reserved elt = SSet.mem elt.value reserved in
  let inter = VarSet.filter is_reserved vars in
  if not (VarSet.is_empty inter) then
    let clash = VarSet.choose inter in
    fail @@ reserved_name clash
  else ok vars

let check_reserved_name var =
  if SSet.mem var.value reserved then
    fail @@ reserved_name var
  else ok ()

let is_wildcard var =
  let var = var.value in
  String.compare var Var.wildcard = 0

(* Checking the linearity of patterns *)

open! CST

let rec vars_of_pattern env = function
  PConstr p -> vars_of_pconstr env p
| PInt _ | PNat _ | PBytes _ | PString _ -> ok @@ env
| PList l -> vars_of_plist env l
| PTuple t -> vars_of_ptuple env t.value
| PRecord p -> vars_of_fields env p.value.elements
| PVar var ->
    if VarSet.mem var env then
      fail @@ non_linear_pattern var
    else
      if String.equal "_" var.value then
        ok env
      else
        ok (VarSet.add var env)

and vars_of_fields env fields =
  Helpers.bind_fold_pseq vars_of_field_pattern env fields
  
and vars_of_field_pattern env field =

  (* TODO: Hmm, not really sure
  let var = field.value.field_name in
  if VarSet.mem var env then
    fail @@ non_linear_pattern var
  else
  *)
  let p = field.value.pattern in
  vars_of_pattern env p
  

and vars_of_pconstr env = function
  PUnit _ | PFalse _ | PTrue _ | PNone _ -> ok @@ env
| PSomeApp {value=_, pattern; _} ->
    vars_of_pattern env pattern
| PConstrApp {value=_, Some tuple; _} ->
    vars_of_ptuple env tuple.value
| PConstrApp {value=_,None; _} -> ok env

and vars_of_plist env = function
  PListComp {value; _} ->
    vars_of_pinj env value
| PNil _ ->
    ok env
| PParCons {value={inside; _}; _} ->
    let head, _, tail = inside in
    let* env = vars_of_pattern env head in
    vars_of_pattern env tail
| PCons {value; _} ->
    Helpers.bind_fold_npseq vars_of_pattern env value

and vars_of_pinj env inj =
  Helpers.bind_fold_pseq vars_of_pattern env inj.elements

and vars_of_ptuple env {inside; _} =
  Helpers.bind_fold_npseq vars_of_pattern env inside

let check_linearity = vars_of_pattern VarSet.empty

(* Checking patterns *)

let check_pattern p =
  check_linearity p >>? check_reserved_names |> ignore

(* Checking variants for duplicates *)

let check_variants variants =
  let add acc {value; _} =
    if VarSet.mem value.constr acc then
      fail @@ duplicate_variant value.constr
    else ok @@ VarSet.add value.constr acc in
  let variants =
    bind_fold_list add VarSet.empty variants
  in ignore variants

(* Checking parameters *)

let check_parameters params =
  let add acc = function
    ParamConst {value; _} ->
      let* () = check_reserved_name value.var in
      if is_wildcard value.var then
        ok @@ acc
      else if VarSet.mem value.var acc then
        fail @@ duplicate_parameter value.var
      else ok @@ VarSet.add value.var acc
  | ParamVar {value; _} ->
      let* () = check_reserved_name value.var in
      if is_wildcard value.var then
        ok @@ acc
      else if VarSet.mem value.var acc then
        fail @@ duplicate_parameter value.var
      else ok @@ VarSet.add value.var acc in
  let params =
    bind_fold_list add VarSet.empty params
  in ignore params

(* Checking record fields *)

let check_fields (fields : CST.field_decl Region.reg list) =
  let add acc (field : CST.field_decl Region.reg) =
      let name = field.value.field_name in
      if VarSet.mem name acc then
        fail @@ duplicate_field_name name
      else ok @@ VarSet.add name acc in
  let fields =
    bind_fold_list add VarSet.empty fields
  in ignore fields

let peephole_type : unit -> type_expr -> (unit, 'err) result =
  fun _ t ->
  match t with
    TProd   {value=_;region=_} -> ok ()
  | TSum    {value;region=_} ->
    let* () = Utils.nsepseq_to_list value.variants |> check_variants in
    ok ()
  | TRecord {value;region=_} ->
    let* () = Utils.nsepseq_to_list value.ne_elements |> check_fields in
    ok ()
  | TApp    {value=_;region=_} -> ok ()
  | TFun    {value=_;region=_} -> ok ()
  | TPar    {value=_;region=_} -> ok ()
  | TVar    {value=_;region=_} -> ok ()
  | TModA   {value=_;region=_} -> ok ()
  | TWild   _                  -> ok ()
  | TString {value=_;region=_} -> ok ()
  | TInt    {value=_;region=_} -> ok ()

let peephole_expression : unit -> expr -> (unit,'err) result = fun () e ->
  match e with
    ECase    {value;region=_}   ->
    let* () =
      Trace.bind_iter_list
        (fun ({value;region=_}: _ case_clause reg) ->
           check_pattern value.pattern)
        (Utils.nsepseq_to_list value.cases.value) in
    ok ()
  | ECond    {value=_;region=_} -> ok ()
  | EAnnot   {value=_;region=_} -> ok ()
  | ELogic   _                  -> ok ()
  | EArith   _                  -> ok ()
  | EString  _                  -> ok ()
  | EList    _                  -> ok ()
  | ESet     _                  -> ok ()
  | EConstr  _                  -> ok ()
  | ERecord  {value=_;region=_} -> ok ()
  | EProj    {value=_;region=_} -> ok ()
  | EUpdate  {value=_;region=_} -> ok ()
  | EModA   {value=_;region=_} -> ok ()
  | EMap     _                  -> ok ()
  | EVar     {value=_;region=_} -> ok ()
  | ECall    {value=_;region=_} -> ok ()
  | EBytes   {value=_;region=_} -> ok ()
  | EUnit    _                  -> ok ()
  | ETuple   {value=_;region=_} -> ok ()
  | EPar     {value=_;region=_} -> ok ()
  | EFun     {value=_;region=_} -> ok ()
  | ECodeInj {value=_;region=_} -> ok ()
  | EBlock   {value=_;region=_} -> ok ()

let peephole_statement : unit -> statement -> (unit, 'err) result = fun _ s ->
  match s with
    Instr Loop For ForCollect  {value;region=_} ->
    let* () = check_reserved_name value.var in
    let* _ = bind_map_option (Function.compose check_reserved_name snd) value.bind_to in
    ok ()
  | Instr Loop For ForInt {value;region=_} ->
    let* () = check_reserved_name value.binder in
    ok ()
  | Instr _ -> ok ()
  | Data LocalConst {value;region=_} ->
    let {kwd_const=_;pattern;const_type=_;equal=_;init=_;terminator=_;attributes=_} = value in
    let* () = check_pattern pattern in
    ok @@ ()
  | Data LocalVar {value;region=_} ->
    let {kwd_var=_;pattern;var_type=_;assign=_;init=_;terminator=_} = value in
    let* () = check_pattern pattern in
    ok @@ ()
  | Data LocalFun {value;region=_}  ->
    let {kwd_recursive=_;kwd_function=_;fun_name;param;ret_type=_;kwd_is=_;return=_;terminator=_;attributes=_} = value in
    let* () = check_parameters @@ Utils.nsepseq_to_list param.value.inside in
    let* () = check_reserved_name fun_name in
    ok ()
  | Data LocalType  {value;region=_} ->
    let {kwd_type=_;name;kwd_is=_;type_expr=_;terminator=_} = value in
    let* () = check_reserved_name name in
    ok ()
  | Data LocalModule {value;region=_} ->
    let {kwd_module=_;name;kwd_is=_;enclosing=_;module_=_;terminator=_} = value in
    let* () = check_reserved_name name in
    ok ()
  | Data LocalModuleAlias {value;region=_} ->
    let {kwd_module=_;alias;kwd_is=_;binders=_;terminator=_} = value in
    let* () = check_reserved_name alias in
    ok ()

let peephole_declaration : unit -> declaration -> (unit, 'err) result = fun _ d ->
  match d with
  | TypeDecl  {value;region=_} ->
    let* () = check_reserved_name value.name in
    ok ()
  | ConstDecl {value;region=_} ->
    let {kwd_const=_;pattern;const_type=_;equal=_;init=_;terminator=_;attributes=_} = value in
    let* () = check_pattern pattern in
    ok @@ ()
  | FunDecl {value;region=_} ->
     let {kwd_recursive=_; kwd_function=_; fun_name; param; ret_type=_;
          kwd_is=_; return=_; terminator=_; attributes=_} = value in
    let* () = check_parameters @@ Utils.nsepseq_to_list param.value.inside in
    let* () = check_reserved_name fun_name in
    ok ()
  | ModuleDecl  {value;region=_} ->
    let* () = check_reserved_name value.name in
    ok ()
  | ModuleAlias {value;region=_} ->
    let* () = check_reserved_name value.alias in
    ok ()
  | Directive _ -> ok ()


let peephole : (unit,'err) Helpers.folder = {
  t = peephole_type;
  e = peephole_expression;
  s = peephole_statement;
  d = peephole_declaration;
}
