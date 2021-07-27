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
  let _ = x in
  ()

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

let check_reserved_names ~raise vars =
  let is_reserved elt = SSet.mem elt.value reserved in
  let inter = VarSet.filter is_reserved vars in
  if not (VarSet.is_empty inter) then
    let clash = VarSet.choose inter in
    raise.raise @@ reserved_name clash
  else vars

let check_reserved_name ~raise var =
  if SSet.mem var.value reserved then
    raise.raise @@ reserved_name var
  else ()

let is_wildcard var =
  let var = var.value in
  String.compare var Var.wildcard = 0

(* Checking the linearity of patterns *)

open! CST

let rec vars_of_pattern ~raise env = function
  PConstr p -> vars_of_pconstr ~raise env p
| PInt _ | PNat _ | PBytes _ | PString _ -> env
| PList l -> vars_of_plist ~raise env l
| PTuple t -> vars_of_ptuple ~raise env t.value
| PRecord p -> vars_of_fields ~raise env p.value.elements
| PVar var ->
    if VarSet.mem var env then
      raise.raise @@ non_linear_pattern var
    else
      if String.equal "_" var.value then
        env
      else
        (VarSet.add var env)

and vars_of_fields ~raise env fields =
  Helpers.bind_fold_pseq (vars_of_field_pattern ~raise) env fields
  
and vars_of_field_pattern ~raise env field =

  (* TODO: Hmm, not really sure
  let var = field.value.field_name in
  if VarSet.mem var env then
    raise.raise @@ non_linear_pattern var
  else
  *)
  let p = field.value.pattern in
  vars_of_pattern ~raise env p
  

and vars_of_pconstr ~raise env = function
  PUnit _ | PFalse _ | PTrue _ | PNone _ -> env
| PSomeApp {value=_, pattern; _} ->
    vars_of_pattern ~raise env pattern
| PConstrApp {value=_, Some tuple; _} ->
    vars_of_ptuple ~raise env tuple.value
| PConstrApp {value=_,None; _} -> env

and vars_of_plist ~raise env = function
  PListComp {value; _} ->
    vars_of_pinj ~raise env value
| PNil _ ->
    env
| PParCons {value={inside; _}; _} ->
    let head, _, tail = inside in
    let env = vars_of_pattern ~raise env head in
    vars_of_pattern ~raise env tail
| PCons {value; _} ->
    Helpers.fold_npseq (vars_of_pattern ~raise) env value

and vars_of_pinj ~raise env inj =
  Helpers.bind_fold_pseq (vars_of_pattern ~raise) env inj.elements

and vars_of_ptuple ~raise env {inside; _} =
  Helpers.fold_npseq (vars_of_pattern ~raise) env inside

let check_linearity ~raise = vars_of_pattern ~raise VarSet.empty

(* Checking patterns *)

let check_pattern ~raise p =
  check_linearity ~raise p |> check_reserved_names ~raise |> ignore

(* Checking variants for duplicates *)

let check_variants ~raise variants =
  let add acc {value; _} =
    if VarSet.mem value.constr acc then
      raise.raise @@ duplicate_variant value.constr
    else VarSet.add value.constr acc in
  let variants =
    List.fold ~f:add ~init:VarSet.empty variants
  in ignore variants

(* Checking parameters *)

let check_parameters ~raise params =
  let add acc = function
    ParamConst {value; _} ->
      let () = check_reserved_name ~raise value.var in
      if is_wildcard value.var then
        acc
      else if VarSet.mem value.var acc then
        raise.raise @@ duplicate_parameter value.var
      else VarSet.add value.var acc
  | ParamVar {value; _} ->
      let () = check_reserved_name ~raise value.var in
      if is_wildcard value.var then
        acc
      else if VarSet.mem value.var acc then
        raise.raise @@ duplicate_parameter value.var
      else VarSet.add value.var acc in
  let params =
    List.fold ~f:add ~init:VarSet.empty params
  in ignore params

(* Checking record fields *)

let check_fields ~raise (fields : CST.field_decl Region.reg list) =
  let add acc (field : CST.field_decl Region.reg) =
      let name = field.value.field_name in
      if VarSet.mem name acc then
        raise.raise @@ duplicate_field_name name
      else VarSet.add name acc in
  let fields =
    List.fold ~f:add ~init:VarSet.empty fields
  in ignore fields

let peephole_type ~raise : unit -> type_expr -> unit =
  fun _ t ->
  match t with
    TProd   {value=_;region=_} -> ()
  | TSum    {value;region=_} ->
    let () = Utils.nsepseq_to_list value.variants |> check_variants ~raise in
    ()
  | TRecord {value;region=_} ->
    let () = Utils.nsepseq_to_list value.ne_elements |> check_fields ~raise in
    ()
  | TApp    {value=_;region=_} -> ()
  | TFun    {value=_;region=_} -> ()
  | TPar    {value=_;region=_} -> ()
  | TVar    {value=_;region=_} -> ()
  | TModA   {value=_;region=_} -> ()
  | TWild   _                  -> ()
  | TString {value=_;region=_} -> ()
  | TInt    {value=_;region=_} -> ()

let peephole_expression ~raise : unit -> expr -> unit = fun () e ->
  match e with
    ECase    {value;region=_}   ->
    let () =
      List.iter
        ~f:(fun ({value;region=_}: _ case_clause reg) ->
           check_pattern ~raise value.pattern)
        (Utils.nsepseq_to_list value.cases.value) in
    ()
  | ECond    {value=_;region=_} -> ()
  | EAnnot   {value=_;region=_} -> ()
  | ELogic   _                  -> ()
  | EArith   _                  -> ()
  | EString  _                  -> ()
  | EList    _                  -> ()
  | ESet     _                  -> ()
  | EConstr  _                  -> ()
  | ERecord  {value=_;region=_} -> ()
  | EProj    {value=_;region=_} -> ()
  | EUpdate  {value=_;region=_} -> ()
  | EModA   {value=_;region=_} -> ()
  | EMap     _                  -> ()
  | EVar     {value=_;region=_} -> ()
  | ECall    {value=_;region=_} -> ()
  | EBytes   {value=_;region=_} -> ()
  | EUnit    _                  -> ()
  | ETuple   {value=_;region=_} -> ()
  | EPar     {value=_;region=_} -> ()
  | EFun     {value=_;region=_} -> ()
  | ECodeInj {value=_;region=_} -> ()
  | EBlock   {value=_;region=_} -> ()

let peephole_statement ~raise : unit -> statement -> unit = fun _ s ->
  match s with
    Instr Loop For ForCollect  {value;region=_} ->
    let () = check_reserved_name ~raise value.var in
    let _ = Option.map ~f:Function.(check_reserved_name ~raise <@  snd) value.bind_to in
    ()
  | Instr Loop For ForInt {value;region=_} ->
    let () = check_reserved_name ~raise value.binder in
    ()
  | Instr _ -> ()
  | Data LocalConst {value;region=_} ->
    let {kwd_const=_;pattern;const_type=_;equal=_;init=_;terminator=_;attributes=_} = value in
    let () = check_pattern ~raise pattern in
    ()
  | Data LocalVar {value;region=_} ->
    let {kwd_var=_;pattern;var_type=_;assign=_;init=_;terminator=_} = value in
    let () = check_pattern ~raise pattern in
    ()
  | Data LocalFun {value;region=_}  ->
    let {kwd_recursive=_;kwd_function=_;fun_name;param;ret_type=_;kwd_is=_;return=_;terminator=_;attributes=_} = value in
    let () = check_parameters ~raise @@ Utils.nsepseq_to_list param.value.inside in
    let () = check_reserved_name ~raise fun_name in
    ()
  | Data LocalType  {value;region=_} ->
    let {kwd_type=_;name;kwd_is=_;type_expr=_;terminator=_} = value in
    let () = check_reserved_name ~raise name in
    ()
  | Data LocalModule {value;region=_} ->
    let {kwd_module=_;name;kwd_is=_;enclosing=_;module_=_;terminator=_} = value in
    let () = check_reserved_name ~raise name in
    ()
  | Data LocalModuleAlias {value;region=_} ->
    let {kwd_module=_;alias;kwd_is=_;binders=_;terminator=_} = value in
    let () = check_reserved_name ~raise alias in
    ()

let peephole_declaration ~raise : unit -> declaration -> unit = fun _ d ->
  match d with
  | TypeDecl  {value;region=_} ->
    let () = check_reserved_name ~raise value.name in
    ()
  | ConstDecl {value;region=_} ->
    let {kwd_const=_;pattern;const_type=_;equal=_;init=_;terminator=_;attributes=_} = value in
    let () = check_pattern ~raise pattern in
    ()
  | FunDecl {value;region=_} ->
     let {kwd_recursive=_; kwd_function=_; fun_name; param; ret_type=_;
          kwd_is=_; return=_; terminator=_; attributes=_} = value in
    let () = check_parameters ~raise @@ Utils.nsepseq_to_list param.value.inside in
    let () = check_reserved_name ~raise fun_name in
    ()
  | ModuleDecl  {value;region=_} ->
    let () = check_reserved_name ~raise value.name in
    ()
  | ModuleAlias {value;region=_} ->
    let () = check_reserved_name ~raise value.alias in
    ()
  | Directive _ -> ()


let peephole ~raise : (unit,'err) Helpers.folder = {
  t = peephole_type ~raise;
  e = peephole_expression ~raise;
  s = peephole_statement ~raise;
  d = peephole_declaration ~raise;
}
