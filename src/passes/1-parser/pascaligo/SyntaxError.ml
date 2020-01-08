[@@@warning "-42"]

type t =
  Reserved_name       of AST.variable
| Duplicate_parameter of AST.variable
| Duplicate_variant   of AST.variable
| Non_linear_pattern  of AST.variable

type error = t

exception Error of t

open Region

(* Useful modules *)

module SSet = Utils.String.Set

module Ord =
  struct
    type t = AST.variable
    let compare v1 v2 =
      compare v1.value v2.value
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
  |> add "list_iter"
  |> add "list_fold"
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

let check_reserved_names vars =
  let is_reserved elt = SSet.mem elt.value reserved in
  let inter = VarSet.filter is_reserved vars in
  if not (VarSet.is_empty inter) then
    let clash = VarSet.choose inter in
    raise (Error (Reserved_name clash))
  else vars

let check_reserved_name var =
  if SSet.mem var.value reserved then
    raise (Error (Reserved_name var))
  else var

let check_reserved_name_opt = function
  Some var -> ignore (check_reserved_name var)
| None     -> ()

(* Checking the linearity of patterns *)

open! AST

let rec vars_of_pattern env = function
  PConstr p -> vars_of_pconstr env p
| PWild _ | PInt _ | PNat _ | PBytes _ | PString _ -> env
| PList l -> vars_of_plist env l
| PTuple t -> vars_of_ptuple env t.value
| PVar var ->
    if VarSet.mem var env then
      raise (Error (Non_linear_pattern var))
    else VarSet.add var env

and vars_of_pconstr env = function
  PUnit _ | PFalse _ | PTrue _ | PNone _ -> env
| PSomeApp {value=_, {value={inside; _};_}; _} ->
    vars_of_pattern env inside
| PConstrApp {value=_, Some tuple; _} ->
    vars_of_ptuple env tuple.value
| PConstrApp {value=_,None; _} -> env

and vars_of_plist env = function
  PListComp {value; _} ->
    vars_of_pinj env value
| PNil _ ->
    env
| PParCons {value={inside; _}; _} ->
   let head, _, tail = inside in
   vars_of_pattern (vars_of_pattern env head) tail
| PCons {value; _} ->
    Utils.nsepseq_foldl vars_of_pattern env value

and vars_of_pinj env inj =
  Utils.sepseq_foldl vars_of_pattern env inj.elements

and vars_of_ptuple env {inside; _} =
  Utils.nsepseq_foldl vars_of_pattern env inside

let check_linearity = vars_of_pattern VarSet.empty

(* Checking patterns *)

let check_pattern p =
  check_linearity p |> check_reserved_names |> ignore

(* Checking variants for duplicates *)

let check_variants variants =
  let add acc {value; _} =
    if VarSet.mem value.constr acc then
      raise (Error (Duplicate_variant value.constr))
    else VarSet.add value.constr acc in
  let variants =
    List.fold_left add VarSet.empty variants
  in ignore variants

(* Checking parameters *)

let check_parameters params =
  let add acc = function
    ParamConst {value; _} ->
      if VarSet.mem value.var acc then
        raise (Error (Duplicate_parameter value.var))
      else VarSet.add value.var acc
  | ParamVar {value; _} ->
      if VarSet.mem value.var acc then
        raise (Error (Duplicate_parameter value.var))
      else VarSet.add value.var acc in
  let params =
    List.fold_left add VarSet.empty params
  in ignore params
