(* This module exports checks on scoping, called from the parser. *)

[@@@warning "-42"]

(* Dependencies *)

module Region = Simple_utils.Region
module CST    = Cst.Pascaligo
module Token  = Lexer_pascaligo.Token

open Region

type window = <
  last_token : Token.t option;
  current_token : Token.t
>

let mk_window var =
  object
    method last_token    = None; (* To keep it simple *)
    method current_token = Token.Ident var
  end

exception Error of string * window

let raise_reserved_name var : 'a =
  let msg =
    Printf.sprintf
      "Reserved name %S.\nHint: Change the name.\n" var.value
  in raise (Error (msg, mk_window var))

let raise_duplicate_variant var : 'a =
  let msg =
    Printf.sprintf
      "Duplicate constructor %S in this sum type declaration.\n\
       Hint: Change the constructor.\n" var.value
  in raise (Error (msg, mk_window var))

let raise_non_linear_pattern var : 'a =
  let msg =
    Printf.sprintf
      "Repeated variable %S in this pattern.\n\
       Hint: Change the name.\n" var.value
  in raise (Error (msg, mk_window var))

let raise_duplicate_field_name var : 'a =
  let msg =
    Printf.sprintf
      "Duplicate field name %S in this record declaration.\n\
       Hint: Change the name.\n" var.value
  in raise (Error (msg, mk_window var))

let raise_duplicate_parameter var : 'a =
  let msg =
    Printf.sprintf
      "Duplicate parameter %S.\nHint: Change the name.\n" var.value
  in raise (Error (msg, mk_window var))

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
  |> add "address"
  |> add "self_address"
  |> add "implicit_account"
  |> add "set_delegate"

let check_reserved_names vars =
  let is_reserved elt = SSet.mem elt.value reserved in
  let inter = VarSet.filter is_reserved vars in
  if not (VarSet.is_empty inter) then
    let clash = VarSet.choose inter in
    raise_reserved_name clash
  else vars

let check_reserved_name var =
  if SSet.mem var.value reserved then
    raise_reserved_name var

(* Checking the linearity of patterns *)

open! CST

let rec vars_of_pattern env = function
  PConstr p -> vars_of_pconstr env p
| PWild _ | PInt _ | PNat _ | PBytes _ | PString _ -> env
| PList l -> vars_of_plist env l
| PTuple t -> vars_of_ptuple env t.value
| PVar var ->
    if VarSet.mem var env then
      raise_non_linear_pattern var
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
      raise_duplicate_variant value.constr
    else VarSet.add value.constr acc in
  let variants =
    List.fold_left add VarSet.empty variants
  in ignore variants

(* Checking parameters *)

let check_parameters params =
  let add acc = function
    ParamConst {value; _} ->
      check_reserved_name value.var;
      if VarSet.mem value.var acc then
        raise_duplicate_parameter value.var
      else VarSet.add value.var acc
  | ParamVar {value; _} ->
      check_reserved_name value.var;
      if VarSet.mem value.var acc then
        raise_duplicate_parameter value.var
      else VarSet.add value.var acc in
  let params =
    List.fold_left add VarSet.empty params
  in ignore params

(* Checking record fields *)

let check_fields (fields : CST.field_decl Region.reg list) =
  let add acc (field : CST.field_decl Region.reg) =
      let name = field.value.field_name in
      if VarSet.mem name acc then
        raise_duplicate_field_name name
      else VarSet.add name acc in
  let fields =
    List.fold_left add VarSet.empty fields
  in ignore fields
