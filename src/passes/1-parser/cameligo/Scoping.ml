[@@@warning "-42"]


type t =
  Reserved_name      of AST.variable
| Duplicate_variant  of AST.variable
| Non_linear_pattern of AST.variable
| Duplicate_field    of AST.variable

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
  |> add "abs"
  |> add "address"
  |> add "amount"
  |> add "assert"
  |> add "balance"
  |> add "black2b"
  |> add "check"
  |> add "continue"
  |> add "failwith"
  |> add "gas"
  |> add "hash"
  |> add "hash_key"
  |> add "implicit_account"
  |> add "int"
  |> add "pack"
  |> add "self_address"
  |> add "sender"
  |> add "sha256"
  |> add "sha512"
  |> add "source"
  |> add "stop"
  |> add "time"
  |> add "unit"
  |> add "unpack"

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

(* Checking the linearity of patterns *)

open! AST

let rec vars_of_pattern env = function
  PConstr p -> vars_of_pconstr env p
| PUnit _ | PFalse _ | PTrue _
| PInt _ | PNat _ | PBytes _
| PString _ | PWild _ -> env
| PVar var ->
    if VarSet.mem var env then
      raise (Error (Non_linear_pattern var))
    else VarSet.add var env
| PList l -> vars_of_plist env l
| PTuple t -> Utils.nsepseq_foldl vars_of_pattern env t.value
| PPar p -> vars_of_pattern env p.value.inside
| PRecord p -> vars_of_fields env p.value.ne_elements
| PTyped p -> vars_of_pattern env p.value.pattern

and vars_of_fields env fields =
  Utils.nsepseq_foldl vars_of_field_pattern env fields

and vars_of_field_pattern env field =
  let var = field.value.field_name in
  if VarSet.mem var env then
    raise (Error (Non_linear_pattern var))
  else
    let p = field.value.pattern
    in vars_of_pattern (VarSet.add var env) p

and vars_of_pconstr env = function
  PNone _ -> env
| PSomeApp {value=_, pattern; _} ->
    vars_of_pattern env pattern
| PConstrApp {value=_, Some pattern; _} ->
    vars_of_pattern env pattern
| PConstrApp {value=_,None; _} -> env

and vars_of_plist env = function
  PListComp {value; _} ->
    Utils.sepseq_foldl vars_of_pattern env value.elements
| PCons {value; _} ->
    let head, _, tail = value in
    List.fold_left vars_of_pattern env [head; tail]

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

(* Checking record fields *)

let check_fields fields =
  let add acc {value; _} =
    if VarSet.mem (value: field_decl).field_name acc then
      raise (Error (Duplicate_field value.field_name))
    else VarSet.add value.field_name acc in
  let fields =
    List.fold_left add VarSet.empty fields
  in ignore fields
