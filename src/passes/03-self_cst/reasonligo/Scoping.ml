[@@@warning "-42"]

(* Dependencies *)

module Region = Simple_utils.Region
module CST    = Cst.Reasonligo

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
| PUnit _
| PInt _ | PNat _ | PBytes _
| PString _ | PVerbatim _ -> ok @@ env
| PVar {var} when is_wildcard var -> ok @@ env
| PVar {var} ->
    if VarSet.mem var env then
      fail @@ non_linear_pattern var
    else ok @@ VarSet.add var env
| PList l -> vars_of_plist env l
| PTuple t -> Helpers.bind_fold_npseq vars_of_pattern env t.value
| PPar p -> vars_of_pattern env p.value.inside
| PRecord p -> vars_of_fields env p.value.ne_elements
| PTyped p -> vars_of_pattern env p.value.pattern

and vars_of_fields env fields =
  Helpers.bind_fold_npseq vars_of_field_pattern env fields

and vars_of_field_pattern env field =
  let var = field.value.field_name in
  if VarSet.mem var env then
    fail @@ non_linear_pattern var
  else
    let p = field.value.pattern
    in vars_of_pattern (VarSet.add var env) p

and vars_of_pconstr env = function
  PNone _ -> ok env
| PSomeApp {value=_, pattern; _} ->
    vars_of_pattern env pattern
| PFalse _ | PTrue _ -> ok env
| PConstrApp {value=_, Some pattern; _} ->
    vars_of_pattern env pattern
| PConstrApp {value=_,None; _} -> ok env

and vars_of_plist env = function
  PListComp {value; _} ->
    Helpers.bind_fold_pseq vars_of_pattern env value.elements
| PCons {value; _} ->
    let {lpattern;rpattern;_} = value in
    bind_fold_list vars_of_pattern env [lpattern; rpattern]

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

(* Checking record fields *)

let check_fields fields =
  let add acc {value; _} =
    let field_name = (value: field_decl).field_name in
    if VarSet.mem field_name acc then
      fail @@ duplicate_field_name value.field_name
    else
      ok @@ VarSet.add value.field_name acc
  in ignore (bind_fold_list add VarSet.empty fields)

let peephole_type : unit -> type_expr -> (unit,'err) result = fun _ t ->
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
  | TModA   {value=_;region=_} -> ok ()
  | TVar    {value=_;region=_} -> ok ()
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
  | EConstr  _                  -> ok ()
  | ERecord  {value=_;region=_} -> ok ()
  | EProj    {value=_;region=_} -> ok ()
  | EUpdate  {value=_;region=_} -> ok ()
  | EModA   {value=_;region=_} -> ok ()
  | EVar     {value=_;region=_} -> ok ()
  | ECall    {value=_;region=_} -> ok ()
  | EBytes   {value=_;region=_} -> ok ()
  | EUnit    {value=_;region=_} -> ok ()
  | ETuple   {value=_;region=_} -> ok ()
  | EPar     {value=_;region=_} -> ok ()
  | ELetIn   {value;region=_}   ->
    let* () = check_pattern value.binding.binders in
    ok ()
  | ETypeIn   {value;region=_}   ->
    let* () = check_reserved_name value.type_decl.name in
    ok @@ ()
  | EModIn   {value;region=_}   ->
    let* () = check_reserved_name value.mod_decl.name in
    ok @@ ()
  | EModAlias {value;region=_}   ->
    let* () = check_reserved_name value.mod_alias.alias in
    ok @@ ()
  | EFun     {value=_;region=_} -> ok @@ ()
  | ESeq     {value=_;region=_} -> ok @@ ()
  | ECodeInj {value=_;region=_} -> ok @@ ()

let peephole_declaration : unit -> declaration -> (unit, 'err) result =
  fun _ d ->
  match d with
    ConstDecl  {value;region=_} ->
    let (_,_,binding,_) = value in
    let* () = check_pattern binding.binders in
    ok ()
  | TypeDecl {value;region=_} ->
    let* () = check_reserved_name value.name in
    ok @@ ()
  | ModuleDecl {value;region=_} ->
    let* () = check_reserved_name value.name in
    ok @@ ()
  | ModuleAlias {value;region=_} ->
    let* () = check_reserved_name value.alias in
    ok @@ ()
  | Directive _ -> ok ()

let peephole : (unit,'err) Helpers.folder = {
  t = peephole_type;
  e = peephole_expression;
  d = peephole_declaration;
}
