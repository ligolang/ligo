[@@@warning "-42"]

(* Dependencies *)

module Region = Simple_utils.Region
module CST    = Cst.Cameligo

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
  PConstr p -> (vars_of_pconstr env p : _ result)
| PUnit _
| PInt _ | PNat _ | PBytes _
| PString _ | PVerbatim _ -> ok @@ env
| PVar {var} when is_wildcard var -> ok @@ env
| PVar {var} ->
    let* () = check_reserved_name var in
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
  (* TODO: Hmm, not really sure
  let var = field.value.field_name in
  if VarSet.mem var env then
    fail @@ non_linear_pattern var
  else
  *)
  let p = field.value.pattern in
  vars_of_pattern env p

and vars_of_pconstr env = function
  PSomeApp {value=(_, pattern); _} ->
    vars_of_pattern env pattern
| PConstrApp {value=(_, Some pattern); _} ->
    vars_of_pattern env pattern
| PNone _
| PFalse _
| PTrue _
| PConstrApp _ -> ok @@ env

and vars_of_plist env = function
  PListComp {value; _} ->
    Helpers.bind_fold_pseq vars_of_pattern env value.elements
| PCons {value; _} ->
    let head, _, tail = value in
    bind_fold_list vars_of_pattern env [head; tail]

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
   TSum {value; _} ->
     let* () =
       Utils.nsepseq_to_list value.variants |> check_variants
     in ok @@ ()
  | TRecord {value; _} ->
     let* () =
       Utils.nsepseq_to_list value.ne_elements |> check_fields
     in ok @@ ()
  | TProd   _
  | TApp    _
  | TFun    _
  | TPar    _
  | TVar    _
  | TModA   _
  | TWild   _
  | TString _
  | TInt    _ -> ok @@ ()

let peephole_expression : unit -> expr -> (unit,'err) result =
  fun () e ->
  match e with
    ECase {value; _}   ->
      let apply ({value; _}: _ case_clause reg)  =
        check_pattern value.pattern in
      let* () =
        Trace.bind_iter_list
          apply
          (Utils.nsepseq_to_list value.cases.value)
      in ok @@ ()
  | ELetIn {value; _}   ->
      let* () =
        Trace.bind_iter_list
          check_pattern
          (Utils.nseq_to_list value.binding.binders)
      in ok @@ ()
  | ETypeIn {value; _}   ->
      let* () = check_reserved_name value.type_decl.name
      in ok @@ ()
  | EModIn {value; _}   ->
      let* () = check_reserved_name value.mod_decl.name
      in ok @@ ()
  | EModAlias {value; _}   ->
      let* () = check_reserved_name value.mod_alias.alias
      in ok @@ ()
  | EFun     _
  | ESeq     _
  | ECodeInj _
  | ECond    _
  | EAnnot   _
  | ELogic   _
  | EArith   _
  | EString  _
  | EList    _
  | EConstr  _
  | ERecord  _
  | EProj    _
  | EUpdate  _
  | EModA    _
  | EVar     _
  | ECall    _
  | EBytes   _
  | EUnit    _
  | ETuple   _
  | EPar     _ -> ok @@ ()

let peephole_declaration : unit -> declaration -> (unit, 'err) result =
  fun _ ->
  function
    Let {value; _} ->
      let _, _, binding, _ = value in
      let* () =
        Trace.bind_iter_list
          check_pattern
          (Utils.nseq_to_list binding.binders)
      in ok @@ ()
  | TypeDecl {value; _} ->
      let* () = check_reserved_name value.name
      in ok @@ ()
  | ModuleDecl {value; _} ->
      let* () = check_reserved_name value.name
      in ok @@ ()
  | ModuleAlias {value; _} ->
      let* () = check_reserved_name value.alias
      in ok @@ ()
  | Directive _ -> ok ()

let peephole : (unit,'err) Helpers.folder = {
  t = peephole_type;
  e = peephole_expression;
  d = peephole_declaration;
}
