[@@@warning "-42"]

(* Dependencies *)

module Region = Simple_utils.Region
module CST    = Cst.Jsligo

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
  |> add "await"
  |> add "break"
  |> add "case"
  |> add "catch"
  |> add "class"
  |> add "const"
  |> add "continue"
  |> add "debugger"
  |> add "default"
  |> add "delete"
  |> add "do"
  |> add "else"
  |> add "enum"
  |> add "export"
  |> add "extends"
  |> add "false"
  |> add "finally"
  |> add "for"
  |> add "function"
  |> add "if"
  |> add "import"
  |> add "in"
  |> add "instanceOf"
  |> add "new"
  |> add "null"
  |> add "return"
  |> add "super"
  |> add "switch"
  |> add "this"
  |> add "throw"
  |> add "true"
  |> add "try"
  |> add "typeof"
  |> add "var"
  |> add "void"
  |> add "while"
  |> add "with"
  |> add "yield"

  |> add "implements"
  |> add "interface"
  |> add "package"
  |> add "private"
  |> add "protected"
  |> add "public"

  |> add "arguments"
  |> add "eval"

let check_reserved_names vars =
  let is_reserved elt = SSet.mem elt.value reserved in
  let inter = VarSet.filter is_reserved vars in
  if not (VarSet.is_empty inter) then
    let clash = VarSet.choose inter in
    fail @@ reserved_name clash
  else ok @@ vars

let check_reserved_name var =
  if SSet.mem var.value reserved then
    fail @@ reserved_name var
  else ok @@ ()

(* Checking the linearity of patterns *)

open! CST

let rec vars_of_pattern env = function
  PVar var ->
    if VarSet.mem var env then
      fail @@ non_linear_pattern var
    else ok @@ VarSet.add var env 
| PConstr   p ->
    if VarSet.mem p env then
      fail @@ non_linear_pattern p
    else ok @@ VarSet.add p env 
| PDestruct {value = {property; target = {value = {binders; _}; _}; _}; _} -> 
    if VarSet.mem property env then
      fail @@ non_linear_pattern property
    else (
      let* env = vars_of_pattern env binders in
      ok @@ VarSet.add property env
    )
| PObject   {value = {inside; _}; _}
| PArray    {value = {inside; _}; _} -> 
    let* env = Utils.nsepseq_to_list inside |> check_patterns in
    ok @@ env
| PAssign {value = {property; _}; _} -> 
    if VarSet.mem property env then
      fail @@ non_linear_pattern property
    else ok @@ VarSet.add property env 
| PWild _
| PRest _ ->
    ok @@ env 

and check_linearity p = vars_of_pattern VarSet.empty p

(* Checking patterns *)

and check_pattern p =
  check_linearity p >>? check_reserved_names

and check_patterns patterns =
  let add _acc p =
    let* env = check_pattern p in
    ok @@ env
  in bind_fold_list add VarSet.empty patterns


(* Checking variants for duplicates *)

let check_variants variants =
  let rec add acc = function
    TString value
  | TVar value -> 
      if VarSet.mem value acc then
        fail @@ duplicate_variant value
      else ok @@ VarSet.add value acc
  | TProd {inside = {value = {inside; _}; _}; _ } as t -> (
    let items = Utils.nsepseq_to_list inside in
    match items with 
      hd :: [] -> add acc hd
    | TString _ as hd :: _ -> add acc hd
    | _ -> 
      fail @@ not_supported_variant t
      )
  | _ as t -> 
    fail @@ not_supported_variant t
  in
  let variants =
    bind_fold_list add VarSet.empty variants
  in ignore variants

(* Checking object fields *)

let check_fields fields =
  let add acc ({value; _}: field_decl reg) =
    let field_name = (value: field_decl).field_name in
    if VarSet.mem field_name acc then
      fail @@ duplicate_field_name value.field_name
    else
      ok @@ VarSet.add value.field_name acc
  in ignore (bind_fold_list add VarSet.empty fields)

let peephole_type : unit -> type_expr -> (unit,'err) result = fun _ t ->
  match t with
    TSum {value; _} ->
      let* () = Utils.nsepseq_to_list value.variants |> check_variants in
    ok @@ ()
  | TObject {value; _} ->
      let* () = Utils.nsepseq_to_list value.ne_elements |> check_fields in
      ok @@ ()
  | TProd _
  | TApp _
  | TFun _
  | TPar _
  | TString _
  | TVar _
  | TModA _
  | TInt _
  | TWild _ -> ok @@ ()

let peephole_expression : unit -> expr -> (unit,'err) result = fun () _ ->
  ok @@ ()

let check_binding ({value = {binders; _}; _}: CST.let_binding Region.reg) = 
  let* () = ignore(check_pattern binders) in
  ok @@ ()

let check_bindings bindings =
  let add _acc b =
    let* () = check_binding b in
    ok @@ ()
  in bind_fold_list add () bindings

let rec peephole_statement : unit -> statement -> (unit, 'err) result = fun _ s ->
  match s with
    SExpr e -> 
    let* () = peephole_expression () e in
    ok @@ ()
  | SNamespace {value = (_, name, _); _} ->
    let* () = check_reserved_name name in 
    ok @@ ()
  | SExport {value = (_, e); _} -> 
    peephole_statement () e
  | SLet   {value = {bindings; _}; _}
  | SConst {value = {bindings; _}; _} ->
    let* () = Utils.nsepseq_to_list bindings |> check_bindings in 
    ok @@ ()
  | SType  {value = {name; _}; _} ->
    let* () = check_reserved_name name in 
    ok @@ ()
  | SWhile {value = {expr; statement; _}; _}
  | SForOf {value = {expr; statement; _}; _} ->
    let* () = peephole_expression () expr in
    let* () = peephole_statement () statement in
    ok @@ ()
  | SBlock  _
  | SCond   _
  | SReturn _
  | SBreak _
  | SImport _
  | SSwitch _ -> ok @@ ()

let peephole : (unit,'err) Helpers.folder = {
  t = peephole_type;
  e = peephole_expression;
  d = peephole_statement;
}
