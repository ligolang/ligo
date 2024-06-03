module Region = Simple_utils.Region
open Lsp_helpers

(** Creates a folding range out of the given kind and region. The region will be
    appropriately converted from the LIGO format (1-indexed lines, 0-indexed characters)
    to the LSP format (0-indexed lines, 0-indexed columns). *)
let mk_folding_range : FoldingRangeKind.t -> Region.t -> FoldingRange.t option =
 fun kind reg ->
  if reg#start#line = reg#stop#line
     (* We don't want to create ranges for regions that fit into one line *)
  then None
  else
    Some
      (let line_diff = 1 in
       let character_diff = 0 in
       let to_column pos = pos#point_num - pos#point_bol + 1 - character_diff in
       let startLine = reg#start#line - line_diff in
       let endLine = reg#stop#line - line_diff in
       let startCharacter = to_column reg#start in
       let endCharacter = to_column reg#stop in
       FoldingRange.create ~startLine ~startCharacter ~endLine ~endCharacter ~kind ())

(** Creates a folding range with [FoldingRangeKind.Region] out of the given region. The
    region will be appropriately converted from the LIGO format (1-indexed lines,
    0-indexed characters) to the LSP format (0-indexed lines, 0-indexed columns). *)
let mk_region : Region.t -> FoldingRange.t option =
  mk_folding_range FoldingRangeKind.Region

(** Creates a folding range with [FoldingRangeKind.Imports] out of the given region. The
    region will be appropriately converted from the LIGO format (1-indexed lines,
    0-indexed characters) to the LSP format (0-indexed lines, 0-indexed columns). *)
let mk_imports : Region.t -> FoldingRange.t option =
  mk_folding_range FoldingRangeKind.Imports

(** Folds the CameLIGO CST, collecting folding ranges that we believe that might be
    interesting to the user. *)
let folding_range_cameligo : Cst.Cameligo.t -> FoldingRange.t list option =
 fun cst ->
  let open Cst_cameligo.Fold in
  let get_range : some_node -> FoldingRange.t fold_control =
   fun (Some_node (x, b)) ->
    match b with
    | S_reg (S_let_in | S_type_in) -> Skip
    | S_reg _ ->
      (match mk_region x.region with
      | Some r -> Continue r
      | _ -> Skip)
    | _ -> Skip
  in
  Some (fold_cst [] (Fun.flip List.cons) get_range cst)

(** Folds the JsLIGO CST, collecting folding ranges that we believe that might be
    interesting to the user. *)
let folding_range_jsligo : Cst.Jsligo.t -> FoldingRange.t list option =
 fun cst ->
  let open Cst_jsligo.Fold in
  let get_range : some_node -> FoldingRange.t fold_control =
   fun (Some_node (x, b)) ->
    let process : 'a option -> 'a fold_control = function
      | None -> Skip
      | Some r -> Continue r
    in
    match b with
    | S_reg (S_import_alias _) -> process (mk_imports x.region)
    | S_reg (S_import_all_as _) -> process (mk_imports x.region)
    | S_reg (S_import_from _) -> process (mk_imports x.region)
    | S_reg (S_import_decl _) -> process (mk_imports x.region)
    | S_reg _ -> process (mk_region x.region)
    | _ -> Skip
  in
  Some (fold_cst [] (Fun.flip List.cons) get_range cst)

(** Runs the handler for folding range. This is usually called when the document is opened
    or after the user types something. *)
let on_req_folding_range : Path.t -> FoldingRange.t list option Handler.t =
 fun file ->
  Handler.with_cst file ~default:None
  @@ fun cst ->
  Handler.return
  @@ Dialect_cst.from_dialect
       { cameligo = folding_range_cameligo; jsligo = folding_range_jsligo }
       cst
