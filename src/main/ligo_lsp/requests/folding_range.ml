open Lsp_helpers
open Simple_utils.Utils

let mk_folding_range : FoldingRangeKind.t -> Region.t -> FoldingRange.t =
 fun kind reg ->
  let line_diff = 1 in
  let character_diff = 0 in
  let to_column pos = pos#point_num - pos#point_bol + 1 - character_diff in
  let startLine = reg#start#line - line_diff in
  let endLine = reg#stop#line - line_diff in
  let startCharacter = Some (to_column reg#start) in
  let endCharacter = Some (to_column reg#stop) in
  FoldingRange.create ~startLine ?startCharacter ~endLine ?endCharacter ~kind ()


let mk_region : Region.t -> FoldingRange.t = mk_folding_range FoldingRangeKind.Region
let mk_imports : Region.t -> FoldingRange.t = mk_folding_range FoldingRangeKind.Imports
let nseq_concat_map nseq ~f = List.concat_map (nseq_to_list nseq) ~f
let nsepseq_concat_map nsepseq ~f = List.concat_map (nsepseq_to_list nsepseq) ~f
let sepseq_concat_map sepseq ~f = List.concat_map (sepseq_to_list sepseq) ~f

let folding_range_cameligo : Cst.Cameligo.t -> FoldingRange.t list option =
 fun cst ->
  let open Cst_cameligo.Fold in
  let get_range : some_node -> FoldingRange.t fold_control =
   fun (Some_node (x, b)) ->
    match b with
    | S_reg (S_let_in | S_type_in) -> Skip
    | S_reg _ -> Continue (mk_region x.region)
    | _ -> Skip
  in
  Some (fold [] (Fun.flip List.cons) get_range cst)


let folding_range_pascaligo : Cst.Pascaligo.t -> FoldingRange.t list option =
 fun cst ->
  let open Cst_pascaligo.Fold in
  let get_range : some_node -> FoldingRange.t fold_control =
   fun (Some_node (x, b)) ->
    match b with
    | S_reg _ -> Continue (mk_region x.region)
    | _ -> Skip
  in
  Some (fold [] (Fun.flip List.cons) get_range cst)


let folding_range_jsligo : Cst.Jsligo.t -> FoldingRange.t list option =
 fun cst ->
  let open Cst_jsligo.Fold in
  let get_range : some_node -> FoldingRange.t fold_control =
   fun (Some_node (x, b)) ->
    match b with
    | S_reg (S_import _) -> Continue (mk_imports x.region)
    | S_reg _ -> Continue (mk_region x.region)
    | _ -> Skip
  in
  Some (fold [] (Fun.flip List.cons) get_range cst)


let on_req_folding_range : Path.t -> FoldingRange.t list option Handler.t =
 fun file ->
  Handler.with_cst file None
  @@ fun cst ->
  Handler.return
  @@ Dialect_cst.from_dialect
       { cameligo = folding_range_cameligo
       ; jsligo = folding_range_jsligo
       ; pascaligo = folding_range_pascaligo
       }
       cst
