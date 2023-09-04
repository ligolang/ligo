open Handler
open Lsp_helpers

(* TODO: [Directive]'s can also be inside E_raw_code (inline michelson code)
         extract those [Directive]'s later
   Also check if [Directive]'s can occur at top-level of module *)

let extract_directives_cameligo (cst : Parsing.Cameligo.CST.t)
    : Preprocessor.Directive.t list
  =
  List.filter_map ~f:(function
      | Cst_cameligo.CST.D_Directive d -> Some d
      | _ -> None)
  @@ Simple_utils.Utils.nseq_to_list cst.decl


let extract_directives_jsligo (cst : Parsing.Jsligo.CST.t) : Preprocessor.Directive.t list
  =
  List.filter_map ~f:(function
      | Cst_jsligo.CST.S_Directive d -> Some d
      | _ -> None)
  @@ (Simple_utils.Utils.nseq_to_list cst.statements |> List.map ~f:fst)


let extract_directives_pascaligo (cst : Parsing.Pascaligo.CST.t)
    : Preprocessor.Directive.t list
  =
  List.filter_map ~f:(function
      | Cst_pascaligo.CST.D_Directive d -> Some d
      | _ -> None)
  @@ Simple_utils.Utils.nseq_to_list cst.decl


let extract_link_from_directive ~(relative_to_dir : Path.t)
    : Preprocessor.Directive.t -> DocumentLink.t option
  = function
  | PP_Include d ->
    let range = Range.of_region d#file_path.region
    and target = Path.concat relative_to_dir d#file_path.value in
    Option.some @@ DocumentLink.create ~range ~target ()
  | PP_Import d ->
    let range = Range.of_region d#file_path.region
    and target = Path.concat relative_to_dir d#file_path.value in
    Option.some @@ DocumentLink.create ~range ~target ()
  | _ -> None


let on_req_document_link (file : Path.t) : DocumentLink.t list option handler =
  let@ () = send_debug_msg @@ "On document_link:" ^ Path.to_string file in
  let dir = Path.dirname file in
  let@ directives_opt =
    with_cst file None
    @@ fun cst ->
    return
    @@ Option.some
    @@ Dialect_cst.from_dialect
         { cameligo = extract_directives_cameligo
         ; jsligo = extract_directives_jsligo
         ; pascaligo = extract_directives_pascaligo
         }
         cst
  in
  return
  @@ Option.map ~f:(List.filter_map ~f:(extract_link_from_directive ~relative_to_dir:dir))
  @@ directives_opt
