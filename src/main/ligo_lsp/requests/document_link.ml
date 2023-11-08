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


let extract_link_from_directive
    ~(relative_to_dir : Path.t)
    ~(mod_res : Preprocessor.ModRes.t option)
    : Preprocessor.Directive.t -> DocumentLink.t option
  =
  let get_document_link (file_path : string Region.reg) =
    let range = Range.of_region file_path.region in
    let target =
      let local_target = Path.concat relative_to_dir file_path.value in
      match Simple_utils.File.exists (Path.to_string local_target) with
      | Some _ -> Some local_target
      | None ->
        let open Preprocessor.ModRes in
        let inclusion_paths =
          get_dependencies ~file:(Path.to_string relative_to_dir) mod_res
        in
        (match find_external_file ~file:file_path.value ~inclusion_paths with
        | Some resolved -> Option.some @@ Path.from_absolute resolved
        | None -> None)
    in
    Option.map target ~f:(fun target -> DocumentLink.create ~range ~target ())
  in
  function
  | PP_Include d -> get_document_link d#file_path
  | PP_Import d -> get_document_link d#file_path
  | _ -> None


let on_req_document_link (file : Path.t) : DocumentLink.t list option handler =
  let@ () = send_debug_msg @@ "On document_link:" ^ Path.to_string file in
  let dir = Path.dirname file in
  let@ mod_res = ask_mod_res in
  let@ directives_opt =
    with_cst file None
    @@ fun cst ->
    return
    @@ Option.some
    @@ Dialect_cst.from_dialect
         { cameligo = extract_directives_cameligo; jsligo = extract_directives_jsligo }
         cst
  in
  return
  @@ Option.map
       ~f:
         (List.filter_map
            ~f:(extract_link_from_directive ~relative_to_dir:dir ~mod_res:!mod_res))
  @@ directives_opt
