open Core
module Region = Simple_utils.Region
module File = Simple_utils.File
module ModRes = Preprocessor.ModRes
module Directive = Preprocessor.Directive
module Cst_cameligo = Cst_cameligo.CST
module Cst_jsligo = Cst_jsligo.CST

(* TODO: [Directive]'s can also be inside E_raw_code (inline michelson code)
         extract those [Directive]'s later
   Also check if [Directive]'s can occur at top-level of module *)

(** Extracts all top-level directives from a CameLIGO CST. Does not visit
    top-level modules. *)
let extract_directives_cameligo (cst : Cst_cameligo.t) : Directive.t list =
  List.filter_map ~f:(function
      | Cst_cameligo.D_Directive d -> Some d
      | _ -> None)
  @@ Nonempty_list.to_list cst.decl

(** Extracts all top-level directives from a JsLIGO CST. Does not visit
    top-level namespaces. *)
let extract_directives_jsligo (cst : Cst_jsligo.t) : Directive.t list =
  List.filter_map ~f:(function
      | Cst_jsligo.S_Directive d -> Some d
      | _ -> None)
  @@ (Nonempty_list.to_list cst.statements |> List.map ~f:fst)

(** Given a directive (see {!extract_directives_cameligo} and {!extract_directives_jsligo}
    to extract directives) with an [#include] or [#import], this function will try to
    return the range of that directive as well as the resolved path to that directive.

    @param relative_to_dir The directory with the file containing the directive.
    @param mod_res Used to resolve imports to LIGO registry packages. *)
let extract_range_and_target
    ~(normalize : Path.normalization)
    ~(relative_to_dir : Path.t)
    ~(mod_res : ModRes.t option)
    : Directive.t -> (Range.t * Path.t) option
  =
  let get_range_and_target (file_path : string Region.reg) =
    let range = Range.of_region file_path.region in
    let target =
      let local_target = Path.concat relative_to_dir file_path.value in
      match File.exists (Path.to_string local_target) with
      | Some _ -> Some local_target
      | None ->
        let open ModRes in
        let inclusion_paths =
          get_dependencies ~file:(Path.to_string relative_to_dir) mod_res
        in
        Option.map ~f:normalize
        @@ find_external_file ~file:file_path.value ~inclusion_paths
    in
    Option.map target ~f:(fun target -> range, target)
  in
  function
  | PP_Include d -> get_range_and_target d#file_path
  | PP_Import d -> get_range_and_target d#file_path
  | _ -> None
