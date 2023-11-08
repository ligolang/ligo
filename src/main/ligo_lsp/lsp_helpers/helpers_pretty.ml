(* This module collects pretty printing stuff that we use in hovers
   and some functions for debug printing. It does not contain reexports/wrappers for
   LIGO CST/AST printers, since such reexports are contained in [Ligo_interface] *)

open Imports

let pp_with_yojson (f : 'a -> Yojson.Safe.t) : 'a Fmt.t =
 fun formatter link -> Yojson.Safe.pretty_print formatter @@ f link


let show_with_yojson (f : 'a -> Yojson.Safe.t) (x : 'a) : string =
  Yojson.Safe.to_string @@ f x


let doc_to_string ~(width : int) (doc : PPrint.document) : string =
  let buffer = Buffer.create 131 in
  PPrint.ToBuffer.pretty 1.0 width buffer doc;
  Buffer.contents buffer


let default_line_width_for_formatted_file = 80
let default_line_width_for_hovers = 60

let get_comment syntax =
  let block =
    match syntax with
    | Syntax_types.CameLIGO -> Preprocessing_cameligo.Config.block
    | Syntax_types.JsLIGO -> Preprocessing_jsligo.Config.block
  in
  match block with
  | Some x -> x#opening, x#closing
  | _ -> "", ""


(* In hovers and completion some types can be unresolved
   (e.g. if the current code is invalid). We're showing
    that some thing has no inferred type printing its type as
   e.g. [(* Unresolved *)] for CameLIGO  *)
let unresolved_type_as_comment syntax =
  let opening_comment, closing_comment = get_comment syntax in
  opening_comment ^ " Unresolved " ^ closing_comment


type module_pp_mode =
  { module_keyword : string
  ; import_keyword : string
  ; sign_on_signature : string
  ; sign_on_import : string option
  ; open_ : string
  ; close : string
  ; semicolon_at_the_end : bool
  }

let cameligo_module =
  { module_keyword = "module"
  ; import_keyword = "module"
  ; sign_on_signature = ":"
  ; sign_on_import = Some "="
  ; open_ = "struct"
  ; close = "end"
  ; semicolon_at_the_end = false
  }


let jsligo_module =
  { module_keyword = "namespace"
  ; import_keyword = "import"
  ; sign_on_signature = "implements"
  ; sign_on_import = Some "="
  ; open_ = "{"
  ; close = "}"
  ; semicolon_at_the_end = true
  }


let print_module_with_description
    :  Syntax_types.t -> project_root:Path.t option -> module_pp_mode -> string
    -> Scopes.Types.mdef -> [> `List of Lsp.Types.MarkedString.t list ]
  =
 fun syntax ~project_root description signature mdef ->
  let open Lsp.Types in
  let project_root = Option.map project_root ~f:Path.to_string in
  let language = Some (Syntax.to_string syntax) in
  match mdef.mod_case with
  | Def _ ->
    let value =
      description.module_keyword
      ^ " "
      ^ Format.asprintf "%a" (Scopes.PP.mod_name project_root) mdef.name
      ^ " "
      ^ description.sign_on_signature
      ^ " "
      ^ signature
    in
    `List [ MarkedString.{ language; value } ]
  | Alias { module_path; file_name; resolved_module = _ } ->
    let marked_string =
      match file_name with
      | Some file_name ->
        let value =
          Format.asprintf
            {|#import %a "%a"|}
            (Scopes.PP.mod_name project_root)
            (Filename file_name)
            (Scopes.PP.mod_name project_root)
            mdef.name
        in
        MarkedString.{ language; value }
      | None ->
        let alias_rhs =
          Option.value_map
            ~default:
              (module_path
              |> List.map ~f:Scopes.Types.Uid.to_name
              |> String.concat ~sep:".")
            ~f:(fun file_name ->
              Format.asprintf "%a" (Scopes.PP.mod_name project_root) (Filename file_name))
            file_name
        in
        let value =
          description.import_keyword
          ^ " "
          ^ Format.asprintf "%a" (Scopes.PP.mod_name project_root) mdef.name
          ^ Option.value_map
              ~default:" "
              ~f:(fun s -> " " ^ s ^ " ")
              description.sign_on_import
          ^ alias_rhs
        in
        MarkedString.{ language; value }
    in
    `List [ marked_string ]


let print_module
    :  Syntax_types.t -> project_root:Path.t option -> string -> Scopes.Types.mdef
    -> [> `List of Lsp.Types.MarkedString.t list ]
  = function
  | CameLIGO -> print_module_with_description CameLIGO cameligo_module
  | JsLIGO -> print_module_with_description JsLIGO jsligo_module


(* Functions made for debugging *)

let location_to_string (location : Loc.t) = Format.asprintf "%a" Loc.pp location

let checking_error_to_string (error : Checking.Errors.typer_error) : string =
  let display_format = Simple_utils.Display.Human_readable in
  Format.asprintf
    "%a"
    (Checking.Errors.error_ppformat ~display_format ~no_colour:false)
    error


let parsing_error_to_string (err : Parsing.Errors.t) : string =
  let ({ content = { message; _ }; _ } : Simple_utils.Error.t) =
    Parsing.Errors.error_json err
  in
  message


let passes_error_to_string (error : Passes.Errors.t) : string =
  let display_format = Simple_utils.Display.Human_readable in
  Format.asprintf
    "%a"
    (Passes.Errors.error_ppformat ~display_format ~no_colour:false)
    error
