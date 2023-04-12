open Lsp.Types
open Linol_lwt.Jsonrpc2
module Loc = Simple_utils.Location
module Region = Simple_utils.Region
module Pos = Simple_utils.Pos
module LSet = Caml.Set.Make (Loc)
module Hashtbl = Caml.Hashtbl

let ( @. ) f g x = f (g x)
let bind_option x f = Option.bind x ~f
let file_start_position = Position.create ~character:0 ~line:0
let file_end_position = Position.create ~character:0 (* FIXME *) ~line:1000000000
let whole_file_range = Range.create ~end_:file_end_position ~start:file_start_position

let pos_to_position (pos : Pos.t) : Position.t =
  let line_diff = 1 in
  let character_diff = 0 in
  Position.create
    ~line:(pos#line - line_diff)
    ~character:(pos#point_num - pos#point_bol - character_diff)


let region_to_range (region : Region.t) : Range.t =
  Range.create ~start:(pos_to_position region#start) ~end_:(pos_to_position region#stop)


let region_to_location : Region.t -> Lsp.Types.Location.t =
 fun region ->
  Lsp.Types.Location.create
    ~uri:(DocumentUri.of_path region#file)
    ~range:(region_to_range region)


let position_of_location (l : Loc.t) : Position.t option =
  match l with
  | Virtual _ -> None
  | File region -> Some (pos_to_position region#start)


let position_equal : Position.t -> Position.t -> bool =
 fun p1 p2 -> p1.line = p2.line && p1.character = p2.character


let position_leq (p1 : Position.t) (p2 : Position.t) : bool =
  p1.line < p2.line || (p1.line = p2.line && p1.character <= p2.character)


(** Is the [small] range contained in the [big] one? *)
let range_inside ~(big : Range.t) ~(small : Range.t) : bool =
  position_leq big.start small.start && position_leq small.end_ big.end_


(** Minimal range that contains both given ranges *)
let range_cover (r1 : Range.t) (r2 : Range.t) : Range.t =
  let position_max (p1 : Position.t) (p2 : Position.t) : Position.t =
    if position_leq p1 p2 then p2 else p1
  in
  let position_min (p1 : Position.t) (p2 : Position.t) : Position.t =
    if position_leq p1 p2 then p1 else p2
  in
  Range.create
    ~start:(position_min r1.start r2.start)
    ~end_:(position_max r1.end_ r2.end_)


(** Minimal range that contains all given ranges *)
let range_cover_nseq (ranges : Range.t Simple_utils.Utils.nseq) : Range.t =
  let h, t = ranges in
  List.fold ~f:range_cover ~init:h t


let location_to_range (l : Loc.t) : Range.t option =
  match l with
  | Virtual _ -> None
  | File region -> Some (region_to_range region)


let dummy_range =
  let dummy_start = Position.create ~character:0 ~line:0 in
  let dummy_end = Position.create ~character:1 ~line:0 in
  Range.create ~end_:dummy_end ~start:dummy_start


let position_le (position_l : Position.t) (position_r : Position.t) : Bool.t =
  position_l.line < position_r.line
  || (position_l.line = position_r.line && position_l.character <= position_r.character)


let is_position_in_range (position : Position.t) (range : Range.t) : Bool.t =
  position_le range.start position && position_le position range.end_


let uri_location_cmp : DocumentUri.t -> Loc.t -> bool =
 fun uri -> function
  | File region -> DocumentUri.equal uri (DocumentUri.of_path @@ region#file)
  | Virtual _ -> false


let references_getter : Scopes.def -> LSet.t =
 fun def ->
  match def with
  | Variable vdef -> LSet.add vdef.range vdef.references
  | Type tdef -> LSet.add tdef.range tdef.references
  | Module mdef -> LSet.add mdef.range mdef.references


let get_references_of_file : DocumentUri.t -> Scopes.def -> LSet.t =
 fun uri def -> LSet.filter (uri_location_cmp uri) @@ references_getter def


let name_getter : Scopes.def -> string = function
  | Variable vdef -> vdef.name
  | Type tdef -> tdef.name
  | Module mdef -> mdef.name


let get_location : Scopes.def -> Loc.t = function
  | Variable vdef -> vdef.range
  | Type tdef -> tdef.range
  | Module mdef -> mdef.range


let is_reference : Position.t -> DocumentUri.t -> Scopes.def -> bool =
 fun pos uri defintion ->
  let check_pos : Loc.t -> bool = function
    | File reg -> is_position_in_range pos @@ region_to_range reg
    | Virtual _ -> false
  in
  LSet.exists check_pos @@ get_references_of_file uri defintion


let hashtbl_find_map : ('a -> 'b -> 'c option) -> ('a, 'b) Hashtbl.t -> 'c list =
 fun f h ->
  let go k v l =
    match f k v with
    | Some x -> x :: l
    | None -> l
  in
  Hashtbl.fold go h []


let position_to_string (position : Position.t) =
  "Position { line: "
  ^ Int.to_string position.line
  ^ ", character: "
  ^ Int.to_string position.character
  ^ " }"


let range_to_string (range : Range.t) =
  let json = Range.yojson_of_t range in
  Yojson.Safe.to_string json


let defintion_to_string (def : Scopes.def) =
  Format.asprintf "%a" Scopes.PP.definitions [ def ]


let location_to_string (location : Loc.t) = Format.asprintf "%a" Loc.pp location

let checking_error_to_string (error : Checking.Errors.typer_error) : string =
  let display_format = Simple_utils.Display.Human_readable in
  Format.asprintf
    "%a"
    (Checking.Errors.error_ppformat ~display_format ~no_colour:false)
    error


let pp_type_expression
    :  syntax:Syntax_types.t
    -> [ `Core of Ast_core.type_expression | `Typed of Ast_typed.type_expression ]
    -> string
  =
 fun ~syntax te ->
  let cte =
    match te with
    | `Core cte -> cte
    | `Typed tte -> Checking.untype_type_expression tte
  in
  let ty_expr_to_string =
    let raise = Simple_utils.Trace.raise_failwith "LSP" in
    let open Simple_utils.Function in
    Buffer.contents
    <@ Decompile.Helpers.specialise_and_print_ty syntax
    <@ Decompile.Of_core.decompile_ty_expr ~raise ~syntax
  in
  try ty_expr_to_string cte with
  | _ ->
    (match te with
    | `Core cte -> Format.asprintf "%a" Ast_core.PP.type_expression cte
    | `Typed tte -> Format.asprintf "%a" Ast_typed.PP.type_expression tte)


let uri_extension : DocumentUri.t -> string option =
  snd @. Filename.split_extension @. DocumentUri.to_path


let get_syntax = Syntax.of_ext_opt ~support_pascaligo:true @. uri_extension

let get_comment syntax =
  let block =
    match syntax with
    | Syntax_types.CameLIGO -> Preprocessing_cameligo.Config.block
    | Syntax_types.JsLIGO -> Preprocessing_jsligo.Config.block
    | Syntax_types.PascaLIGO -> Preprocessing_pascaligo.Config.block
  in
  match block with
  | Some x -> x#opening, x#closing
  | _ -> "", ""


type module_pp_mode =
  { module_keyword : string
  ; import_keyword : string
  ; sign_on_definition : string option
  ; sign_on_import : string option
  ; open_ : string
  ; close : string
  ; semicolon_at_the_end : bool
  }

let cameligo_module =
  { module_keyword = "module"
  ; import_keyword = "module"
  ; sign_on_definition = Some "="
  ; sign_on_import = Some "="
  ; open_ = "struct"
  ; close = "end"
  ; semicolon_at_the_end = false
  }


let jsligo_module =
  { module_keyword = "namespace"
  ; import_keyword = "import"
  ; sign_on_definition = None
  ; sign_on_import = Some "="
  ; open_ = "{"
  ; close = "}"
  ; semicolon_at_the_end = true
  }


let pascaligo_module =
  { module_keyword = "module"
  ; import_keyword = "module"
  ; sign_on_definition = Some "is"
  ; sign_on_import = Some "is"
  ; open_ = "{"
  ; close = "}"
  ; semicolon_at_the_end = false
  }


let print_module_with_description
    : module_pp_mode -> string * string -> Scopes.Types.mdef -> string
  =
 fun description (opening_comment, closing_comment) mdef ->
  match mdef.mod_case with
  | Def _ ->
    description.module_keyword
    ^ " "
    ^ mdef.name
    ^ (Option.value ~default:" "
      @@ Option.map ~f:(fun s -> " " ^ s ^ " ") description.sign_on_definition)
    ^ description.open_
    ^ " "
    ^ opening_comment
    ^ " ... " (* TODO: print module*)
    ^ closing_comment
    ^ " "
    ^ description.close
  | Alias module_path_list ->
    description.import_keyword
    ^ " "
    ^ mdef.name
    ^ (Option.value ~default:" "
      @@ Option.map ~f:(fun s -> " " ^ s ^ " ") description.sign_on_import)
    ^ (module_path_list
      |> List.map ~f:(String.split ~on:'#')
      |> List.map ~f:(Fun.flip List.nth_exn 0)
      |> String.concat ~sep:".")


let print_module : Syntax_types.t -> Scopes.Types.mdef -> string = function
  | CameLIGO -> print_module_with_description cameligo_module (get_comment CameLIGO)
  | JsLIGO -> print_module_with_description jsligo_module (get_comment JsLIGO)
  | PascaLIGO -> print_module_with_description pascaligo_module (get_comment PascaLIGO)


let range
    ((line_start, character_start) : int * int)
    ((line_end, character_end) : int * int)
    : Range.t
  =
  Range.create
    ~start:(Position.create ~line:line_start ~character:character_start)
    ~end_:(Position.create ~line:line_end ~character:character_end)


let interval (line : int) (character_start : int) (character_end : int) : Range.t =
  Range.create
    ~start:(Position.create ~line ~character:character_start)
    ~end_:(Position.create ~line ~character:character_end)


let point (line : int) (character : int) : Range.t =
  let position = Position.create ~line ~character in
  Range.create ~start:position ~end_:position


let parsing_error_to_string (err : Parsing.Errors.t) : string =
  let ({ content = { message; _ }; _ } : Simple_utils.Error.t) =
    Parsing.Errors.error_json err
  in
  message


type dialect_cst =
  | CameLIGO_cst of Parsing.Cameligo.CST.t
  | JsLIGO_cst of Parsing.Jsligo.CST.t
  | PascaLIGO_cst of Parsing.Pascaligo.CST.t

type parsing_raise = (Parsing.Errors.t, Main_warnings.all) Simple_utils.Trace.raise

exception Fatal_cst_error of string

let get_cst ~(strict : bool) (syntax : Syntax_types.t) (code : string)
    : (dialect_cst, string) result
  =
  let buffer = Caml.Buffer.of_seq (Caml.String.to_seq code) in
  (* Warnings and errors will be reported to the user via diagnostics, so we
     ignore them here unless the strict mode is enabled. *)
  let raise : parsing_raise =
    { error = (fun err -> raise @@ Fatal_cst_error (parsing_error_to_string err))
    ; warning = (fun _ -> ())
    ; log_error =
        (fun err -> if strict then raise @@ Fatal_cst_error (parsing_error_to_string err))
    ; fast_fail = false
    }
  in
  try
    match syntax with
    | CameLIGO ->
      Ok (CameLIGO_cst (Parsing.Cameligo.parse_string ~preprocess:false ~raise buffer))
    | JsLIGO ->
      Ok (JsLIGO_cst (Parsing.Jsligo.parse_string ~preprocess:false ~raise buffer))
    | PascaLIGO ->
      Ok (PascaLIGO_cst (Parsing.Pascaligo.parse_string ~preprocess:false ~raise buffer))
  with
  | Fatal_cst_error err -> Error err


let rec take (n : int) (xs : 'a list) : 'a list =
  if n <= 0
  then []
  else (
    match xs with
    | [] -> []
    | x :: xs -> x :: take (n - 1) xs)


let value_map ~f ~default value = Option.value (Option.map ~f value) ~default
