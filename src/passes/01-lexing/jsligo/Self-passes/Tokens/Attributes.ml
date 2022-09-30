(* Collecting attributes from comments *)

(* Vendor dependencies *)

module Std = Simple_utils.Std

(* Insertion *)

let attribute_regexp = Str.regexp "@\\([a-zA-Z:0-9_]+\\)"

let collect_attributes str : string list =
  let x : Str.split_result list =
    Str.full_split attribute_regexp str in
  let f (acc : string list) = function
    Str.Text _ -> acc
  | Str.Delim string -> string :: acc
  in List.rev @@ List.fold_left ~f ~init:[] x

let collect_attributes tokens =
  let open! Token in
  let rec inner result = function
    LineCom  c :: tl
  | BlockCom c :: tl ->
      let attrs = collect_attributes c#payload in
      let apply key = Token.mk_attr ~key c#region in
      let attrs = List.map ~f:apply attrs
      in inner (attrs @ result) tl
  | hd :: tl -> inner (hd :: result) tl
  | [] -> List.rev result
  in Ok (inner [] tokens)

(* Exported *)

let filter
    : ?print_passes:Std.t ->
      add_warning:(Main_warnings.all -> unit) ->
      Token.t list ->
      _ result =
  fun ?print_passes ~add_warning:_ tokens -> (* No warning registered *)
  let () =
    match print_passes with
      Some std ->
        Std.(add_line std.out
             "Running JsLIGO token self-pass: \
              Extraction of attributes from comments.")
    | None -> ()
  in collect_attributes tokens
