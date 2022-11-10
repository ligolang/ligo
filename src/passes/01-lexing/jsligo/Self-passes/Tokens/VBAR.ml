(* Insertion of vertical bars ("vbars") for sum types *)

(* Vendor dependencies *)

module Std = Simple_utils.Std

(* Insertion *)

let vbar_insertion tokens =
  let open! Token in
  let rec aux acc insert_token = function
    VBAR _ as hd :: tl ->
      aux (hd :: acc) false tl
  | EQ _ as hd :: tl ->
      let acc =
        if insert_token then Token.ghost_VBAR :: acc else acc
      in List.rev_append (hd :: acc) tl
  | RBRACKET _ as hd :: tl ->
      aux (hd :: acc) true tl
  | hd :: tl ->
      aux (hd :: acc) insert_token tl
  | [] ->
      List.rev acc
  in aux [] false tokens

let vbar_insertion tokens : _ result =
  let open! Token in
  let rec aux acc = function
    VBAR _ as hd :: tl ->
      aux (vbar_insertion (hd :: acc)) tl
  | hd :: tl -> aux (hd :: acc) tl
  | [] -> List.rev acc
  in Stdlib.Ok (aux [] tokens)

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
              Injecting VBAR tokens in sum types.")
    | None -> ()
  in vbar_insertion tokens
