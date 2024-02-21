open Ast_aggregated.Types
open Simple_utils.Trace
module Ligo_string = Simple_utils.Ligo_string
open Ligo_prim

let format_string format_str =
  let rec parse acc = function
    | [] -> List.rev acc
    | '@' :: '.' :: rest -> parse ('\n' :: acc) rest
    | '@' :: '@' :: rest -> parse ('@' :: acc) rest
    | c :: rest -> parse (c :: acc) rest
  in
  let unique = function
    | '@' :: '!' :: rest ->
      let prefix, rest = List.split_while rest ~f:(fun c -> Char.(c <> '!')) in
      (match rest with
      | '!' :: rest -> Some prefix, rest
      | _ -> Some prefix, [])
    | rest -> None, rest
  in
  let unique, l = unique @@ String.to_list format_str in
  Option.map ~f:String.of_char_list unique, String.of_char_list (parse [] l)


module Table = struct
  module UID = struct
    type t = string [@@deriving compare, hash, sexp]
  end

  module Message = Core.Hashtbl.Make (Value_var)
  module Unique = Core.Hashtbl.Make (UID)

  type t =
    { used : unit Unique.t
    ; message : (UID.t option * string) Message.t
    }

  let add (v : Value_var.t) (fmt : string) (t : t) : unit =
    Message.set t.message ~key:v ~data:(format_string fmt)


  let find (v : Value_var.t) (t : t) : string option =
    match Message.find t.message v with
    | None -> None
    | Some (None, s) -> Some s
    | Some (Some uid, _) when Unique.mem t.used uid -> None
    | Some (Some uid, s) ->
      Unique.set t.used ~key:uid ~data:();
      Some s


  let create () : t =
    let message = Message.create () in
    let used = Unique.create () in
    { used; message }
end

let build_table : Table.t -> expression -> Table.t =
 fun t e ->
  match e.expression_content with
  | E_let_in
      { let_binder; rhs = _; let_result = _; attributes = { deprecated = Some s; _ } } ->
    let binders = Linear_pattern.binders let_binder in
    let vars = List.map ~f:Binder.get_var binders in
    List.iter ~f:(fun v -> Table.add v s t) vars;
    t
  | _ -> t


let build_table : expression -> Table.t =
 fun e -> Helpers.fold_expression build_table (Table.create ()) e


let warn ~raise ~table : unit -> expression -> unit =
 fun () e ->
  match e.expression_content with
  | E_variable v
    when (not (Location.is_dummy_or_generated e.location))
         && not (String.equal (Option.value_exn (Location.get_file e.location))#file "")
    ->
    (match Table.find v table with
    | Some s ->
      raise.warning (`Self_ast_aggregated_deprecated (e.location, s));
      ()
    | None -> ())
  | _ -> ()


let warn ~raise : expression -> unit =
 fun e ->
  let table = build_table e in
  Helpers.fold_expression (warn ~raise ~table) () e
