open Ligo_prim
open Ast_aggregated.Types
module Trace = Simple_utils.Trace

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

  type t =
    { used : (UID.t, unit) Hashtbl.t
    ; message : (Value_var.t, UID.t option * string) Hashtbl.t
    }

  let add (v : Value_var.t) (fmt : string) (t : t) : unit =
    Hashtbl.set t.message ~key:v ~data:(format_string fmt)


  let find (v : Value_var.t) (t : t) : string option =
    match Hashtbl.find t.message v with
    | None -> None
    | Some (None, s) -> Some s
    | Some (Some uid, _) when Hashtbl.mem t.used uid -> None
    | Some (Some uid, s) ->
      Hashtbl.set t.used ~key:uid ~data:();
      Some s


  let create () : t =
    let message = Hashtbl.create (module Value_var) in
    let used = Hashtbl.create (module UID) in
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


let warn ~(raise : _ Trace.raise) ~table : unit -> expression -> unit =
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
