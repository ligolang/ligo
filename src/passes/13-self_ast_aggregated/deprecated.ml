open Ast_aggregated.Types
open Simple_utils.Trace
module Ligo_string = Simple_utils.Ligo_string
open Ligo_prim

module Table = struct
  type t = (Value_var.t * string) list

  let add (v : Value_var.t) (s : string) (t : t) : t = (v, s) :: t
  let empty : t = []
end

let format_string format_str =
  let rec parse acc = function
    | [] -> List.rev acc
    | '@' :: '.' :: rest -> parse ('\n' :: acc) rest
    | '@' :: '@' :: rest -> parse ('@' :: acc) rest
    | c :: rest -> parse (c :: acc) rest
  in
  String.of_char_list (parse [] (String.to_list format_str))


let build_table : Table.t -> expression -> Table.t =
 fun t e ->
  match e.expression_content with
  | E_let_in
      { let_binder; rhs = _; let_result = _; attributes = { deprecated = Some s; _ } } ->
    let binders = Linear_pattern.binders let_binder in
    let vars = List.map ~f:Binder.get_var binders in
    List.fold_right ~f:(fun v t -> Table.add v s t) ~init:t vars
  | _ -> t


let build_table : expression -> Table.t =
 fun e -> Helpers.fold_expression build_table Table.empty e


let warn ~raise ~table : unit -> expression -> unit =
 fun () e ->
  match e.expression_content with
  | E_variable v ->
    (match List.Assoc.find ~equal:Value_var.equal table v with
    | Some s ->
      raise.warning (`Self_ast_aggregated_deprecated (e.location, format_string s));
      ()
    | None -> ())
  | _ -> ()


let warn ~raise : expression -> unit =
 fun e ->
  let table = build_table e in
  Helpers.fold_expression (warn ~raise ~table) () e
