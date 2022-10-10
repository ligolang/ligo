(**
    This modules aims at finding the difference between two large tuple types
    for improved error message during type mismatch.
    For example when type [a * b * c * d * e] cannot unify with [c * d * e],
    the module will find the "diff" between the two, [a * b] here,
    so the message can be augmented into :
    > Cannot unify a * b * c * d * e with c * d * e
    > Diff : a * b
*)

(* module Location = Simple_utils.Location *)
open Ast_typed
open Ligo_prim
open Simple_utils


module TyExprDiffComparable = struct
  type t = type_expression
  let compare = compare_type_expression
end

module Diff = Simple_diff.Make(TyExprDiffComparable)

type t = Diff.diff list

let get_diff : type_expression -> type_expression -> t = fun t1 t2 ->
  match t1.type_content, t2.type_content with
  | T_record r1, T_record r2
    when (Record.is_tuple r1.fields)
    &&   (Record.is_tuple r2.fields) ->
    let rows_to_te_array : rows -> ty_expr array = fun r ->
      let fields : row_element Record.t    = r.fields in
      let l : (Label.t * row_element) list = Record.to_list fields in
      let l : row_element list             = List.map ~f:snd l in
      let l : type_expression list         = List.map ~f:(fun re -> re.associated_type) l in
      Array.of_list l
    in
    let r1 : ty_expr array = rows_to_te_array r1 in
    let r2 : ty_expr array = rows_to_te_array r2 in
    let diff : t = Diff.get_diff r1 r2 in
    diff
  | _ -> [] (* Don't display any difference in other cases *)

module PP = struct
  let diff ppf (c : Diff.diff ) : unit =
    let pp_array pp_content ppf arr =
      PP_helpers.list_sep_d pp_content ppf (Array.to_list arr)
    in
    let pp_te_array ppf (tes : ty_expr array) =
      pp_array Ast_typed.PP.type_expression ppf tes
    in
    match c with
    | Deleted tes -> (
      Format.fprintf ppf "(Deleted: @.%a@.)" pp_te_array tes
    )
    | Added tes -> (
      Format.fprintf ppf   "(Added: @.%a@.)" pp_te_array tes
    )
    | Equal tes -> (
      Format.fprintf ppf   "(Equal: @.%a@.)" pp_te_array tes
    )

  let t ppf (td : t) : unit =
    match td with
    | [] -> Format.fprintf ppf ""
    | _ -> Format.fprintf ppf "@.Diff: @.%a" (PP_helpers.list_sep_d diff) td
end
