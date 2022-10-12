 (**
    This modules aims at finding the difference between two large tuple types
    for improved error message during type mismatch.
    For example when type [a * b * c * d * e] cannot unify with [c * d * e],
    the module will find the "diff" between the two, [a * b] here,
    so the message can be augmented into :
    > Cannot unify a * b * c * d * e with c * d * e
    > Diff :
    a  <
    b  <
    c  =  c
    d  =  d
    e  =  e
*)

(* module Location = Simple_utils.Location *)
open Ast_typed
open Ligo_prim
open Simple_utils

module Defs = struct
  type left  = type_expression
  type right = type_expression
  type eq    = unit
  type diff  = unit
  type state = unit
end

module Define = Diffing.Define(Defs)

module TeArg = struct
  (*
    The module will try to find the simplest diff between the two lists.
    To find the simplest one, we tell it how costly is a change.

    For example,
    from :  a  b  c  d  e
    to :    a  b  c  e
    The most trivial patch is :
    patch 1 : (keep a) (keep b) (keep c) (keep d) (REMOVE D) (keep e)
    But another possible patch is :
    patch 2 : (keep a) (keep b) (keep c) (REPLACE d BY e) (REMOVE e)

    For the first  patch, cost = 1 REMOVE = 1
    For the second patch, cost = 1 REPLACE + 1 REMOVE = 2
    weight patch 1 < weight patch 2, so the algorithm will prefer patch 1.

    Weights will be used to construct a "cost matrix" to find the lightest patch,
    see : https://en.wikipedia.org/wiki/Wagner%E2%80%93Fischer_algorithm
  *)
  let weight : Define.change -> int = function
    | Delete _ -> 1
    | Insert _ -> 1
    | Keep   _ -> 0
    | Change _ -> 1

  let test : Defs.state -> Defs.left -> Defs.right -> (Defs.eq, Defs.diff) result =
    fun _state te_l te_r ->
    match type_expression_eq (te_l, te_r) with
    | true -> Ok ()
    | false -> Error ()

  let update : Define.change -> Defs.state -> Defs.state = fun _change _state -> ()
end

module Diff = Define.Simple(TeArg)

type t = Define.patch

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
      Array.of_list @@ List.rev l
    in
    let r1 : ty_expr array = rows_to_te_array r1 in
    let r2 : ty_expr array = rows_to_te_array r2 in
    let diff : t = Diff.diff () r1 r2 in
    List.rev diff
  | _ -> [] (* Don't display any difference in other cases *)

module PP = struct
  let change ppf (c : Define.change ) : unit =
    let pp_te_custom ppf (te : ty_expr) : unit =
      Format.fprintf ppf "%a <hash:%d>" Ast_typed.PP.type_expression te (hash_type_expression te)
    in
    (* type ('left,'right,'eq,'diff) change =
    | Delete of 'left
    | Insert of 'right
    | Keep of 'left * 'right *' eq
    | Change of 'left * 'right * 'diff *)
    match c with
    | Delete  l              -> Format.fprintf ppf "Delete : %a@." pp_te_custom l
    | Insert  r              -> Format.fprintf ppf "Insert : %a@." pp_te_custom r
    | Keep    (l, r, _eq)    -> Format.fprintf ppf "Keep   : %a = %a@." pp_te_custom l pp_te_custom r
    | Change  (l, r, _diff)  -> Format.fprintf ppf "Change : %a vs. %a@." pp_te_custom l pp_te_custom r

  let t ppf (patch : t) : unit =
    match patch with
    | [] -> Format.fprintf ppf "@.No patch"
    | _ -> Format.fprintf ppf "@.Diff: @.%a" (PP_helpers.list_sep_d change) patch
end
