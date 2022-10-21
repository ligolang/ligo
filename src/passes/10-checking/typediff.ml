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



module rec TeArg : sig
  val weight : Define.change -> int
  val test :
    unit -> type_expression -> type_expression -> (unit, unit) result
  val update : Define.change -> unit -> unit
end = struct
  (*
    The module will try to find the simplest diff between the two lists.
    To find the simplest one, we tell it how costly is a change.

    For example,
    from :  a  b  c  d  e
    to :    a  b  c  e
    The most trivial patch is :
    patch 1 : (keep a) (keep b) (keep c) (REMOVE D) (keep e)
    But another possible patch is :
    patch 2 : (keep a) (keep b) (keep c) (REPLACE d BY e) (REMOVE e)

    For the first  patch, cost = 1 REMOVE = 1
    For the second patch, cost = 1 REPLACE + 1 REMOVE = 2
    weight patch 1 < weight patch 2, so the algorithm will prefer patch 1.

    Weights will be used to construct a "cost matrix" to find the lightest patch,
    see : https://en.wikipedia.org/wiki/Wagner%E2%80%93Fischer_algorithm
  *)
  let weight : Define.change -> int = function
    | Delete te -> List.length ( M.get_diff te (t_unit ()) )
    | Insert te -> List.length ( M.get_diff te (t_unit ()) )
    | Keep   _ -> 0
    | Change (te1, te2, _) -> List.length (M.get_diff te1 te2)

  let test : Defs.state -> Defs.left -> Defs.right -> (Defs.eq, Defs.diff) result =
    fun _state te_l te_r ->
    match type_expression_eq (te_l, te_r) with
    | true -> Ok ()
    | false -> Error ()

  let update : Define.change -> Defs.state -> Defs.state = fun _change _state -> ()
end

and Diff : sig
  val diff :
    unit -> type_expression array -> type_expression array -> Define.patch
end = Define.Simple(TeArg)

(* [t] and [get_diff] are put in this module [M]
   for enabling mutual recursivity with above modules.
   Because weights depend on [get_diff]
   and [get_diff] depends on weights.
   *)
and M : sig
  type t = Define.patch
  val get_diff : type_expression -> type_expression -> t
end = struct

  type t = Define.patch

  let get_diff : type_expression -> type_expression -> t = fun t1 t2 ->
    let rows_to_te_array : rows -> ty_expr array = fun r ->
      let fields : row_element Record.t    = r.fields in
      let l : (Label.t * row_element) list = Record.to_list fields in
      let l : row_element list             = List.map ~f:snd l in
      let l : type_expression list         = List.map ~f:(fun re -> re.associated_type) l in
      Array.of_list @@ List.rev l
    in
    List.rev @@
    match t1.type_content, t2.type_content with
    | T_record r1, T_record r2
      when (Record.is_tuple r1.fields)
      &&   (Record.is_tuple r2.fields) ->
      let r1 : ty_expr array = rows_to_te_array r1 in
      let r2 : ty_expr array = rows_to_te_array r2 in
      let diff : t = Diff.diff () r1 r2 in
       diff
    (* If one is a record and not the other
       the weight of the changes is basically the number of nodes
       of the type_expression tree *)
    | T_record r, _ ->
      let r : ty_expr array = rows_to_te_array r in
      let diff : t = Diff.diff () r (Array.of_list [t2]) in
      diff
    | _, T_record r -> 
      let r : ty_expr array = rows_to_te_array r in
      let diff : t = Diff.diff () r (Array.of_list [t1]) in
      diff
    | _ ->
      let r1 : ty_expr array = Array.of_list [t1] in
      let r2 : ty_expr array = Array.of_list [t2] in
      let diff : t = Diff.diff () r1 r2 in
      diff

end

type t = M.t
let get_diff = M.get_diff

module PP = struct

  let pp_list_newline pp_content ppf content =
    PP_helpers.list_sep pp_content (PP_helpers.tag "@,") ppf content
  let pp_te = Ast_typed.PP.type_expression
  let _pp_te_debug ppf (te : ty_expr) : unit =
    Format.fprintf ppf "%a <hash:%d>" pp_te te (hash_type_expression te)

  let rec change ppf (c : Define.change ) : unit =
    let self = change in
    (* type ('left,'right,'eq,'diff) change =
    | Delete of 'left
    | Insert of 'right
    | Keep of 'left * 'right *' eq
    | Change of 'left * 'right * 'diff *)
    match c with
    | Delete  l              -> Format.fprintf ppf "- %a" pp_te l
    | Insert  r              -> Format.fprintf ppf "+ %a" pp_te r
    | Keep    (l, _r, _eq)   -> Format.fprintf ppf "  %a" pp_te l
    | Change  (l, r, _diff)  -> pp_list_newline self ppf [Delete l; Insert r]

  let t ppf (patch : t) : unit =
    match patch with
    | [] -> Format.fprintf ppf "@.No patch"
    | _ -> Format.fprintf ppf "@.@[<v>Difference between the types:@,%a@]" (pp_list_newline change)  patch
end
