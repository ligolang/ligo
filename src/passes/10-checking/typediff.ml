 (**
    This modules aims at finding the difference between two large tuple types
    for improved error message during type mismatch.
    For example when type [a * b * c * d * e] cannot unify with [c * d * e],
    the module will find the "diff" between the two, [a * b] here,
    so the message can be augmented into :
    > Cannot unify a * b * c * d * e * y * z with c * d * f * y * z
    > Diff :
    - a
    - b
      c
      d
    - e
    + f
      y
      z
*)

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

let rows_to_te_list : rows -> ty_expr list = fun r ->
  let fields : row_element Record.t    = r.fields in
  let l : (Label.t * row_element) list = Record.to_list fields in
  let l : row_element list             = List.map ~f:snd l in
  let l : type_expression list         = List.map ~f:(fun re -> re.associated_type) l in
  l

let rows_to_te_array : rows -> ty_expr array = fun r ->
  Array.of_list @@ List.rev @@ rows_to_te_list r

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

    ---

    In above example, the weight of a INSERT/DELETE/CHANGE was assumed to always be 1,
    but all changes are not created equal.
    In below example, there is a [string] added, and a [nat->int] change in the big tuple.
      from :          tuple1=(int * tez * int * nat)
      to   : string * tuple2=(int * tez * nat * nat) 
             ^^^^^^                       ^^^
    Here, if all changes are weighted 1, then the diff would be :
      patch 1 : (CHANGE tuple1 to STRING), (ADD tuple2)
    However, we would rather like the following patch :
      patch 2 : (ADD string), (CHANGE tuple1 to tuple2)
    
    More generally, in those cases when two big tuples t1 and t2 are similar, we prefer a (CHANGE t1 to t2).
    To do this, we account for the type_expression in the computation of the weight. For example :
      1. weight of (INSERT  int)                     = 1
      2. weight of (INSERT  nat * int * tez)         = 3
      3. weight of (CHANGE  string TO int)           = 1
      4. weight of (CHANGE  int    * int * tez
                        TO  string * nat * string)   = 3
                            ^^^^^^   ^^^   ^^^^^^
      5. weight of (CHANGE  int * int * tez
                        TO  int * int * nat)         = 1
                                        ^^^
    In this last example (5.), the only real change is [tez] -> [nat], so its weight is 1.
    More generally :
      * For singleton types, weights of changes is 1
      * For tuples, weight of INSERT / DELETE is the length of the tuple
      * Weight of CHANGE tuple_a to tuple_b is the number of changes to do within the two tuples (see example 5. above)

  *)

  (* let weight_simple : Define.change -> int = function
    | Delete _ -> 1
    | Insert _ -> 1
    | Keep   _ -> 0
    | Change _ -> 1 *)

  let rec weight : Define.change -> int = function
    | Delete te
    | Insert te -> (
        match te.type_content with
        | T_record r -> List.length @@ rows_to_te_list r
        | _          -> 1
        )
    | Keep   _ -> 0
    | Change (te1, te2, _) -> (
        match te1.type_content, te2.type_content with
        | T_record r1, T_record r2 ->
          (* We consider the weight to change a record into another
             as the weight of the diff between them,
             so that "close" records are gather together in the diff *)
          let diff = Diff.diff () (rows_to_te_array r1) (rows_to_te_array r2) in
          let diff_weights = List.map ~f:(fun change -> weight change) diff in
          let total_weight = List.fold ~init:0 ~f:(+) diff_weights in
          total_weight
        | T_record r, _
        | _         , T_record r -> List.length @@ rows_to_te_list r
        | _                      -> 1
        )
  
  let test : Defs.state -> Defs.left -> Defs.right -> (Defs.eq, Defs.diff) result =
    fun _state te_l te_r ->
    match type_expression_eq (te_l, te_r) with
    | true -> Ok ()
    | false -> Error ()

  let update : Define.change -> Defs.state -> Defs.state = fun _change _state -> ()
end

(*
  The [Diff] module will compute the minimal list of changes between two lists of type_expressions,
  using the above computation of "weights" of type_expression changes.
*)
and Diff : sig
  val diff :
    unit -> type_expression array -> type_expression array -> Define.patch
end = Define.Simple(TeArg)


type t = Define.patch

let get_diff : type_expression -> type_expression -> t = fun t1 t2 ->
  List.rev @@
  match t1.type_content, t2.type_content with
  (* When the two types are records, call the [Diffing] to get the optimal diff *)
  | T_record r1, T_record r2
    when (Record.is_tuple r1.fields)
    &&   (Record.is_tuple r2.fields) ->
    (* let () = Printf.printf "[DEBUG] Matching get_diff case (T_record, T_record) <%d> <%d> )\n" (hash_ty_expr t1) (hash_ty_expr t2) in *)
    let r1 : ty_expr array = rows_to_te_array r1 in
    let r2 : ty_expr array = rows_to_te_array r2 in
    let diff : t = Diff.diff () r1 r2 in
    diff
  (* Other types are considered "singletons", in the sense they are not a collection of te themselves, so there is no need to diff them.
     TODO NP : Actually that's wrong, if we get a (a * b * c * d * e) list vs. (b * a * c * d * e) list,
    a [list] contains only one type but we'd like to diff the inner type, i.e. the two confusing tuples inside the list type. *)
  (* TODO NP : Arrow types can be diffed too *)
  | _ -> []

module PP = struct

  (* The [ANSI] module uses Ocaml Format's semantic tags to enable styling of output.
     After calling [add_ansi_marking] on the given [ppf],
     you can add colored text by enclosing it with a semantic tag, like this :
      [Format.fprintf ppf "normal text, @{<red>some red text@}, normal text again"

    The module will basically surround "some red text"
    with [mark_open_stag "red"] string output as prefix
    and [mark_close_stag "red"] string output as suffix.

    See :
    https://ocamlpro.com/blog/2020_06_01_fr_tutoriel_format
    https://hal.archives-ouvertes.fr/hal-01503081/file/format-unraveled.pdf
  *)
  module ANSI = struct

    type style =
    | Normal
    | Red
    | Green

    let style_of_stag = function
    | Format.String_tag s ->
      begin match s with
      | "normal" -> Normal
      | "red"    -> Red
      | "green"  -> Green
      | _ -> failwith "Unknown ANSI style" (* TODO NP : How to report errors properly ? *)
      end
    | _ -> failwith "Unknown ANSI semantic tag" (* TODO NP : How to report errors properly ? *)

    let closing_style = function
    | Red | Green | Normal -> Normal

    let code_of_style = function
    | Normal -> 0
    | Red    -> 31
    | Green  -> 32

    let ansi_of_code code =
      Format.sprintf "\027[%dm" code

    let ansi_of_style style = ansi_of_code @@ code_of_style style

    let mark_open_stag t = ansi_of_style @@ style_of_stag t
    let mark_close_stag t = ansi_of_style @@ closing_style @@ style_of_stag t

    let add_ansi_marking ppf =
      let open Format in
      pp_set_mark_tags ppf true;
      let old_fs = pp_get_formatter_stag_functions ppf () in
      pp_set_formatter_stag_functions ppf
        {old_fs with mark_open_stag; mark_close_stag}

  end

  let pp_list_newline pp_content ppf content =
    PP_helpers.list_sep pp_content (PP_helpers.tag "@,") ppf content
  let pp_te = Ast_typed.PP.type_expression
  let _pp_te_debug ppf (te : ty_expr) : unit =
    Format.fprintf ppf "%a <hash:%d>" pp_te te (hash_type_expression te)

  let rec change ppf (c : Define.change ) : unit =
    let self = change in
    match c with
    | Delete  l              -> Format.fprintf ppf "@{<red>- %a@}" pp_te l
    | Insert  r              -> Format.fprintf ppf "@{<green>+ %a@}" pp_te r 
    | Keep    (l, _r, _eq)   -> Format.fprintf ppf "  %a" pp_te l
    | Change  (l, r, _diff)  -> pp_list_newline self ppf [Delete l; Insert r]

  let t ppf (patch : t) : unit =
    ANSI.add_ansi_marking ppf; (* TODO NP : Add option to disable coloring *)
    match patch with
    | [] -> Format.fprintf ppf ""
    | _ -> Format.fprintf ppf "@.@[<v>Difference between the types:@,%a@]" (pp_list_newline change)  patch
end
