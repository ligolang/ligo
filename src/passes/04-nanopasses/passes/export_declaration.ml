open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
open Unit_test_helpers
module Location = Simple_utils.Location

(* Upon exported declaration 'export', attribute "public" must be added *)
include Flag.No_arg ()

let compile ~raise:_ =
  (* given a declaration, we extract all visibility attributes (in reverse order) *)
  let rec extract_visibility : _ -> _ =
   fun d ->
    match d with
    | D_attr ((Attribute.{ key = "public" | "private"; value = _ } as attr), d) ->
      let d = get_d d in
      let vattrs, d = extract_visibility d in
      vattrs @ [ attr ], d
    | D_attr (attr, d) ->
      let loc = get_d_loc d in
      let d = get_d d in
      let vattrs, d = extract_visibility d in
      vattrs, D_attr (attr, make_d ~loc @@ d)
    | _ -> [], d
  in
  let declaration : _ declaration_ -> declaration =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | D_export d ->
      (* if marked as export, we turn it into public *)
      let loc = get_d_loc d in
      let _, d = extract_visibility (get_d d) in
      d_attr ~loc (Attribute.{ key = "public"; value = None }, make_d ~loc d)
    | d -> make_d ~loc d
  in
  let program_entry
      : (program_entry, declaration, instruction) program_entry_ -> program_entry
    =
   fun e ->
    match e with
    | PE_declaration d ->
      (match extract_visibility (get_d d) with
      | [], dc ->
        (* if no visibility, we set it to private *)
        let loc = get_d_loc d in
        pe_declaration
          (d_attr ~loc (Attribute.{ key = "private"; value = None }, make_d ~loc dc))
      | vattr :: _, dc ->
        (* otherwise, we just keep the inner most visibility *)
        let loc = get_d_loc d in
        pe_declaration (d_attr ~loc (vattr, make_d ~loc dc)))
    | e -> make_pe e
  in
  Fold { idle_fold with declaration; program_entry }


let decompile ~raise:_ =
  let declaration : _ declaration_ -> declaration =
   fun e ->
    let loc = Location.get_location e in
    match Location.unwrap e with
    | D_attr (Attribute.{ key = "public"; value = None }, d) -> d_export ~loc d
    | d -> make_d ~loc d
  in
  Fold { idle_fold with declaration }


let reduction ~raise =
  { Iter.defaults with
    declaration =
      (function
      | { wrap_content = D_export _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let name = __MODULE__

let%expect_test _ =
  Declaration.(
    {| (D_export (DECLARATION1)) |} |-> compile;
    [%expect {| (D_attr (((key public)) (DECLARATION1))) |}];
    {| (D_attr (((key public)) (DECLARATION1))) |} |-> decompile;
    [%expect {|
      (D_export (DECLARATION1))
    |}])
