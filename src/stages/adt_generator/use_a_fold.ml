open Amodule
open Fold

(* TODO: how should we plug these into our test framework? *)

let () =
  let some_root : root = A [{ a1 = X (A [{ a1 = X (B [1;2;3]) ; a2 = W () ; }]) ; a2 = Z (W ()) ; }] in
  let op = {
      no_op with
      a = fun the_a (*_info*) state continue_fold ->
          let (a1' , state') = continue_fold.ta1 the_a.a1 state in
          let (a2' , state'') = continue_fold.ta2 the_a.a2 state' in
          ({
              a1' = a1' ;
              a2' = a2' ;
            }, state'' + 1)
    } in
  let state = 0 in
  let (_, state) = fold_map_root op some_root state in
  if state != 2 then
    failwith (Printf.sprintf "Test failed: expected folder to count 2 nodes, but it counted %d nodes" state)
  else
    ()

let () =
  let some_root : root = A [{ a1 = X (A [{ a1 = X (B [1;2;3]) ; a2 = W () ; }]) ; a2 = Z (W ()) ; }] in
  let op = { no_op with a_pre_state = fun _the_a (*_info*) state -> state + 1 } in
  let state = 0 in
  let (_, state) = fold_map_root op some_root state in
  if state != 2 then
    failwith (Printf.sprintf "Test failed: expected folder to count 2 nodes, but it counted %d nodes" state)
  else
    ()

let () =
  let some_root : root = A [{ a1 = X (A [{ a1 = X (B [1;2;3]) ; a2 = W () ; }]) ; a2 = Z (W ()) ; }] in
  let op = { no_op with a_post_state = fun _the_a _new_a (*_info*) state -> state + 1 } in
  let state = 0 in
  let (_, state) = fold_map_root op some_root state in
  if state != 2 then
    failwith (Printf.sprintf "Test failed: expected folder to count 2 nodes, but it counted %d nodes" state)
  else
    ()


(* Test that the same fold_map_config can be ascibed with different 'a type arguments *)
let _noi : int fold_map_config = no_op (* (fun _ -> ()) *)
let _nob : bool fold_map_config = no_op (* (fun _ -> ()) *)

let () =
  let some_root : root = A [ { a1 = X (A [ { a1 = X (B [ 1 ; 2 ; 3 ]) ; a2 = W () } ]) ; a2 = Z (W ()) } ] in
  let assert_nostate (needs_parens, state) = assert (not needs_parens && String.equal state "") in
  let nostate = false, "" in
  let op = {
      generic = (fun info state ->
        assert_nostate state;
        match info () with
        | (_, Adt_info.Record { name=_; fields }) ->
           false, "{ " ^ String.concat " ; " (List.map (fun (fld : 'x Adt_info.ctor_or_field_continue) -> fld.name ^ " = " ^ snd (fld.continue nostate)) fields) ^ " }"
        | (_, Adt_info.Variant { name=_; constructor={ name; isBuiltin=_; type_=_; continue }; variant=_ }) ->
           (match continue nostate with
            | true,  arg -> true, name ^ " (" ^ arg ^ ")"
            | false, arg -> true, name ^ " "  ^ arg)
        | (_, Adt_info.Poly { name=_; type_=_; arguments=_; continue }) ->
           (continue nostate)
      );
      string = (fun _visitor str state -> assert_nostate state; false , "\"" ^ str ^ "\"") ;
      unit = (fun _visitor () state -> assert_nostate state; false , "()") ;
      int = (fun _visitor i state -> assert_nostate state; false , string_of_int i) ;
      list = (fun _visitor lst continue state ->
        assert_nostate state;
        false , "[ " ^ String.concat " ; " (List.map snd @@ List.map (continue nostate) lst) ^ " ]") ;
      (* generic_ctor_or_field = (fun _info state ->
       *   match _info () with
       *     (_, _, { name=_; isBuiltin=_; type_=_; continue }) -> state ^ "ctor_or_field [" ^ (continue "") ^ "]"
       * ); *)
    } in
  let (_ , state) = fold_root op some_root nostate in
  let expected = "A [ { a1 = X (A [ { a1 = X (B [ 1 ; 2 ; 3 ]) ; a2 = W () } ]) ; a2 = Z (W ()) } ]" in
  if String.equal state expected; then
    ()
  else
    failwith (Printf.sprintf "Test failed: expected\n  %s\n but got\n  %s" expected state)
