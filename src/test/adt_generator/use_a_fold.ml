open Amodule
open Fold
open Simple_utils.Trace

module Errors = struct
  let test_fail msg =
    let title () = "test failed" in
    let message () = msg in
    error title message
end

(* TODO: how should we plug these into our test framework? *)
let test (x : unit result) : unit = match x with
| Ok (() , _annotation_thunk) -> ()
| Error err -> failwith (Yojson.Basic.to_string @@ err ())

let () =
  test @@
  let some_root : root = A [{ a1 = X (A [{ a1 = X (B [1;2;3]) ; a2 = W () ; }]) ; a2 = Z (W ()) ; }] in
  let op = {
      no_op with
      a = { no_op.a with
            node__a = fun state the_a (*_info*) continue_fold ->
                      let%bind state, a1__' = continue_fold.ta1.node__ta1 state the_a.a1 in
                      let%bind state, a2__' = continue_fold.ta2.node__ta2 state the_a.a2 in
                      ok (state + 1, { a1__' ; a2__' }) }
    } in
  let state = 0 in
  let%bind (state , _) = fold_map__root op state some_root in
  if state != 2 then
    fail @@ Errors.test_fail (Printf.sprintf "expected folder to count 2 nodes, but it counted %d nodes" state)
  else
    ok ()

let () =
  test @@
  let some_root : root = A [{ a1 = X (A [{ a1 = X (B [1;2;3]) ; a2 = W () ; }]) ; a2 = Z (W ()) ; }] in
  let op = { no_op with a = { no_op.a with node__a__pre_state = fun state _the_a (*_info*) -> ok @@ state + 1 } } in
  let state = 0 in
  let%bind (state , _) = fold_map__root op state some_root in
  if state != 2 then
    fail @@ Errors.test_fail (Printf.sprintf "expected folder to count 2 nodes, but it counted %d nodes" state)
  else
    ok ()

let () =
  test @@
  let some_root : root = A [{ a1 = X (A [{ a1 = X (B [1;2;3]) ; a2 = W () ; }]) ; a2 = Z (W ()) ; }] in
  let op = { no_op with a = { no_op.a with node__a__post_state = fun state _the_a _new_a (*_info*) -> ok @@ state + 1 } } in
  let state = 0 in
  let%bind (state , _) = fold_map__root op state some_root in
  if state != 2 then
    fail @@ Errors.test_fail (Printf.sprintf "expected folder to count 2 nodes, but it counted %d nodes" state)
  else
    ok ()


(* Test that the same fold_map_config can be ascibed with different 'a type arguments *)
let _noi : int fold_map_config = no_op (* (fun _ -> ()) *)
let _nob : bool fold_map_config = no_op (* (fun _ -> ()) *)

let () =
  let some_root : root = A [ { a1 = X (A [ { a1 = X (B [ 1 ; 2 ; 3 ]) ; a2 = W () } ]) ; a2 = Z (W ()) } ] in
  let assert_nostate (needs_parens, state) = assert (not needs_parens && String.equal state "") in
  let nostate = false, "" in
  let op = {
      generic = (fun state info ->
        assert_nostate state;
        match info.node_instance.instance_kind with
        | RecordInstance { fields } ->
           false, "{ " ^ String.concat " ; " (List.map (fun (fld : 'x Adt_info.ctor_or_field_instance) -> fld.cf.name ^ " = " ^ snd (fld.cf_continue nostate)) fields) ^ " }"
        | VariantInstance { constructor={ cf = { name; is_builtin=_; type_=_ }; cf_continue }; variant=_ } ->
           (match cf_continue nostate with
            | true,  arg -> true, name ^ " (" ^ arg ^ ")"
            | false, arg -> true, name ^ " "  ^ arg)
        | PolyInstance { poly=_; arguments=_; poly_continue } ->
           (poly_continue nostate)
      );
      string = (fun _visitor state str -> assert_nostate state; false , "\"" ^ str ^ "\"") ;
      unit = (fun _visitor state () -> assert_nostate state; false , "()") ;
      int = (fun _visitor state i -> assert_nostate state; false , string_of_int i) ;
      list = (fun _visitor continue state lst ->
        assert_nostate state;
        false , "[ " ^ String.concat " ; " (List.map snd @@ List.map (continue nostate) lst) ^ " ]") ;
      (* generic_ctor_or_field = (fun _info state ->
       *   match _info () with
       *     (_, _, { name=_; isBuiltin=_; type_=_; continue }) -> state ^ "ctor_or_field [" ^ (continue "") ^ "]"
       * ); *)
    } in
  let (_ , state) = fold__root op nostate some_root in
  let expected = "A [ { a1 = X (A [ { a1 = X (B [ 1 ; 2 ; 3 ]) ; a2 = W () } ]) ; a2 = Z (W ()) } ]" in
  if String.equal state expected; then
    ()
  else
    failwith (Printf.sprintf "Test failed: expected\n  %s\n but got\n  %s" expected state)
