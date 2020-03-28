open Amodule
open Fold

(* TODO: how should we plug these into our test framework? *)

let () =
  let some_root : root = A [{ a1 = X (A [{ a1 = X (B [1;2;3]) ; a2 = W () ; }]) ; a2 = Z (W ()) ; }] in
  let op = {
      no_op with
      a = { no_op.a with
            node__a = fun the_a (*_info*) state continue_fold ->
                      let (a1__' , state') = continue_fold.ta1.node__ta1 the_a.a1 state in
                      let (a2__' , state'') = continue_fold.ta2.node__ta2 the_a.a2 state' in
                      ({ a1__' ; a2__' }, state'' + 1) }
    } in
  let state = 0 in
  let (_, state) = fold_map__root op some_root state in
  if state != 2 then
    failwith (Printf.sprintf "Test failed: expected folder to count 2 nodes, but it counted %d nodes" state)
  else
    ()

let () =
  let some_root : root = A [{ a1 = X (A [{ a1 = X (B [1;2;3]) ; a2 = W () ; }]) ; a2 = Z (W ()) ; }] in
  let op = { no_op with a = { no_op.a with node__a__pre_state = fun _the_a (*_info*) state -> state + 1 } } in
  let state = 0 in
  let (_, state) = fold_map__root op some_root state in
  if state != 2 then
    failwith (Printf.sprintf "Test failed: expected folder to count 2 nodes, but it counted %d nodes" state)
  else
    ()

let () =
  let some_root : root = A [{ a1 = X (A [{ a1 = X (B [1;2;3]) ; a2 = W () ; }]) ; a2 = Z (W ()) ; }] in
  let op = { no_op with a = { no_op.a with node__a__post_state = fun _the_a _new_a (*_info*) state -> state + 1 } } in
  let state = 0 in
  let (_, state) = fold_map__root op some_root state in
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
  let (_ , state) = fold__root op some_root nostate in
  let expected = "A [ { a1 = X (A [ { a1 = X (B [ 1 ; 2 ; 3 ]) ; a2 = W () } ]) ; a2 = Z (W ()) } ]" in
  if String.equal state expected; then
    ()
  else
    failwith (Printf.sprintf "Test failed: expected\n  %s\n but got\n  %s" expected state)
