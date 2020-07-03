open Amodule
open Fold
open Simple_utils.Trace

module O = Fold.O

let (|>) v f = f v

(* TODO: how should we plug these into our test framework? *)
let test (x : (unit,_) result) : unit = match x with
| Ok (() , _annotation_thunk) -> ()
(* | Error err -> failwith (Yojson.to_string @@ err ()) *)
| Error _err -> failwith ("TODO")

let () =
  test @@
  let some_root : root = A [{ a1 = X (A [{ a1 = X (B [1;2;3]) ; a2 = W () ; }]) ; a2 = Z (W ()) ; }] in
  let op =
    no_op |>
      with__a (fun state the_a (*_info*) continue_fold ->
          let%bind state, a1 = continue_fold.ta1.node__ta1 state the_a.a1 in
          let%bind state, a2 = continue_fold.ta2.node__ta2 state the_a.a2 in
          ok (state + 1, (O.make__a ~a1 ~a2 : O.a)))
  in
  let state = 0 in
  let%bind (state , _) = fold_map__root op state some_root in
  if state != 2 then
    (* expected folder to count 2 nodes, but it counted 'state' nodes *)
    fail @@ Main_errors.test_internal __LOC__
  else
    ok ()

let () =
  test @@
  let some_root : root = A [{ a1 = X (A [{ a1 = X (B [1;2;3]) ; a2 = W () ; }]) ; a2 = Z (W ()) ; }] in
  let op = no_op |>
      with__a__pre_state (fun state _the_a (*_info*) -> ok @@ state + 1) in
  let state = 0 in
  let%bind (state , _) = fold_map__root op state some_root in
  if state != 2 then
    (* expected folder to count 2 nodes, but it counted 'state' nodes *)
    fail @@ Main_errors.test_internal __LOC__
  else
    ok ()

let () =
  test @@
  let some_root : root = A [{ a1 = X (A [{ a1 = X (B [1;2;3]) ; a2 = W () ; }]) ; a2 = Z (W ()) ; }] in
  let op = no_op |> with__a__post_state (fun state _the_a _new_a (*_info*) -> ok @@ state + 1) in
  let state = 0 in
  let%bind (state , _) = fold_map__root op state some_root in
  if state != 2 then
    (* expected folder to count 2 nodes, but it counted 'state' nodes *)
    fail @@ Main_errors.test_internal __LOC__
  else
    ok ()


(* Test that the same fold_map_config can be ascibed with different 'a type arguments *)
let _noi : (int, _) fold_map_config__Amodule = no_op (* (fun _ -> ()) *)
let _nob : (bool, _) fold_map_config__Amodule = no_op (* (fun _ -> ()) *)

type no_state = NoState
let to_string some_root =
  let op : ('i, 'o) Generated_fold.fold_config = {
    generic = (fun NoState info ->
        match info.node_instance.instance_kind with
        | RecordInstance { field_instances } ->
          false, "{ " ^ String.concat " ; " (List.map (fun (fld : ('xi , 'xo) Adt_info.ctor_or_field_instance) -> fld.cf.name ^ " = " ^ snd (fld.cf_continue NoState)) field_instances) ^ " }"
        | VariantInstance { constructor={ cf = { name; is_builtin=_; type_=_; }; cf_continue; cf_new_fold=_ }; variant=_ } ->
          (match cf_continue NoState with
           | true,  arg -> true, name ^ " (" ^ arg ^ ")"
           | false, arg -> true, name ^ " "  ^ arg)
        | PolyInstance { poly=_; arguments=_; poly_continue } ->
          (poly_continue NoState)
      ) ;
    generic_empty_ctor = (fun NoState -> false, "") ;
    string = (fun _visitor NoState str -> false , "\"" ^ str ^ "\"") ;
    unit = (fun _visitor NoState () -> false , "()") ;
    int = (fun _visitor NoState i -> false , string_of_int i) ;
    list = (fun _visitor continue NoState lst ->
        false , "[ " ^ String.concat " ; " (List.map snd @@ List.map (continue NoState) lst) ^ " ]") ;
    (* generic_ctor_or_field = (fun _info state ->
     *   match _info () with
     *     (_, _, { name=_; isBuiltin=_; type_=_; continue }) -> state ^ "ctor_or_field [" ^ (continue "") ^ "]"
     * ); *)
  } in
  let (_ , state) = Generated_fold.fold__root op NoState some_root in
  state

let () =
  let some_root : root = A [ { a1 = X (A [ { a1 = X (B [ 1 ; 2 ; 3 ]) ; a2 = W () } ]) ; a2 = Z (W ()) } ] in
  let expected = "A [ { a1 = X (A [ { a1 = X (B [ 1 ; 2 ; 3 ]) ; a2 = W () } ]) ; a2 = Z (W ()) } ]" in
  let state = to_string some_root in
  if String.equal state expected; then
    ()
  else
    failwith (Printf.sprintf "Test failed: expected\n  %s\n but got\n  %s" expected state)

(* Test generic creation of nodes *)
let () =
  let i = whole_adt_info () in
  let dynamic =
    match RedBlackTrees.PolyMap.find_opt "rootB" i with
    | Some { kind = PolyType { poly_name = _; make_poly }; declaration_name = _ } ->
      (match make_poly with
         Make_List mk ->
         match mk [ Whatever_Int 42 ; Whatever_Int 43 ] with
           Some l ->
           (match RedBlackTrees.PolyMap.find_opt "root" i with
              Some { kind = VariantType { constructors }; declaration_name = _ } ->
              (* TODO: use a PolyMap.t *)
              let { ctor = _ ; make_ctor } = List.find (fun { ctor = { name; is_builtin = _; type_ = _ }; make_ctor = _ } -> String.equal name "B") constructors in
              let _ =
                (match l with
                 | Whatever_RootB _ -> () | _ -> failwith "whoops")
              in
              (match make_ctor l with (* Wrap the int list with the B constructor *)
                 Some b -> b
               | None -> failwith "Couldn't create instance of the B constructor, did you supply the right argument type?")
            | Some { kind = _ ; _ } -> failwith "unexpected node info for root: wrong kind !!!"
            | None -> failwith "can't find node info for root !!!")
         | None -> failwith "Couldn't create list, did you supply the wrong element type?"
         (* | _ -> failwith "unexpected maker function for rootB: expected rootB to be a list !!!" *)
      )
    | Some { kind = _ ; _ } -> failwith "unexpected node info for rootB: wrong kind !!!"
    | None -> failwith "can't find node info for rootB !!!"
  in
  (match dynamic with
     Whatever_Root root ->
     (match root with
        B [ 42 ; 43 ] -> () (* Victory, we created the expected value *)
      | _ -> failwith ("Incorrect value " ^ to_string root))
   | _ -> failwith "Incorrect result type: expected a dynamically-typed root, but got something else")

