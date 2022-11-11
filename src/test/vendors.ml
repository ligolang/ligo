open Test_helpers

let uniq lst =
  let module Set = Caml.Set.Make (struct type t = int let compare = compare end) in
  let s = Set.empty in
  let aux (s,lst) e =
    if Set.mem e s then (s,lst)
    else (Set.add e s, e::lst)
  in
  let _,lst = List.fold_left ~f:aux ~init:(s,[]) lst in
  lst
let redblack ~raise:_ () =
  let open RedBlackTrees in
  Random.self_init ();
  let tree = RedBlack.empty in
  let bound = 10000 in
  let rec aux lst e = 
    if e > bound then
      lst
    else 
      let lst = (Random.int bound)::lst in
      aux lst @@ e+1
  in
  let lst = uniq @@ aux [] 1 in
  (*Add test*)
  let tree = List.fold_left ~f:(
    fun tree e ->
      let tree = RedBlack.add ~debug:(fun ppf i -> Format.fprintf ppf "%i" i) ~cmp:(-) RedBlack.New e tree in
      if not @@ RedBlack.is_legal tree then failwith "Unbalanced tree";
      tree
  ) ~init:tree lst in
  (* Removal test *)
  let _ = List.fold_left ~f:(
    fun tree e ->
      let tree = RedBlack.delete ~debug:(fun ppf i -> Format.fprintf ppf "%i" i) ~cmp:(-) e tree in
      if not @@ RedBlack.is_legal tree then failwith "Unbalanced tree";
      tree
  ) ~init:tree lst in
  ()

let main = test_suite "Vendors" [
    test "RedblackTree" redblack ;

    test "ModRes - empty project" Mod_res_tests.empty_project ;
    test "ModRes - no installation" Mod_res_tests.no_installation_json ;
    test "ModRes - no lock file" Mod_res_tests.no_lock_file ;
    test "ModRes - working project" Mod_res_tests.working_project ;
    test "ModRes - complex project" Mod_res_tests.complex_project ;
]
