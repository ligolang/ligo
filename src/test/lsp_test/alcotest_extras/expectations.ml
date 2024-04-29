open Pretty_check

let rec remove_by (eq : 'a -> 'a -> bool) (y : 'a) (xs : 'a list) : 'a list =
  match xs with
  | [] -> []
  | x :: xs -> if eq x y then xs else x :: remove_by eq y xs


let diff_by (eq : 'a -> 'a -> bool) (xs : 'a list) (ys : 'a list) : 'a list =
  List.fold_left ys ~init:xs ~f:(Fun.flip (remove_by eq))


let match_list ~(actual : 'a list) ~(expected : 'a list) ~(eq : 'a -> 'a -> bool)
    : 'a list * 'a list
  =
  let extra = diff_by eq expected actual in
  let missing = diff_by eq actual expected in
  extra, missing


(* FIXME: In case of failure, the printed format is pretty ugly and gets repeated twice. *)
let should_match_list
    ?(msg : string option)
    (testable_a : 'a Alcotest.testable)
    ~(actual : 'a list)
    ~(expected : 'a list)
    : unit
  =
  match match_list ~actual ~expected ~eq:(Alcotest.equal testable_a) with
  | [], [] -> ()
  | extra, missing ->
    (* For some reason, using [format_list] for a leads to problems with identation,
       so we print each [a] separately *)
    let to_string = Format.asprintf "%a" (Alcotest.pp testable_a) in
    let format_list = Fmt.Dump.list Fmt.string in
    failf
      ("%s"
      ^^ "\n* Expected: %a"
      ^^ "\n\n* Actual:   %a"
      ^^ "\n\n* Extra:    %a"
      ^^ "\n\n* Missing:  %a")
      (Option.value ~default:"Lists do not match." msg)
      format_list
      (List.map ~f:to_string expected)
      format_list
      (List.map ~f:to_string actual)
      format_list
      (List.map ~f:to_string extra)
      format_list
      (List.map ~f:to_string missing)


module Alcotest_map_of_lists (Key : Map_intf.Key) = struct
  let should_match
      ?(msg : string option)
      (testable_key : Key.t Alcotest.testable)
      (testable_a : 'a Alcotest.testable)
      ~(actual : (_, 'a list, _) Map.t)
      ~(expected : (_, 'a list, _) Map.t)
      : unit
    =
    let msg = Option.value_map ~default:"" ~f:(Fun.flip ( ^ ) "\n") msg in
    let () =
      should_match_list
        ~msg:(msg ^ "Keys do not match.")
        testable_key
        ~actual:(Map.keys actual)
        ~expected:(Map.keys expected)
    in
    Map.iteri actual ~f:(fun ~key ~data ->
        match Map.find expected key with
        | None -> failf "impossible"
        | Some expected_data ->
          should_match_list
            ~msg:(msg ^ "Values do not match.")
            testable_a
            ~actual:data
            ~expected:expected_data)
end

let should_be_contained_in
    ?(msg : string option)
    (testable_a : 'a Alcotest.testable)
    ~(small : 'a list)
    ~(big : 'a list)
    : unit
  =
  match diff_by (Alcotest.equal testable_a) small big with
  | [] -> ()
  | missing ->
    let to_string = Format.asprintf "%a" (Alcotest.pp testable_a) in
    let format_list = Fmt.Dump.list Fmt.string in
    failf
      ("%s"
      ^^ "\n* Small list contains: %a"
      ^^ "\n\n* Big list contains: %a"
      ^^ "\n\n* Missing: %a")
      (Option.value ~default:"The first list is not contained in the second list." msg)
      format_list
      (List.map ~f:to_string small)
      format_list
      (List.map ~f:to_string big)
      format_list
      (List.map ~f:to_string missing)


let should_not_be_contained_in
    ?(msg : string option)
    (testable_a : 'a Alcotest.testable)
    ~small
    ~big
    : unit
  =
  match List.filter small ~f:(List.mem big ~equal:(Alcotest.equal testable_a)) with
  | [] -> ()
  | extra ->
    let to_string = Format.asprintf "%a" (Alcotest.pp testable_a) in
    let format_list = Fmt.Dump.list Fmt.string in
    failf
      ("%s"
      ^^ "\n* Small list contains: %a"
      ^^ "\n\n* Big list contains: %a"
      ^^ "\n\n* Extra: %a")
      (Option.value ~default:"The first list contains items in the second list." msg)
      format_list
      (List.map ~f:to_string small)
      format_list
      (List.map ~f:to_string big)
      format_list
      (List.map ~f:to_string extra)
