open Pretty_check

(* should_match_list *)
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
