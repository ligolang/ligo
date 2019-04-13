module J = Yojson.Basic

type error = [`Assoc of (string * J.t) list]

module JSON_string_utils = struct
  let member = J.Util.member
  let string = J.Util.to_string_option
  let int = J.Util.to_int_option

  let swap f l r = f r l

  let unit x = Some x
  let bind f = function None -> None | Some x -> Some (f x)
  let bind2 f = fun l r -> match l, r with
      None,   None   -> None
    | None,   Some _ -> None
    | Some _, None   -> None
    | Some l, Some r -> Some (f l r)

  let default d = function
      Some x -> x
    | None -> d

  let string_of_int = bind string_of_int

  let (||) l r = l |> default r
  let (|^) = bind2 (^)
end

let mk_error ?(error_code : int option) ~(title : string) ?(message : string option) () =
  let collapse l =
    List.fold_left (fun acc -> function None -> acc | Some e -> e::acc) [] (List.rev l) in
  `Assoc
    (collapse
       [(match error_code with Some c -> Some ("error_code", `Int c) | None -> None);
        Some ("title", `String title);
        (match message with Some m -> Some ("message", `String m) | None -> None)])


type error_thunk = unit -> error

type 'a result =
    Ok of 'a
  | Errors of error_thunk list

let ok x = Ok x
let fail err = Errors [err]

(* When passing a constant string where a thunk is expected, we wrap it with thunk, as follows:
   (thunk "some string")
   We always put the parentheses around the call, to increase grep and sed efficiency.

   When a trace function is called, it is passed a `(fun () -> …)`.
   If the `…` is e.g. error then we write `(fun () -> error title msg ()` *)
let thunk x () = x

let error title message () = mk_error ~title:(title ()) ~message:(message ()) ()

let simple_error str () = mk_error ~title:str ()

let simple_fail str = fail @@ simple_error str

let map f = function
  | Ok x -> f x
  | Errors _ as e -> e

let apply f = function
  | Ok x -> Ok (f x)
  | Errors _ as e -> e

let (>>?) x f = map f x
let (>>|?) = apply

module Let_syntax = struct
  let bind m ~f = m >>? f
end

let trace_strong err = function
  | Ok _ as o -> o
  | Errors _ -> Errors [err]

let trace err = function
  | Ok _ as o -> o
  | Errors errs -> Errors (err :: errs)

let trace_r err_thunk_may_fail = function
  | Ok _ as o -> o
  | Errors errs ->
      match err_thunk_may_fail () with
      | Ok err -> Errors (err :: errs)
      | Errors errors_while_generating_error ->
          (* TODO: the complexity could be O(n*n) in the worst case,
             this should use some catenable lists. *)
          Errors (errors_while_generating_error
                  @ errs)

let trace_f f error x =
  trace error @@ f x

let trace_f_2 f error x y =
  trace error @@ f x y

let trace_f_ez f name =
  trace_f f (error (thunk "in function") name)

let trace_f_2_ez f name =
  trace_f_2 f (error (thunk "in function") name)

let to_option = function
  | Ok o -> Some o
  | Errors _ -> None

let trace_option error = function
  | None -> fail error
  | Some s -> ok s

let rec bind_list = function
  | [] -> ok []
  | hd :: tl -> (
      hd >>? fun hd ->
      bind_list tl >>? fun tl ->
      ok @@ hd :: tl
    )

let bind_smap (s:_ X_map.String.t) =
  let open X_map.String in
  let aux k v prev =
    prev >>? fun prev' ->
    v >>? fun v' ->
    ok @@ add k v' prev' in
  fold aux s (ok empty)

let bind_fold_smap f init (smap : _ X_map.String.t) =
  let aux k v prev =
    prev >>? fun prev' ->
    f prev' k v
  in
  X_map.String.fold aux smap init

let bind_map_smap f smap = bind_smap (X_map.String.map f smap)

let bind_map_list f lst = bind_list (List.map f lst)

let bind_location (x:_ Location.wrap) =
  x.wrap_content >>? fun wrap_content ->
  ok { x with wrap_content }

let bind_map_location f x = bind_location (Location.map f x)

let bind_fold_list f init lst =
  let aux x y =
    x >>? fun x ->
    f x y
  in
  List.fold_left aux (ok init) lst

let bind_list_iter f lst =
  let aux () y = f y in
  bind_fold_list aux () lst

let bind_or (a, b) =
  match a with
  | Ok x -> ok x
  | _ -> b

let bind_lr (type a b) ((a : a result), (b:b result)) : [`Left of a | `Right of b] result =
  match (a, b) with
  | Ok x, _ -> ok @@ `Left x
  | _, Ok x -> ok @@ `Right x
  | _, Errors b -> Errors b

let bind_lr_lazy (type a b) ((a : a result), (b:unit -> b result)) : [`Left of a | `Right of b] result =
  match a with
  | Ok x -> ok @@ `Left x
  | _ -> (
      match b() with
      | Ok x -> ok @@ `Right x
      | Errors b -> Errors b
    )

let bind_and (a, b) =
  a >>? fun a ->
  b >>? fun b ->
  ok (a, b)

let bind_pair = bind_and
let bind_map_pair f (a, b) =
  bind_pair (f a, f b)

module AE = Memory_proto_alpha.Alpha_environment
module TP = Tezos_base__TzPervasives

let of_tz_error (err:X_error_monad.error) : error_thunk =
  let str () = X_error_monad.(to_string err) in
  error (thunk "alpha error") str

let of_alpha_tz_error err = of_tz_error (AE.Ecoproto_error err)

let trace_alpha_tzresult err : 'a AE.Error_monad.tzresult -> 'a result =
  function
  | Result.Ok x -> ok x
  | Error errs -> Errors (err :: List.map of_alpha_tz_error errs)

let trace_alpha_tzresult_lwt error (x:_ AE.Error_monad.tzresult Lwt.t) : _ result =
  trace_alpha_tzresult error @@ Lwt_main.run x

let trace_tzresult err =
  function
  | Result.Ok x -> ok x
  | Error errs -> Errors (err :: List.map of_tz_error errs)

(* TODO: should be a combination of trace_tzresult and trace_r *)
let trace_tzresult_r err_thunk_may_fail =
  function
  | Result.Ok x -> ok x
  | Error errs ->
      let tz_errs = List.map of_tz_error errs in
      match err_thunk_may_fail () with
      | Ok err -> Errors (err :: tz_errs)
      | Errors errors_while_generating_error ->
          (* TODO: the complexity could be O(n*n) in the worst case,
             this should use some catenable lists. *)
          Errors (errors_while_generating_error
                  @ tz_errs)

let trace_tzresult_lwt err (x:_ TP.Error_monad.tzresult Lwt.t) : _ result =
  trace_tzresult err @@ Lwt_main.run x

let trace_tzresult_lwt_r err (x:_ TP.Error_monad.tzresult Lwt.t) : _ result =
  trace_tzresult_r err @@ Lwt_main.run x

let generic_try err f =
  try (
    ok @@ f ()
  ) with _ -> fail err

let specific_try handler f =
  try (
    ok @@ f ()
  ) with exn -> fail ((handler ()) exn)

let sys_try f =
  let handler () = function
    | Sys_error str -> error (thunk "Sys_error") (fun () -> str)
    | exn -> raise exn
  in
  specific_try handler f

let sys_command command =
  sys_try (fun () -> Sys.command command) >>? function
  | 0 -> ok ()
  | n -> fail (fun () -> error (thunk "Nonzero return code") (fun () -> (string_of_int n)) ())

let sequence f lst =
  let rec aux acc = function
    | hd :: tl -> (
        match f hd with
        | Ok x -> aux (x :: acc) tl
        | Errors _ as errs -> errs
      )
    | [] -> ok @@ List.rev acc in
  aux [] lst

let json_of_error = J.to_string
let error_pp out (e : error) =
  let open JSON_string_utils in
  let e : J.t = (match e with `Assoc _ as e -> e) in
  let message    = e |> member "message"    |> string in
  let title      = e |> member "title"      |> string || "(no title)" in
  let error_code = unit " " |^ (e |> member "error_code" |> int |> string_of_int) || "" in
  Format.fprintf out "%s" (error_code ^ ": " ^ title ^ (unit ":" |^ message  || ""))

let error_pp_short out (e : error) =
  let open JSON_string_utils in
  let e : J.t = (match e with `Assoc _ as e -> e) in
  let title      = e |> member "title"      |> string || "(no title)" in
  let error_code = unit " " |^ (e |> member "error_code" |> int |> string_of_int) || "" in
  Format.fprintf out "%s" (error_code ^ ": " ^ title)

let errors_pp =
  Format.pp_print_list
    ~pp_sep:Format.pp_print_newline
    error_pp

let errors_pp_short =
  Format.pp_print_list
    ~pp_sep:Format.pp_print_newline
    error_pp_short

let pp_to_string pp () x =
  Format.fprintf Format.str_formatter "%a" pp x ;
  Format.flush_str_formatter ()

let errors_to_string = pp_to_string errors_pp

module Assert = struct
  let assert_true ?(msg="not true") = function
    | true -> ok ()
    | false -> simple_fail msg

  let assert_equal ?msg expected actual =
    assert_true ?msg (expected = actual)

  let assert_equal_int ?msg expected actual =
    let msg =
      let default = Format.asprintf "Not equal int : expected %d, got %d" expected actual in
      Option.unopt ~default msg in
    assert_equal ~msg expected actual

  let assert_equal_bool ?msg expected actual =
    let msg =
      let default = Format.asprintf "Not equal bool : expected %b, got %b" expected actual in
      Option.unopt ~default msg in
    assert_equal ~msg expected actual

  let assert_none ?(msg="not a none") opt = match opt with
    | None -> ok ()
    | _ -> simple_fail msg

  let assert_list_size ?(msg="lst doesn't have the right size") lst n =
    assert_true ~msg List.(length lst = n)

  let assert_list_empty ?(msg="lst isn't empty") lst =
    assert_true ~msg List.(length lst = 0)

  let assert_list_same_size ?(msg="lists don't have same size") a b =
    assert_true ~msg List.(length a = length b)

  let assert_list_size_2 ~msg = function
    | [a;b] -> ok (a, b)
    | _ -> simple_fail msg

  let assert_list_size_1 ~msg = function
    | [a] -> ok a
    | _ -> simple_fail msg
end
