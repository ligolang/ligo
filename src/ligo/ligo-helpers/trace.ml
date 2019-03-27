type error = {
  message : string ;
  title : string ;
}

type 'a result =
    Ok of 'a
  | Errors of error list

let ok x = Ok x
let fail err = Errors [err]

let simple_error str = {
  message = "" ;
  title = str ;
}

let error title message = { title ; message }

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

let bind_and (a, b) =
  a >>? fun a ->
  b >>? fun b ->
  ok (a, b)

module AE = Tezos_utils.Memory_proto_alpha.Alpha_environment
module TP = Tezos_base__TzPervasives

let of_tz_error (err:Tezos_utils.Error_monad.error) : error =
  let str = Tezos_utils.Error_monad.(to_string err) in
  error "alpha error" str

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

let trace_tzresult_lwt err (x:_ TP.Error_monad.tzresult Lwt.t) : _ result =
  trace_tzresult err @@ Lwt_main.run x

let generic_try err f =
  try (
    ok @@ f ()
  ) with _ -> fail err

let specific_try handler f =
  try (
    ok @@ f ()
  ) with exn -> fail (handler exn)

let sequence f lst =
  let rec aux acc = function
    | hd :: tl -> (
        match f hd with
        | Ok x -> aux (x :: acc) tl
        | Errors _ as errs -> errs
      )
    | [] -> ok @@ List.rev acc in
  aux [] lst

let error_pp fmt error =
  if error.message = ""
  then Format.fprintf fmt "%s" error.title
  else Format.fprintf fmt "%s : %s" error.title error.message

let error_pp_short fmt error =
  Format.fprintf fmt "%s" error.title

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
    let default = Format.asprintf "Not equal int : expected %d, got %d" expected actual in
    let msg = Option.unopt ~default msg in
    assert_equal ~msg expected actual

  let assert_equal_bool ?msg expected actual =
    let default = Format.asprintf "Not equal bool : expected %b, got %b" expected actual in
    let msg = Option.unopt ~default msg in
    assert_equal ~msg expected actual

  let assert_list_size ?(msg="lst doesn't have the right size") lst n =
    assert_true ~msg List.(length lst = n)

  let assert_list_same_size ?(msg="lists don't have same size") a b =
    assert_true ~msg List.(length a = length b)

  let assert_list_size_2 ~msg = function
    | [a;b] -> ok (a, b)
    | _ -> simple_fail msg

  let assert_list_size_1 ~msg = function
    | [a] -> ok a
    | _ -> simple_fail msg
end
