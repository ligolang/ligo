(** Trace tutorial

    The module below guides the reader through the writing of a
    simplified version of the trace monad (`result`), and the
    definition of a few operations that make it easier to work with
    `result`.
*)

module Trace_tutorial = struct
(** The trace monad is fairly similar to the option type: *)

type 'a option =
    Some of 'a  (* Ok also stores a list of annotations *)
  | None;;      (* Errors also stores a list of messages *)

type annotation = string;;
type      error = string;;
type 'a result =
    Ok of 'a * annotation list
  | Errors of error list;;

(** When applying a partial function on a result, it can return a valid result
    (Some v), or indicate failure (None). *)

let divide a b =
  if b = 0
  then None
  else Some (a/b);;

(** With the trace monad, the Errors case also indicates some information about
    the failure, to ease debugging. *)

let divide_trace a b =
  if b = 0
  then (Errors [Printf.sprintf "division by zero: %d / %d" a b])
  else Ok ((a/b) , []);;

(** when composing two functions, the error case is propagated. *)

let divide_three a b c =
  let maybe_a_div_b = divide_trace a b in
  match maybe_a_div_b with
     Ok (a_div_b , _) -> divide_trace a_div_b c
   | (Errors _) as e -> e;;

(** If both calls are successful, the lists of annotations are concatenated. *)

let divide_three_annots a b c =
  let maybe_a_div_b = divide_trace a b in
  match maybe_a_div_b with
     Ok (a_div_b , annots1) ->
       let maybe_a_div_b_div_c = divide_trace a_div_b c in
       begin
         match maybe_a_div_b_div_c with
           Ok (a_div_b_div_c , annots2)
           -> Ok (a_div_b_div_c , annots2 @ annots1)
         | (Errors _) as e2 -> e2
       end
   | (Errors _) as e1 -> e1;;

(** This incurs quite a lot of noise, so we define a `bind` operator which
    takes a function ('x -> ('y result)) and applies it to an existing
    ('x result).

   * If the existing result is Errors, `bind` returns that error without
     calling the function
   * Otherwise `bind` unwraps the Ok and calls the function
     * That function may itself return an error
     * Otherwise `bind` combines the annotations and returns the second
       result. *)

let bind f = function
  | Ok (x, annotations) ->
      (match f x with
         Ok (x', annotations') -> Ok (x', annotations' @ annotations)
       | Errors _ as e' -> ignore annotations; e')
  | Errors _ as e -> e;;

(** The following function divide_three_bind is equivalent to the verbose
   divide_three. *)

let divide_three_bind a b c =
  let maybe_a_div_b = divide_trace a b in
  let continuation a_div_b = divide_trace a_div_b c in
  bind continuation maybe_a_div_b;;

(** This made the code shorter, but the reading order is a bit awkward.
    We define an operator symbol for `bind`: *)

let (>>?) x f = bind f x;;

let divide_three_bind_symbol a b c =
  let maybe_a_div_b = divide_trace a b in
  let continuation a_div_b = divide_trace a_div_b c in
  maybe_a_div_b >>? continuation;;

(** and we inline the two temporary let definitions: *)

let divide_three_bind_symbol' a b c =
  divide_trace a b >>? (fun a_div_b -> divide_trace a_div_b c);;

(** This is now fairly legible, but chaining many such functions is
    not the usual way of writing code. We use ppx_let to add some
    syntactic sugar.

    The ppx is enabled by adding the following lines inside the
    section (library …) or (executable …) of the dune file for
    the project that uses ppx_let.

    (preprocess
      (pps simple-utils.ppx_let_generalized))
*)

module Let_syntax = struct
  let bind m ~f = m >>? f
  module Open_on_rhs_bind = struct end
end;;

(** divide_three_bind_ppx_let is equivalent to divide_three_bind_symbol'.

    Strictly speaking, the only difference is that the module
    Open_on_rhs_bind is opened around the expression on the righ-hand side
    of the `=` sign, namely `divide_trace a b` *)

let divide_three_bind_ppx_let a b c =
  let%bind a_div_b = divide_trace a b in
  divide_trace a_div_b c;;

(** This notation scales fairly well: *)

let divide_many_bind_ppx_let a b c d e f  =
  let      x = a                in
  let%bind x = divide_trace x b in
  let%bind x = divide_trace x c in
  let%bind x = divide_trace x d in
  let%bind x = divide_trace x e in
  let%bind x = divide_trace x f in
  Ok (x , []);;

(** We define a couple of shorthands for common use cases.

    `ok` lifts a ('foo) value to a ('foo result): *)

let ok x = Ok (x, []);;

(** `map` lifts a regular ('foo -> 'bar) function on values
    to a function on results, with type ('foo result -> 'bar result): *)

let map f = function
  | Ok (x, annotations) -> Ok (f x, annotations)
  | Errors _ as e -> e;;

(** `bind_list` turns a (('foo result) list) into a (('foo list) result).

    If the list only contains Ok values, it strips the Ok returns that list
    wrapped with Ok.

    Otherwise, when one or more of the elements of the original list is
    Errors, `bind_list` returns the first error in the list. *)

let rec bind_list = function
  | [] -> ok []
  | hd :: tl -> (
      hd >>? fun hd ->
      bind_list tl >>? fun tl ->
      ok @@ hd :: tl
    );;

(**
   A major feature of Trace is that it enables having a stack of errors (that
   should act as a simplified stack frame), rather than a unique error.
   It is done by using the function `trace`.
   For instance, let's say that you have a function that can trigger two errors,
   and you want to pass their data along with an other error, what you would
   usually do is:
   ```
   let foobarer ... =
     ... in
     let value =
       try ( get key map )
       with
       | Bad_key _ -> raise (Foobar_error ("bad key" , key , map))
       | Missing_value _ -> raise (Foobar_error ("missing index" , key , map))
     in ...
   ```
   With Trace, you would instead:
   ```
   let foobarer ... =
     ... in
     let%bind value =
       trace (simple_error "error getting key") @@
       get key map
     in ...
   ```
   And this will pass along the error triggered by "get key map".
*)

let trace err = function
  | Ok _ as o -> o
  | Errors errs -> Errors (err :: errs);;

(** The real trace monad is very similar to the one that we have
    defined above. The main difference is that the errors and
    annotations are structured data (instead of plain strings) and are
    lazily-generated. *)

let the_end = "End of the tutorial.";;

end (* end Trace_tutorial. *)

module J = Yojson.Basic

module JSON_string_utils = struct
  let member = J.Util.member
  let string = J.Util.to_string_option
  let to_list_option = fun x ->
    try ( Some (J.Util.to_list x))
    with _ -> None
  let to_assoc_option = fun x ->
    try ( Some (J.Util.to_assoc x))
    with _ -> None
  let list = to_list_option
  let assoc = to_assoc_option
  let int = J.Util.to_int_option

  let patch j k v =
    match assoc j with
    | None -> j
    | Some assoc -> `Assoc (
        List.map (fun (k' , v') -> (k' , if k = k' then v else v')) assoc
      )
  
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

type 'a thunk = unit -> 'a

(**
   Errors are encoded in JSON. This is because different libraries will
   implement their own helpers, and we don't want to hardcode in their type how
   they are supposed to interact.
*)
type error = J.t

(**
   Thunks are used because computing some errors can be costly, and we don't
   to spend most of our time building errors. Instead, their computation is
   deferred.
*)
type error_thunk = error thunk

(**
   Annotations should be used in debug mode to aggregate information about some
   value history. Where it was produced, when it was modified, etc.
   It's currently not being used.
*)
type annotation = J.t

(**
   Even in debug mode, building annotations can be quite resource-intensive.
   Instead, a thunk is passed, that is computed only when debug information is
   queried (typically before a print).
*)
type annotation_thunk = annotation thunk

(**
   Types of traced elements. It might be good to rename it `trace` at some
   point.
*)
type 'a result =
  | Ok of 'a * annotation_thunk list
  | Error of error_thunk


(**
   Constructors
*)
let ok x = Ok (x, [])
let fail err = Error err

(**
   Monadic operators
*)
let bind f = function
  | Ok (x, annotations) ->
      (match f x with
         Ok (x', annotations') -> Ok (x', annotations' @ annotations)
       | Error _ as e' -> ignore annotations; e')
  | Error _ as e -> e

let map f = function
  | Ok (x, annotations) -> Ok (f x, annotations)
  | Error _ as e -> e

(**
   Usual bind-syntax is `>>=`, but this is taken from the Tezos code base. Where
   the `result` bind is `>>?`, Lwt's (threading library) is `>>=`, and the
   combination of both is `>>=?`.
*)
let (>>?) x f = bind f x
let (>>|?) x f = map f x

(**
   Used by PPX_let, an OCaml preprocessor.
   What it does is that, when you only care about the case where a result isn't
   an error, instead of writing:
   ```
   (* Stuff that might return an error *) >>? fun ok_value ->
   (* Stuff being done on the result *)
   ```
   You can write:
   ```
   let%bind ok_value = (* Stuff that might return an error *) in
   (* Stuff being done on the result *)
   ```
   This is much more typical of OCaml. makes the code more readable, easy to
   write and refactor. It is used pervasively in LIGO.
*)
module Let_syntax = struct
  let bind m ~f = m >>? f
  module Open_on_rhs_bind = struct end
end


(**
   Build a thunk from a constant.
*)
let thunk x () = x

(**
   Build a standard error, with a title, a message, an error code and some data.
*)
let mk_error
    ?(error_code : int thunk option) ?(message : string thunk option)
    ?(data : (string * string thunk) list option)
    ?(children = []) ?(infos = [])
    ~(title : string thunk) () : error =
  let error_code' = X_option.map (fun x -> ("error_code" , `Int (x ()))) error_code in
  let title' = X_option.some ("title" , `String (title ())) in
  let data' =
    let aux (key , value) = (key , `String (value ())) in
    X_option.map (fun x -> ("data" , `Assoc (List.map aux x))) data in
  let message' = X_option.map (fun x -> ("message" , `String (x ()))) message in
  let type' = Some ("type" , `String "error") in
  let children' = Some ("children" , `List children) in
  let infos' = Some ("infos" , `List infos) in
  `Assoc (X_option.collapse_list [ error_code' ; title' ; message' ; data' ; type' ; children' ; infos' ])

let error ?data ?error_code ?children ?infos title message () = mk_error ?data ?error_code ?children ?infos ~title:(title) ~message:(message) ()

let prepend_child = fun child err ->
  let open JSON_string_utils in
  let children_opt = err |> member "children" |> list in
  let children = match children_opt with
    | Some children -> (child ()) :: children
    | None -> [ child () ] in
  patch err "children" (`List children)

let patch_children = fun children err ->
  let open JSON_string_utils in
  patch err "children" (`List (List.map (fun f -> f ()) children))

(**
   Build a standard info, with a title, a message, an info code and some data.
*)
let mk_info
    ?(info_code : int thunk option) ?(message : string thunk option)
    ?(data : (string * string thunk) list option)
    ~(title : string thunk) () : error =
  let error_code' = X_option.map (fun x -> ("error_code" , `Int (x ()))) info_code in
  let title' = X_option.some ("title" , `String (title ())) in
  let data' =
    let aux (key , value) = (key , `String (value ())) in
    X_option.map (fun x -> ("data" , `Assoc (List.map aux x))) data in
  let message' = X_option.map (fun x -> ("message" , `String (x ()))) message in
  let type' = Some ("type" , `String "info") in
  `Assoc (X_option.collapse_list [ error_code' ; title' ; message' ; data' ; type' ])

let info ?data ?info_code title message () = mk_info ?data ?info_code ~title:(title) ~message:(message) ()

let prepend_info = fun info err ->
  let open JSON_string_utils in
  let infos_opt = err |> member "infos" |> list in
  let infos = match infos_opt with
    | Some infos -> info :: infos
    | None -> [ info ] in
  patch err "infos" (`List infos)


(**
   Helpers that ideally shouldn't be used in production.
*)
let simple_error str () = mk_error ~title:(thunk str) ()
let simple_info str () = mk_info ~title:(thunk str) ()
let simple_fail str = fail @@ simple_error str
let internal_assertion_failure str = simple_error ("assertion failed: " ^ str)

(**
   To be used when you only want to signal an error. It can be useful when
   followed by `trace_strong`.
*)
let dummy_fail = simple_fail "dummy"

let trace info = function
  | Ok _ as o -> o
  | Error err -> Error (fun () -> prepend_info (info ()) (err ()))

(**
   Erase the current error stack, and replace it by the given error. It's useful
   when using `Assert` and you want to discard its auto-generated message.
*)
let trace_strong err = function
  | Ok _ as o -> o
  | Error _ -> Error err

(**
   Sometimes, when you have a list of potentially erroneous elements, you need
   to retrieve all the errors, instead of just the first one. In that case, do:
   ```
   let type_list lst =
     let%bind lst' =
       trace_list (simple_error "Error while typing a list") @@
       List.map type_element lst in
     ...
   ```
   Where before you would have written:
   ```
   let type_list lst =
     let%bind lst' = bind_map_list type_element lst in
     ...
   ```
*)
let trace_list err lst =
  let oks =
    let aux = function
      | Ok (x , _) -> Some x
      | _ -> None in
    X_list.filter_map aux lst in
  let errs =
    let aux = function
      | Error x -> Some x
      | _ -> None in
    X_list.filter_map aux lst in
  match errs with
  | [] -> ok oks
  | errs -> fail (fun () -> patch_children errs err)

(**
   Trace, but with an error which generation may itself fail.
*)
let trace_r err_thunk_may_fail = function
  | Ok _ as o -> o
  | Error _ -> (
      match err_thunk_may_fail () with
      | Ok (err, annotations) -> ignore annotations; Error (err)
      | Error errors_while_generating_error ->
          (* TODO: the complexity could be O(n*n) in the worst case,
             this should use some catenable lists. *)
          Error (errors_while_generating_error)
    )

(**
   `trace_f f error` yields a function that acts the same as `f`, but with an
   error frame that has one more error.
*)
let trace_f f error x =
  trace error @@ f x

(**
   Same, but for functions with 2 parameters.
*)
let trace_f_2 f error x y =
  trace error @@ f x y

(**
   Same, but with a prototypical error.
*)
let trace_f_ez f name =
  trace_f f (error (thunk "in function") name)

let trace_f_2_ez f name =
  trace_f_2 f (error (thunk "in function") name)


(**
   Check if there is no error. Useful for tests.
*)
let to_bool = function
  | Ok _ -> true
  | Error _ -> false

let to_option = function
  | Ok (o, annotations) -> ignore annotations; Some o
  | Error _ -> None

(**
   Convert an option to a result, with a given error if the parameter is None.
*)
let trace_option error = function
  | None -> fail error
  | Some s -> ok s

(**
   Utilities to interact with other data-structure.
   `bind_t` takes an `'a result t` and makes a `'a t result` out of it. It
   "lifts" the error out of the type.
   The most common context is when mapping a given type. For instance, if you
   use a function that can fail in `List.map`, you need to manage a whole list
   of results. Instead, you do `let%bind lst' = bind_list @@ List.map f lst`,
   which will yield an `'a list`.
   `bind_map_t` is roughly syntactic sugar for `bind_t @@ T.map`. So that you
   can rewrite the previous example as `let%bind lst' = bind_map_list f lst`.
   Same thing with folds.
*)

let bind_map_option f = function
  | None -> ok None
  | Some s -> f s >>? fun x -> ok (Some x)

let rec bind_list = function
  | [] -> ok []
  | hd :: tl -> (
      hd >>? fun hd ->
      bind_list tl >>? fun tl ->
      ok @@ hd :: tl
    )  

let bind_ne_list = fun (hd , tl) ->
  hd >>? fun hd ->
  bind_list tl >>? fun tl ->
  ok @@ (hd , tl)

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
let bind_map_ne_list : _ -> 'a X_list.Ne.t -> 'b X_list.Ne.t result = fun f lst -> bind_ne_list (X_list.Ne.map f lst)
let bind_iter_list : (_ -> unit result) -> _ list -> unit result = fun f lst ->
  bind_map_list f lst >>? fun _ -> ok ()

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

let bind_fold_map_list = fun f acc lst ->
  let rec aux (acc , prev) f = function
    | [] -> ok (acc , prev)
    | hd :: tl ->
        f acc hd >>? fun (acc' , hd') ->
        aux (acc' , hd' :: prev) f tl
  in
  aux (acc , []) f lst >>? fun (_acc' , lst') ->
  ok @@ List.rev lst'

let bind_fold_map_right_list = fun f acc lst ->
  let rec aux (acc , prev) f = function
    | [] -> ok (acc , prev)
    | hd :: tl ->
        f acc hd >>? fun (acc' , hd') ->
        aux (acc' , hd' :: prev) f tl
  in
  aux (acc , []) f (List.rev lst) >>? fun (_acc' , lst') ->
  ok lst'

let bind_fold_right_list f init lst =
  let aux x y =
    x >>? fun x ->
    f x y
  in
  X_list.fold_right' aux (ok init) lst

let bind_find_map_list error f lst =
  let rec aux lst =
    match lst with
    | [] -> fail error
    | hd :: tl -> (
        match f hd with
        | Error _ -> aux tl
        | o -> o
      )
  in
  aux lst

let bind_list_iter f lst =
  let aux () y = f y in
  bind_fold_list aux () lst

let bind_or (a, b) =
  match a with
  | Ok _ as o -> o
  | _ -> b

let bind_lr (type a b) ((a : a result), (b:b result)) : [`Left of a | `Right of b] result =
  match (a, b) with
  | (Ok _ as o), _ -> map (fun x -> `Left x) o
  | _, (Ok _ as o) -> map (fun x -> `Right x) o
  | _, Error b -> Error b

let bind_lr_lazy (type a b) ((a : a result), (b:unit -> b result)) : [`Left of a | `Right of b] result =
  match a with
  | Ok _ as o -> map (fun x -> `Left x) o
  | _ -> (
      match b() with
      | Ok _ as o -> map (fun x -> `Right x) o
      | Error b -> Error b
    )

let bind_and (a, b) =
  a >>? fun a ->
  b >>? fun b ->
  ok (a, b)

let bind_pair = bind_and
let bind_map_pair f (a, b) =
  bind_pair (f a, f b)


(**
   Wraps a call that might trigger an exception in a result.
*)
let generic_try err f =
  try (
    ok @@ f ()
  ) with _ -> fail err

(**
   Same, but with a handler that generates an error based on the exception,
   rather than a fixed error.
*)
let specific_try handler f =
  try (
    ok @@ f ()
  ) with exn -> fail (handler exn)

(**
   Same, but tailored to `Sys_error`s, found in `Sys` from `Pervasives`.
*)
let sys_try f =
  let handler = function
    | Sys_error str -> error (thunk "Sys_error") (fun () -> str)
    | exn -> raise exn
  in
  specific_try handler f

(**
   Same, but for a given command.
*)
let sys_command command =
  sys_try (fun () -> Sys.command command) >>? function
  | 0 -> ok ()
  | n -> fail (fun () -> error (thunk "Nonzero return code") (fun () -> (string_of_int n)) ())

(**
   Assertion module.
   Would make sense to move it outside Trace.
*)
module Assert = struct
  let assert_fail ?(msg="didn't fail") = function
    | Ok _ -> simple_fail msg
    | _ -> ok ()

  let assert_true ?(msg="not true") = function
    | true -> ok ()
    | false -> simple_fail msg

  let assert_equal ?msg expected actual =
    assert_true ?msg (expected = actual)

  let assert_equal_int ?msg expected actual =
    let msg =
      let default = Format.asprintf "Not equal int : expected %d, got %d" expected actual in
      X_option.unopt ~default msg in
    assert_equal ~msg expected actual

  let assert_equal_bool ?msg expected actual =
    let msg =
      let default = Format.asprintf "Not equal bool : expected %b, got %b" expected actual in
      X_option.unopt ~default msg in
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

let json_of_error = J.to_string

let error_pp out (e : error) =
  let open JSON_string_utils in
  let message =
    let opt = e |> member "message" |> string in
    X_option.unopt ~default:"" opt in
  let error_code =
    let error_code = e |> member "error_code" in
    match error_code with
    | `Null -> ""
    | _ -> " (" ^ (J.to_string error_code) ^ ")" in
  let title =
    let opt = e |> member "title" |> string in
    X_option.unopt ~default:"" opt in
  let data =
    let data = e |> member "data" in
    match data with
    | `Null -> ""
    | _ -> J.to_string data in
  Format.fprintf out "%s (%s): %s. %s" title error_code message data


let error_pp_short out (e : error) =
  let open JSON_string_utils in
  let title      = e |> member "title"      |> string || "(no title)" in
  let error_code = e |> member "error_code" |> int |> string_of_int || "no error code" in
  Format.fprintf out "%s (%s)" title error_code

let errors_pp =
  Format.pp_print_list
    ~pp_sep:Format.pp_print_newline
    error_pp

let errors_pp_short =
  Format.pp_print_list
    ~pp_sep:Format.pp_print_newline
    error_pp_short

