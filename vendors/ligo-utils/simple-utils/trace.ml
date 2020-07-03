(* Trace tutorial

    This module guides the reader through the writing of a simplified
    version of the trace monad [result], and the definition of a few
    operations that make it easier to work with [result].
*)

module Trace_tutorial = struct
  (* The trace monad is fairly similar to the predefined [option]
     type. It is an instance of the predefined [result] type. *)

  type annotation = string
  type      error = string

  (* The type ['a result] is used by the trace monad to both model an
     expected value of type ['a] or the failure to obtain it, instead
     of working directly with ['a] values and handling separately
     errors, for example by means of exceptions. (See the type [('a,'b)
     result] in the module [Pervasives] of the OCaml system for a
     comparable approach to error handling.)

     The type ['a result] carries either a value of type ['a], with a
     list of annotations (information about past successful
     computations), or it is a list of errors accumulated so far.
     The former case is denoted by the data constructor [Ok], and the
     second by [Error]. *)

  type nonrec 'a result = ('a * annotation list, error list) result
  (*
  = Ok of 'a * annotation list
  | Error of error list
  *)

  (* The function [divide_trace] shows the basic use of the trace
      monad. *)

  let divide_trace a b =
    if   b = 0
    then Error [Printf.sprintf "division by zero: %d/%d" a b]
    else Ok (a/b, [])

  (* The function [divide_three] shows that when composing two
     functions, if the first call fails, the error is passed along
     and the second call is not evaluated. (A pattern called
     "error-passing style"). *)

  let divide_three a b c =
    match divide_trace a b with
      Ok (a_div_b , _) -> divide_trace a_div_b c
    |          errors  -> errors

  (* The function [divide_three_annot] shows that when composing two
     functions, if both calls are successful, the lists of
     annotations are joined. *)

  let divide_three_annot a b c =
    match divide_trace a b with
      Ok (a_div_b, annot1) -> (
        match divide_trace a_div_b c with
           Ok (a_div_b_div_c, annot2) ->
             Ok (a_div_b_div_c, annot2 @ annot1)
        | errors -> errors)
    | errors -> errors

  (* The systematic matching of the result of each call in a function
      composition is bulky, so we define a [bind] function which takes
      a function [f: 'a -> 'b result] and applies it to a current ['a
      result] (not ['a]).

        * If the current result is an error, then [bind]
        returns that same error without calling [f];

        * otherwise [bind] unwraps the [Ok] of the current result
          and calls [f] on it:
            * That call itself may return an error;}
            * if not, [bind] combines the annotations and returns the
              last result. *)
  let bind (f: 'a -> 'b result) : 'a result -> 'b result =
    function
      Ok (x, annot) -> (
        match f x with
          Ok (x', annot') -> Ok (x', annot' @ annot)
        |          errors -> ignore annot; errors)
    | Error _ as e -> e

  (* The function [divide_three_bind] is equivalent to the verbose
     [divide_three] above, but makes use of [bind]. *)

  let divide_three_bind a b c =
    let maybe_a_div_b = divide_trace a b in
    let continuation a_div_b = divide_trace a_div_b c
    in bind continuation maybe_a_div_b

  (* The operator [(>>?)] is a redefinition of [bind] that makes the
     program shorter, at the cost of a slightly
     awkward reading because the two parameters are swapped. *)

  let (>>?) x f = bind f x

  (* The function [divide_three_bind_symbol] is equivalent to
     [divide_three_bind], but makes use of the operator [(>>?)]. *)

  let divide_three_bind_symbol a b c =
    let maybe_a_div_b = divide_trace a b in
    let continuation a_div_b = divide_trace a_div_b c in
    maybe_a_div_b >>? continuation

  (* The function [divide_three_bind_symbol'] is equivalent to
     [divide_three_bind_symbol], where the two temporary [let]
     definitions are inlined for a more compact reading. *)

  let divide_three_bind_symbol' a b c =
    divide_trace a b >>? (fun a_div_b -> divide_trace a_div_b c)

  (* This is now fairly legible, but chaining many such functions is
     not the usual way of writing code. We use the PPX extension to
     the OCaml compiler [ppx_let] to add some syntactic sugar.
     The extension framework PPX is enabled by adding the following
     lines inside the section [(library ...)] or [(executable ...)]
     of the [dune] file for the project that uses [ppx_let], like so:
     [(preprocess
        (pps simple-utils.ppx_let_generalized))]
     The extension [ppx_let] requires the module [Let_syntax] to be
     defined. *)

  module Let_syntax = struct
    let bind m ~f = m >>? f
    module Open_on_rhs_bind = struct end
  end

  (* The function [divide_three_bind_ppx_let] is equivalent to the
     function [divide_three_bind_symbol']. The only difference is
     that the module [Open_on_rhs_bind] is implicitly opened around
     the expression on the righ-hand side of the [=] sign, namely
     [divide_trace a b]. *)

  let divide_three_bind_ppx_let a b c =
    let%bind a_div_b = divide_trace a b
    in divide_trace a_div_b c

  (** The function [divide_many_bind_ppx_let] shows how well this
      notation composes. *)

  let divide_many_bind_ppx_let a b c d e f  =
    let      x = a                in
    let%bind x = divide_trace x b in
    let%bind x = divide_trace x c in
    let%bind x = divide_trace x d in
    let%bind x = divide_trace x e in
    let%bind x = divide_trace x f
    in Ok (x, [])

  (** The function [ok] is a shorthand for an [Ok] without
      annotations. *)

  let ok x = Ok (x, [])

  (* The function [map] lifts a regular ['a -> 'b] function on values to
      a function on results, of type ['a result -> 'b result].  *)

  let map f = function
    Ok (x, annotations) -> Ok (f x, annotations)
  | e -> e

  (*  The function [bind_list] turns a list of results of type [('a
      result) list] into a result of list, of type [('a list) result],
      as follows.
        * If the list only contains [Ok] values, it strips the [Ok]
          of each element and returns that list wrapped with [Ok].}

        * Otherwise, one or more of the elements of the input list
          is [Error], then [bind_list] returns the first error in the
          list.
   *)

  let rec bind_list = function
    [] -> ok []
  | hd::tl ->
     hd >>? fun hd ->
     bind_list tl >>? fun tl ->
     ok @@ hd::tl
  (*  A major feature of [Trace] is that it enables having a stack of
      errors (that should act as a simplified stack frame), rather
      than a unique error. It is done by using the function
      [trace]. For instance, let's say that you have a function that
      can trigger two errors, and you want to pass their data along
      with an other error, what you would usually do is:
      [let foobarer ... = ... in
        let value =
          try get key map with
            Bad_key _ -> raise (Error ("bad key", key, map))
          | Missing_value _ -> raise (Error ("missing index", key, map))
        in ...]
      With [Trace], you would instead:
      [let foobarer ... = ... in
        let%bind value =
          trace (simple_error "error getting key") @@
          get key map
        in ...]
        And this will pass along the error triggered by [get key map]. *)

  let trace err = function
    Error e -> Error (err::e)
  | ok -> ok

  (* The real trace monad is very similar to the one that we have
      defined above. The main difference is that the errors and
      annotations are structured data (instead of plain strings) and
      are generated lazily. *)

  let the_end = "End of the tutorial."

end (* end Trace_tutorial. *)

(* Annotations should be used in debug mode to aggregate information
   about some value history. Where it was produced, when it was
   modified, etc.  It is currently not being used. *)

type 'a thunk = unit -> 'a
type annotation = Yojson.t

(* Even in debug mode, building annotations can be quite
   resource-intensive.  Instead, a thunk is passed, that is computed
   only when debug information is queried (typically before a print).*)

type annotation_thunk = annotation thunk

type nonrec ('value, 'error) result = ('value * annotation_thunk list, 'error) result

(** {1 Constructors} *)

let ok x = Ok (x, [])

let fail err = Error err

(* Monadic operators *)

let bind f = function
  Error _ as e -> e
| Ok (x, ann) ->
    match f x with
      Ok (x', ann') -> Ok (x', ann' @ ann)
    | Error _ as e' -> ignore ann; e'

let map f = function
  Ok (x, annotations) -> Ok (f x, annotations)
| Error _ as e -> e

(* The lexical convention usually adopted for the bind function is
   [>>=], but ours comes from the Tezos code base, where the [result]
   bind is [>>?], and [Lwt]'s (threading library) is [>>=], and the
   combination of both is [>>=?]. *)

let (>>?)  x f = bind f x
let (>>|?) x f = map f x

(* Used by PPX_let, an OCaml preprocessor.
   What it does is that, when you only care about the case where a result isn't
   an error, instead of writing:
   [
   (* Stuff that might return an error *) >>? fun ok_value ->
   (* Stuff being done on the result *)
   ]
   You can write:
   [
   let%bind ok_value = (* Stuff that might return an error *) in
   (* Stuff being done on the result *)
   ]
   This is much more typical of OCaml. This makes the code more
   readable, easy to write and refactor. It is used pervasively in
   LIGO. *)

module Let_syntax = struct
  let bind m ~f = m >>? f
  module Open_on_rhs_bind = struct end
end

(* Build a thunk from a constant. *)

let thunk x () = x

(* To be used when you only want to signal an error. It can be useful
   when followed by [trace_strong]. *)

(* let trace info = function
  Ok _ as o -> o
| Error err -> Error (fun () -> prepend_info (info ()) (err ())) *)
let trace tracer v = match v with
  | Ok v' -> Ok v'
  | Error err -> Error (tracer err)

(* Erase the current error stack, and replace it by the given
   error. It's useful when using [Assert] and you want to discard its
   autogenerated message. *)

let trace_strong err = function
  Ok _ as o -> o
| Error _ -> Error err

(* Trace, but with an error which generation may itself fail. *)

let trace_r err_thunk_may_fail = function
  Ok _ as o -> o
| Error _ ->
    match err_thunk_may_fail () with
      Ok (err, annotations) -> ignore annotations; Error (err)
    | Error errors_while_generating_error ->
       (* TODO: the complexity could be O(n*n) in the worst case,
           this should use some catenable lists. *)
       Error (errors_while_generating_error)

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
  Convert a result to a json, if res in an error, the produces JSON will be
  empty, otherwise the provided to_json function will be used
*)
let to_json to_json = function
  | Ok (v,_) ->  to_json v
  | Error _ -> `Null 

(**
   Convert an option to a result, with a given error if the parameter is None.
*)
let trace_option error = function
    None -> fail error
    | Some s -> ok s

let trace_assert_fail_option error = function
    None -> ok ()
    | Some _s -> fail error


(** Utilities to interact with other data-structure.  [bind_t] takes
   an ['a result t] and makes a ['a t result] out of it. It "lifts" the
   error out of the type.  The most common context is when mapping a
   given type. For instance, if you use a function that can fail in
   [List.map], you need to manage a whole list of results. Instead,
   you do [let%bind lst' = bind_list @@ List.map f lst], which will
   yield an ['a list].  [bind_map_t] is roughly syntactic sugar for
   [bind_t @@ T.map]. So that you can rewrite the previous example as
   [let%bind lst' = bind_map_list f lst].  Same thing with folds.
 *)

let bind_compose f g x = 
  let%bind y = g x in
  f y

let bind_map_option f = function
    None -> ok None
| Some s -> f s >>? fun x -> ok (Some x)

let rec bind_list = function
      [] -> ok []
| hd::tl -> hd >>? fun hd -> bind_list tl >>? fun tl -> ok @@ hd :: tl
let bind_ne_list (hd, tl) =
  hd >>? fun hd -> bind_list tl >>? fun tl -> ok @@ (hd, tl)
let bind_smap (s:_ X_map.String.t) =
  let open X_map.String in
  let aux k v prev =
    prev >>? fun prev' ->
    v >>? fun v' ->
    ok @@ add k v' prev'
  in fold aux s (ok empty)

let bind_fold_smap f init (smap : _ X_map.String.t) =
  let aux k v prev =
    prev >>? fun prev' -> f prev' k v
  in X_map.String.fold aux smap init

let bind_map_smap f smap = bind_smap (X_map.String.map f smap)

let bind_concat l1 l2 =
  let%bind l1' = l1 in
  let%bind l2' = l2 in
  ok @@ (l1' @ l2')

let bind_map_list f lst = bind_list (List.map f lst)
let bind_mapi_list f lst = bind_list (List.mapi f lst)

let rec bind_map_list_seq f lst = match lst with
  | [] -> ok []
  | hd :: tl ->
      let%bind hd' = f hd in
      let%bind tl' = bind_map_list_seq f tl in
      ok (hd' :: tl')

let bind_map_ne_list : _ -> 'a X_list.Ne.t -> ('b X_list.Ne.t,_) result =
  fun f lst -> bind_ne_list (X_list.Ne.map f lst)

let bind_iter_list : (_ -> (unit,_) result) -> _ list -> (unit,_) result =
  fun f lst -> bind_map_list f lst >>? fun _ -> ok ()

let bind_location (x:_ Location.wrap) =
  x.wrap_content >>? fun wrap_content ->
  ok { x with wrap_content }

let bind_map_location f x = bind_location (Location.map f x)

let bind_fold_list f init lst =
  let aux x y = x >>? fun x -> f x y
  in List.fold_left aux (ok init) lst

module TMap(X : Map.OrderedType) = struct
  module MX = Map.Make(X)
  let bind_fold_Map f init map =
    let aux k v x  =
      x >>? fun x ->
      f ~x ~k ~v
    in MX.fold aux map (ok init)

  let bind_map_Map f map =
    let aux k v map'  =
      map' >>? fun map' ->
      f ~k ~v >>? fun v' ->
      ok @@ MX.update k (function
          | None -> Some v'
          | Some _ ->
             failwith "Key collision: Should not happen in bind_map_Map")
        map'
    in MX.fold aux map (ok MX.empty)
end

let bind_fold_pair f init (a,b) =
  let aux x y = x >>? fun x -> f x y
  in List.fold_left aux (ok init) [a;b]

let bind_fold_triple f init (a,b,c) =
  let aux x y = x >>? fun x -> f x y
  in List.fold_left aux (ok init) [a;b;c]

let bind_fold_map_list f acc lst =
  let rec aux (acc, prev) f = function
    | [] -> ok (acc, prev)
    | hd :: tl ->
        f acc hd >>? fun (acc' , hd') ->
        aux (acc', hd'::prev) f tl in
  aux (acc , []) f lst >>? fun (acc' , lst') ->
  ok @@ (acc' , List.rev lst')

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
  let aux x y = x >>? fun x -> f x y
  in X_list.fold_right' aux (ok init) lst

let bind_find_map_list error f lst =
  let rec aux lst =
    match lst with
    | [] -> fail error
    | hd :: tl ->
        match f hd with
        | Error _ -> aux tl
        | o -> o
  in aux lst

let bind_list_iter f lst =
  let aux () y = f y in
  bind_fold_list aux () lst

let bind_or (a, b) =
  match a with
  | Ok _ as o -> o
  | _ -> b

let bind_map_or (fa, fb) c = bind_or (fa c, fb c)

let bind_and (a, b) =
  a >>? fun a ->
  b >>? fun b ->
  ok (a, b)

let bind_and3 (a, b, c) =
  a >>? fun a ->
  b >>? fun b ->
  c >>? fun c ->
  ok (a, b, c)

let bind_pair = bind_and

let bind_map_pair f (a, b) =
  bind_pair (f a, f b)

let bind_fold_map_pair f acc (a, b) =
  f acc a >>? fun (acc', a') ->
  f acc' b >>? fun (acc'', b') ->
  ok (acc'', (a', b'))

let bind_map_triple f (a, b, c) = bind_and3 (f a, f b, f c)

let bind_list_cons v lst = lst >>? fun lst -> ok (v::lst)

let rec bind_chain : ('a -> ('a,_) result) list -> 'a -> ('a,_) result = fun fs x ->
  match fs with
  | [] -> ok x
  | hd :: tl -> (
      let aux : 'a -> ('a,_) result = fun x -> bind (bind_chain tl) (hd x) in
      bind aux (ok x)
    )

let rec bind_chain_ignore_acc : ('a -> ('b * 'a, _) result) list -> 'a -> ('a,_) result = fun fs x ->
  match fs with
  | [] -> ok x
  | hd :: tl -> (
      let aux : 'a -> ('a,_) result = fun x ->
        hd x >>? fun (_,aa) ->
        bind (bind_chain_ignore_acc tl) (ok aa) in
      bind aux (ok x)
    )

(**
   Wraps a call that might trigger an exception in a result.
*)
let generic_try err f = try ok @@ f () with _ -> fail err

(**
   Same, but with a handler that generates an error based on the exception,
   rather than a fixed error.
*)
let specific_try handler f =
  try ok @@ f () with exn -> fail (handler exn)

(**
   Assertion module.
   (* Would make sense to move it outside Trace. *)
*)
module Assert = struct
  let assert_fail err = function
    Ok _ -> fail err
  |    _ -> ok ()

  let assert_true err = function
    | true -> ok ()
    | false -> fail err

  let assert_list_size err lst n =
    assert_true err List.(length lst = n)

  let assert_list_empty err lst =
    assert_true err List.(length lst = 0)
  
  let assert_list_same_size err lsta lstb =
    assert_true err List.(length lsta = length lstb)

end
