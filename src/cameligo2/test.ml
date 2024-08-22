open Parsetree

let loc = Location.none

module OCaml = struct
  open Ocaml_common

  let env = Env.initial_safe_string
  let _type_expr env expr = Typecore.type_expression env expr

  let type_str env str =
    let str, sig_, names, shape, env = Typemod.type_structure env str in
    str
end

module Ligo = struct
  open Cameligo2
  open Var_solving

  let env = Env.initial

  let compile_str env str =
    let str = From_ocaml.compile_str str in
    let env, str = Var_solving.solve_module env str in
    str
end

(* [%str let sequence x y = x] *)
(*  [%str
      type nonrec packed_id = { f : 'a. 'a -> 'a }

      let id x = x
      let packed_id = { f = id }] *)
(*     [%str
      module M = struct
        let id x = x
      end]
      *)
(* [%str type nonrec user = { id : 'a. 'a -> 'a }]  *)
(* type nonrec user = { id : 'a. 'a -> 'a }

  type nonrec example =
    | A of unit
    | B of user
    | C of user * user
    | D of { id : 'a. 'a -> 'a } *)
(* [%str
      type nonrec user =
        { id : int
        ; name : string
        }

      let michael = { id = 0; name = "Michael" } *)
(* type nonrec status =
        | Default
        | Blocked of string
        | Unblocked of string * string

      let x = Default
      let y = Blocked "bad user"
      let z = Unblocked ("bad user", "but nice")
*)
(* 
   type nonrec user =
        | Not_user
        | User of
            { id : int
            ; name : string
            }

      let a = Not_user
      let b = User { id = 1; name = "Billie Jean" }
*)
(*
  type nonrec user =
    | A
    | B of int
    | C of int * int
    | User of { id : int; name : string }

  let f x =
    match x with
    | A -> 0
    | B x -> x
    | C (x, y) -> y
    | User { id; name } -> id
*)
(* let f x : int = x *)
(* let f (x : int) = x *)
(* let f : int -> int = fun (x : int) : int -> x *)
(* let id x = x
   let f : int -> int = id *)
(* let not b =
      match b with
      | true -> false
      | false -> true *)
(* 
  (* TODO: ignore unused *)
  let[@ligo.predef "int"] (_ : int -> _) = fun x -> x
  let[@ligo.predef "string"] (_ : string -> _) = fun x -> x
  let[@ligo.predef "list"] (_ : _ list -> _) = fun x -> x

  (* let[@ligo.predef "option"] (_ : _ option -> _) = fun x -> x *)
  let x = 1
*)
(*
type nonrec ('k, 'v) t = A of 'k

let x : (int, int) t = A 1
*)
(*
let id : 'a. 'a -> 'a = fun x -> x
let sequence : 'a 'b. 'a -> 'b -> 'a = fun x y -> x

type nonrec 'a t = A of 'a

let a : 'a. 'a -> 'a t = fun x -> A x
*)
(*
type nonrec 'a t =
  | A of
      { id : 'a
      ; name : int
      }
  | B of { x : int }

let f id name = A { id; name }
let b = B { x = 1 }
*)
(*
type nonrec nat [@@ligo.internal.constant "nat"]
type nonrec tez [@@ligo.internal.constant "tez"]
type nonrec address [@@ligo.internal.constant "address"]

external nat : int -> nat = "%ligo.nat"
external tez : int -> tez = "%ligo.tez"
external address : string -> address = "%ligo.address"

let x = nat 1
let y = tez 1
let z = address "a"
  *)
(* TODO: by the end ensure the OCaml produced stdlib
  is equivalent to the Ligo one in names and types *)
let _stdlib =
  [%str
    (* OCaml predef *)
    (* TODO: this is weird syntax *)
    let[@ligo.internal.predef "string"] (_ : string -> _) = fun x -> x

    (* type bytes = "%constant:bytes" *)
    let[@ligo.internal.predef "int"] (_ : int -> _) = fun x -> x

    type nonrec nat [@@ligo.internal.constant "nat"]

    let[@ligo.internal.predef "unit"] (_ : unit -> _) = fun x -> x

    (* let[@ligo.internal.predef "option"] (_ : int -> _) = fun x -> x *)

    (* TODO: bytes is a weird predef *)
    (* Ligo constant *)

    type nonrec operation [@@ligo.internal.constant "operation"]
    type nonrec tez [@@ligo.internal.constant "tez"]
    type nonrec address [@@ligo.internal.constant "address"]
    type nonrec signature [@@ligo.internal.constant "signature"]
    type nonrec key [@@ligo.internal.constant "key"]
    type nonrec key_hash [@@ligo.internal.constant "key_hash"]
    type nonrec timestamp [@@ligo.internal.constant "timestamp"]

    let[@ligo.internal.predef "list"] (_ : _ list -> _) = fun x -> x

    type nonrec ('k, 'v) big_map [@@ligo.internal.constant "big_map"]
    type nonrec ('k, 'v) map [@@ligo.internal.constant "map"]
    type nonrec 'v set [@@ligo.internal.constant "set"]
    type nonrec contract [@@ligo.internal.constant "contract"]
    type nonrec michelson_or [@@ligo.internal.constant "michelson_or"]
    type nonrec michelson_pair [@@ligo.internal.constant "michelson_pair"]
    type nonrec chain_id [@@ligo.internal.constant "chain_id"]
    type nonrec baker_hash [@@ligo.internal.constant "baker_hash"]
    type nonrec pvss_key [@@ligo.internal.constant "pvss_key"]
    type nonrec sapling_state [@@ligo.internal.constant "sapling_state"]
    type nonrec sapling_transaction [@@ligo.internal.constant "sapling_transaction"]
    type nonrec baker_operation [@@ligo.internal.constant "baker_operation"]
    type nonrec bls12_381_g1 [@@ligo.internal.constant "bls12_381_g1"]
    type nonrec bls12_381_g2 [@@ligo.internal.constant "bls12_381_g2"]
    type nonrec bls12_381_fr [@@ligo.internal.constant "bls12_381_fr"]
    type nonrec never [@@ligo.internal.constant "never"]
    type nonrec ticket [@@ligo.internal.constant "ticket"]
    type nonrec chest [@@ligo.internal.constant "chest"]
    type nonrec chest_key [@@ligo.internal.constant "chest_key"]
    (* Ligo stdlib *)]


let main () =
  let code =
    [%str
      type nonrec nat [@@ligo.internal.constant "nat"]
      type nonrec tez [@@ligo.internal.constant "tez"]
      type nonrec address [@@ligo.internal.constant "address"]

      external nat : int -> nat = "%ligo.nat"
      external tez : int -> tez = "%ligo.tez"
      external address : string -> address = "%ligo.address"

      let x = nat 1
      let y = tez 1
      let z = address "a"]
  in
  let code = OCaml.(type_str env code) in
  let code = Ligo.(compile_str env code) in
  let code =
    let raw_options = Compiler_options.Raw_options.make () in
    let options = Compiler_options.make ~raw_options () in
    let stdlib = Build.Stdlib.get ~options in
    let stdlib = stdlib.content_typed.pr_sig in
    Simple_utils.Trace.to_stdlib_result ~fast_fail:No_fast_fail
    @@ Simple_utils.Trace.trace Main_errors.checking_tracer
    @@ Checking.type_program ~options:options.middle_end ~env:stdlib code
  in
  (* TODO: this is weird *)
  Format.set_formatter_out_channel stderr;
  let rec pp_errors fmt errors =
    match errors with
    | [] -> ()
    | error :: errors ->
      Format.fprintf
        fmt
        "%a\n%a"
        (Main_errors.Formatter.error_ppformat ~display_format:Dev ~no_colour:false)
        error
        pp_errors
        errors
  in
  let code =
    match code with
    | Ok (code, [], []) ->
      (* TODO: warnings? *)
      code
    | Ok (code, [], warnings) ->
      Format.printf "warnings : %d\n%!" @@ List.length warnings;
      failwith "warnings"
    | Ok _ -> failwith "an error happened, but ok"
    | Error (errors, _warnings) ->
      Format.printf "%a\n%!" pp_errors errors;
      failwith "an error happened"
  in
  let () =
    (* TODO: something hijacks Format buffer? *)
    Format.printf "%a\n%!" (Ast_typed.PP.program ~use_hidden:false) code
  in
  ()


let () = main ()
