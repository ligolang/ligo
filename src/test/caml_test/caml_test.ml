open Parsetree

let ( let* ) v f = Result.bind v ~f

module OCaml = struct
  open Ocaml_common

  let env = Env.initial_safe_string
  let _type_expr env expr = Typecore.type_expression env expr

  let type_str env str =
    let str, sig_, names, shape, env = Typemod.type_structure env str in
    str
end

module Ligo = struct
  let compile_str str =
    let* str =
      let open Caml_extraction in
      Context.run @@ fun ctx -> extract_str ctx str
    in
    let open Caml_solving in
    Context.run
    @@ fun ctx ->
    let _ctx, str = solve_module ctx str in
    str
end

let loc = Location.none

let _tests =
  [ "sequence", [%str let sequence x y = x]
  ; ( "packed_id"
    , [%str
        type nonrec packed_id = { f : 'a. 'a -> 'a }

        let id x = x
        let packed_id = { f = id }] )
  ; ( "simple module"
    , [%str
        module M = struct
          let id x = x
        end] )
  ; ( "record"
    , [%str
        type nonrec user =
          { id : int
          ; name : string
          }

        let michael = { id = 0; name = "Michael" }] )
  ; ( "defining variants"
    , [%str
        type nonrec user = { id : int }

        type nonrec example =
          | A of unit
          | B of user
          | C of user * user
          | D of { id : 'a. 'a -> 'a }] )
  ; ( "creating variants"
    , [%str
        type nonrec status =
          | Default
          | Blocked of string
          | Unblocked of string * string

        let x = Default
        let y = Blocked "bad user"
        let z = Unblocked ("bad user", "but nice")] )
  ; ( "inline records"
    , [%str
        type nonrec user =
          | Not_user
          | User of
              { id : int
              ; name : string
              }

        let a = Not_user
        let b = User { id = 1; name = "Billie Jean" }] )
  ; ( "matching on variants"
    , [%str
        type nonrec user =
          | A
          | B of int
          | C of int * int
          | User of
              { id : int
              ; name : string
              }

        let f x =
          match x with
          | A -> 0
          | B x -> x
          | C (x, y) -> y
          | User { id; name } -> id] )
  ; "expression annotation", [%str let f x : int = x]
  ; "pattern annotation", [%str let f (x : int) = x]
  ; "let annotation", [%str let f : int -> int = fun (x : int) : int -> x]
  ; ( "annotation instantiation"
    , [%str
        let id x = x
        let f : int -> int = id] )
  ; ( "boolean matching"
    , [%str
        let not b =
          match b with
          | true -> false
          | false -> true] )
  ; ( "ocaml predef types"
    , [%str
        let[@ligo.predef "int"] (_ : int -> _) = fun x -> x
        let[@ligo.predef "string"] (_ : string -> _) = fun x -> x
        let[@ligo.predef "list"] (_ : _ list -> _) = fun x -> x

        (* let[@ligo.predef "option"] (_ : _ option -> _) = fun x -> x *)
        let x = 1
        let y = "a"
        let z = [ 1, 2 ]] )
  ; ( "type constructors"
    , [%str
        type nonrec ('k, 'v) t = A of 'k

        let x : (int, int) t = A 1] )
  ; ( "forall on let"
    , [%str
        let id : 'a. 'a -> 'a = fun x -> x
        let sequence : 'a 'b. 'a -> 'b -> 'a = fun x y -> x

        type nonrec 'a t = A of 'a

        let a : 'a. 'a -> 'a t = fun x -> A x] )
  ; ( "record constructors"
    , [%str
        type nonrec 'a t =
          | A of
              { id : 'a
              ; name : int
              }
          | B of { x : int }

        let f id name = A { id; name }
        let b = B { x = 1 }] )
  ; ( "ligo literals"
    , [%str
        type nonrec nat [@@ligo.internal.predef "nat"]
        type nonrec tez [@@ligo.internal.predef "tez"]
        type nonrec address [@@ligo.internal.predef "address"]

        external nat : int -> nat = "%ligo.nat"
        external tez : int -> tez = "%ligo.tez"
        external address : string -> address = "%ligo.address"

        let x = nat 1
        let y = tez 1
        let z = address "a"] )
  ; ( "predefs"
    , [%str
        (* TODO: major concern on using aliases
        is about shadowing names on the LSP *)
        (* used by variants *)
        type nonrec unit = unit = () [@@ligo.internal.predef]

        (* OCaml predefs *)

        type nonrec int = int [@@ligo.internal.predef]
        type nonrec char = char [@@ligo.internal.predef.unsupported]
        type nonrec string = string [@@ligo.internal.predef]
        type nonrec bytes = bytes [@@ligo.internal.predef]
        type nonrec float = float [@@ligo.internal.predef.unsupported]

        type nonrec bool = bool =
          | false
          | true

        type nonrec exn = exn [@@ligo.internal.predef.unsupported]
        type nonrec 'a array = 'a array [@@ligo.internal.predef.unsupported]

        type nonrec 'a list = 'a list =
          | []
          | ( :: ) of 'a * 'a list
        [@@ligo.internal.predef]

        type nonrec 'a option = 'a option =
          | None
          | Some of 'a

        type nonrec nativeint = nativeint [@@ligo.internal.predef.unsupported]
        type nonrec int32 = int32 [@@ligo.internal.predef.unsupported]
        type nonrec int64 = int64 [@@ligo.internal.predef]
        type nonrec 'a lazy_t = 'a lazy_t [@@ligo.internal.predef.unsupported]

        type nonrec extension_constructor = extension_constructor
        [@@ligo.internal.predef.unsupported]

        type nonrec floatarray = floatarray [@@ligo.internal.predef.unsupported]

        (* Ligo Constants *)
        (* TODO: better letters for constructors *)
        type nonrec operation [@@ligo.internal.predef]
        type nonrec nat [@@ligo.internal.predef]
        type nonrec tez [@@ligo.internal.predef]
        type nonrec address [@@ligo.internal.predef]
        type nonrec signature [@@ligo.internal.predef]
        type nonrec key [@@ligo.internal.predef]
        type nonrec key_hash [@@ligo.internal.predef]
        type nonrec timestamp [@@ligo.internal.predef]
        type nonrec chain_id [@@ligo.internal.predef]
        type nonrec ('k, 'v) map [@@ligo.internal.predef]
        type nonrec ('k, 'v) big_map [@@ligo.internal.predef]
        type nonrec 'v set [@@ligo.internal.predef]
        type nonrec 'a contract [@@ligo.internal.predef]
        type nonrec ('l, 'r) michelson_or [@@ligo.internal.predef]
        type nonrec ('l, 'r) michelson_pair [@@ligo.internal.predef]
        type nonrec baker_hash [@@ligo.internal.predef]
        type nonrec pvss_key [@@ligo.internal.predef]
        type nonrec 'a sapling_transaction [@@ligo.internal.predef]
        type nonrec 'a sapling_state [@@ligo.internal.predef]
        type nonrec baker_operation [@@ligo.internal.predef]
        type nonrec bls12_381_g1 [@@ligo.internal.predef]
        type nonrec bls12_381_g2 [@@ligo.internal.predef]
        type nonrec bls12_381_fr [@@ligo.internal.predef]
        type nonrec never [@@ligo.internal.predef]
        type nonrec 'd ticket [@@ligo.internal.predef]
        type nonrec ('a, 'b) dynamic_entrypoint [@@ligo.internal.predef]
        type nonrec michelson_program [@@ligo.internal.predef]
        type nonrec ('a, 'b) michelson_contract [@@ligo.internal.predef]
        type nonrec ('a, 'b) typed_address [@@ligo.internal.predef]
        type nonrec mutation [@@ligo.internal.predef]
        type nonrec tx_rollup_l2_address [@@ligo.internal.predef]
        type nonrec 'a pbt_gen [@@ligo.internal.predef]
        type nonrec 'a views [@@ligo.internal.predef]
        type nonrec chest [@@ligo.internal.predef]
        type nonrec chest_key [@@ligo.internal.predef]] )
  ; ( "more ligo literals"
    , [%str
        let a = ()
        let b = false
        let c = Some 1
        let d = [ 1 ]] )
  ]


(* TODO: by the end ensure the OCaml produced stdlib
  is equivalent to the Ligo one in names and types *)

let main () =
  let loc =
    let fname, lnum, cnum, enum = __POS__ in
    let loc_start =
      Lexing.{ pos_fname = fname; pos_lnum = lnum; pos_bol = 0; pos_cnum = 0 }
    in
    let loc_end = loc_start in
    Warnings.{ loc_start; loc_end; loc_ghost = false }
  in
  let code =
    [%str
      (* TODO: major concern on using aliases
        is about shadowing names on the LSP *)
      (* used by variants *)
      type nonrec unit = unit = () [@@ligo.internal.predef]

      (* OCaml predefs *)

      type nonrec int = int [@@ligo.internal.predef]
      type nonrec char = char [@@ligo.internal.predef.unsupported]
      type nonrec string = string [@@ligo.internal.predef]
      type nonrec bytes = bytes [@@ligo.internal.predef]
      type nonrec float = float [@@ligo.internal.predef.unsupported]

      type nonrec bool = bool =
        | false
        | true

      type nonrec exn = exn [@@ligo.internal.predef.unsupported]
      type nonrec 'a array = 'a array [@@ligo.internal.predef.unsupported]

      type nonrec 'a list = 'a list =
        | []
        | ( :: ) of 'a * 'a list
      [@@ligo.internal.predef]

      type nonrec 'a option = 'a option =
        | None
        | Some of 'a

      type nonrec nativeint = nativeint [@@ligo.internal.predef.unsupported]
      type nonrec int32 = int32 [@@ligo.internal.predef.unsupported]
      type nonrec int64 = int64 [@@ligo.internal.predef]
      type nonrec 'a lazy_t = 'a lazy_t [@@ligo.internal.predef.unsupported]

      type nonrec extension_constructor = extension_constructor
      [@@ligo.internal.predef.unsupported]

      type nonrec floatarray = floatarray [@@ligo.internal.predef.unsupported]

      (* Ligo Constants *)
      (* TODO: better letters for constructors *)
      type operation [@@ligo.internal.predef]
      type nat [@@ligo.internal.predef]
      type tez [@@ligo.internal.predef]
      type address [@@ligo.internal.predef]
      type signature [@@ligo.internal.predef]
      type key [@@ligo.internal.predef]
      type key_hash [@@ligo.internal.predef]
      type timestamp [@@ligo.internal.predef]
      type chain_id [@@ligo.internal.predef]
      type ('k, 'v) map [@@ligo.internal.predef]
      type ('k, 'v) big_map [@@ligo.internal.predef]
      type 'v set [@@ligo.internal.predef]
      type 'a contract [@@ligo.internal.predef]
      type ('l, 'r) michelson_or [@@ligo.internal.predef]
      type ('l, 'r) michelson_pair [@@ligo.internal.predef]
      type baker_hash [@@ligo.internal.predef]
      type pvss_key [@@ligo.internal.predef]
      type 'a sapling_transaction [@@ligo.internal.predef]
      type 'a sapling_state [@@ligo.internal.predef]
      type baker_operation [@@ligo.internal.predef]
      type bls12_381_g1 [@@ligo.internal.predef]
      type bls12_381_g2 [@@ligo.internal.predef]
      type bls12_381_fr [@@ligo.internal.predef]
      type never [@@ligo.internal.predef]
      type 'd ticket [@@ligo.internal.predef]
      type ('a, 'b) dynamic_entrypoint [@@ligo.internal.predef]
      type michelson_program [@@ligo.internal.predef]
      type ('a, 'b) michelson_contract [@@ligo.internal.predef]
      type ('a, 'b) typed_address [@@ligo.internal.predef]
      type mutation [@@ligo.internal.predef]
      type tx_rollup_l2_address [@@ligo.internal.predef]
      type 'a pbt_gen [@@ligo.internal.predef]
      type 'a views [@@ligo.internal.predef]
      type chest [@@ligo.internal.predef]
      type chest_key [@@ligo.internal.predef]

      (* contract *)

      type storage =
        | A
        | B
        | C

      type return = operation list * storage

      let[@entry] set new_storage (_storage : storage) : return = [], new_storage

      let[@entry] next () (storage : storage) : return =
        let storage =
          match storage with
          | A -> B
          | B -> C
          | C -> A
        in
        [], storage]
  in
  (* TODO: please clean this file *)
  let code = OCaml.(type_str env code) in
  let code = Ligo.(compile_str code) in
  let code =
    match code with
    | Ok code -> code
    | Error exn ->
      failwith @@ Format.asprintf "an exception at compile %s" (Exn.to_string exn)
  in
  let code =
    let raw_options = Compiler_options.Raw_options.make () in
    let options = Compiler_options.make ~raw_options () in
    Simple_utils.Trace.to_stdlib_result ~fast_fail:No_fast_fail
    @@ Simple_utils.Trace.trace Main_errors.checking_tracer
    @@ Checking.type_program ~options:options.middle_end code
  in
  (* TODO: this is weird *)
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
      print_endline @@ Format.asprintf "%a\n%!" pp_errors errors;
      failwith "an error happened"
  in
  let () =
    (* TODO: something hijacks Format buffer? *)
    Format.printf "%a\n%!" (Ast_typed.PP.program ~use_hidden:false) code
  in
  ()


let () =
  let () = Printexc.record_backtrace true in
  try main () with
  | exn ->
    print_endline @@ Exn.to_string exn;
    raise exn
