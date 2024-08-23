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
  open Caml_extraction
  open Caml_solving

  let env = Env.initial

  let compile_str env str =
    let str = extract_str str in
    let env, str = solve_module env str in
    str
end

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
        type nonrec nat [@@ligo.internal.constant "nat"]
        type nonrec tez [@@ligo.internal.constant "tez"]
        type nonrec address [@@ligo.internal.constant "address"]

        external nat : int -> nat = "%ligo.nat"
        external tez : int -> tez = "%ligo.tez"
        external address : string -> address = "%ligo.address"

        let x = nat 1
        let y = tez 1
        let z = address "a"] )
  ]


(* TODO: by the end ensure the OCaml produced stdlib
  is equivalent to the Ligo one in names and types *)

let main () =
  let code =
    [%str
      (* TODO: major concern on using aliases
        is about shadowing names on the LSP *)
      (* predefs *)
      type nonrec int = int [@@ligo.internal.predef]

      (* type nonrec char = char [@@ligo.internal.predef.unsupported] *)
      type nonrec string = string [@@ligo.internal.predef]
      type nonrec bytes = bytes [@@ligo.internal.predef]
      (* type nonrec float = float [@@ligo.internal.unsupported] *)

      (* type nonrec bool = bool =
        | false
        | true
      [@@ligo.internal.predef] *)

      (*
      
      type nonrec exn = exn [@@ligo.internal.unsupported]
      type nonrec 'a array = 'a array [@@ligo.internal.unsupported]

      *)
      (* type nonrec 'a list = 'a list =
        | []
        | ( :: ) of 'a * 'a list
      [@@ligo.internal.predef "list"] *)

      (* 
      type nonrec 'a option = 'a option =
        | None
        | Some of 'a
      [@@ligo.internal.predef "option"]

      type nonrec nativeint = nativeint [@@ligo.internal.unsupported]
      type nonrec int32 = int32 [@@ligo.internal.unsupported]
      type nonrec int64 = int64 [@@ligo.internal.unsupported]
      type nonrec 'a lazy_t = 'a lazy_t [@@ligo.internal.unsupported]

      type nonrec extension_constructor = extension_constructor
      [@@ligo.internal.unsupported]

      type nonrec floatarray = floatarray [@@ligo.internal.unsupported] *)]
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
