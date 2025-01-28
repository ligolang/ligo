open Ppxlib
open Ocaml_common

(* let coerce = 
  let x =
    (let module M = struct
       external magic : unit -> 'a = "%identity" [@@ligo.internal.michelson]
     end
     in
     M.magic ()
      : nat)
    [@@ligo.internal.coerce 1] *)
let ocaml_predef ~loc =
  [%str
    [@@@ocaml.warning "-34"]

    (* OCaml predefs *)
    type nonrec unit = unit = () [@@ligo.internal.ocaml.predef.register]

    (* TODO: different int for Ligo? *)
    type nonrec int = int [@@ligo.internal.ocaml.predef.register]
    type nonrec char = char [@@ligo.internal.ocaml.predef.unsupported]
    type nonrec string = string [@@ligo.internal.ocaml.predef.register]
    type nonrec bytes = bytes [@@ligo.internal.ocaml.predef.register]
    type nonrec float = float [@@ligo.internal.ocaml.predef.unsupported]

    type nonrec bool = bool =
      | false
      | true
    [@@ligo.internal.ocaml.predef.weird]

    type nonrec exn = exn [@@ligo.internal.ocaml.predef.unsupported]
    type nonrec 'a array = 'a array [@@ligo.internal.ocaml.predef.unsupported]

    type nonrec 'a list = 'a list =
      | []
      | ( :: ) of 'a * 'a list
    [@@ligo.internal.ocaml.predef.register]

    type nonrec 'a option = 'a option =
      | None
      | Some of 'a
    [@@ligo.internal.ocaml.predef.weird]

    type nonrec nativeint = nativeint [@@ligo.internal.ocaml.predef.unsupported]
    type nonrec int32 = int32 [@@ligo.internal.ocaml.predef.unsupported]
    type nonrec int64 = int64 [@@ligo.internal.ocaml.predef.register]
    type nonrec 'a lazy_t = 'a lazy_t [@@ligo.internal.ocaml.predef.unsupported]

    type nonrec extension_constructor = extension_constructor
    [@@ligo.internal.ocaml.predef.unsupported]

    type nonrec floatarray = floatarray [@@ligo.internal.ocaml.predef.unsupported]]

let stdlib ~loc =
  let open Ast_builder.Default in
  let ocaml_predef = pmod_structure ~loc @@ ocaml_predef ~loc in
  [%str
    include ([%m ocaml_predef] : sig end) [@@ligo.internal.ocaml.predef]

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

    (* module Int = struct
      external add : int -> int -> int = "%ligo" [@@ligo.internal.constant "ADD"]

      let add x y = add x y

      external sub : int -> int -> int = "SUB" [@@ligo.internal.michelson]

      let f x y = add 1 2
    end *)]

let loc_of_ligo_location ~loc =
  match (loc : Simple_utils.Location.t) with
  (* TODO: what is a ghost location? *)
  | File reg when reg#is_ghost -> Ocaml_common.Location.none
  | File reg ->
    let loc_start = reg#start#byte in
    let loc_end = reg#stop#byte in
    Ocaml_common.Location.{ loc_ghost = false; loc_start; loc_end }
  | Virtual _ ->
    (* TODO: is None okay? *)
    Location.none

(* TODO: Location seems to be too complex in ligo
  match loc_ghost with
  | true ->
    (* TODO: test ghost *)
    Location.File Region.ghost
  | false ->
    (* TODO: when problems cnum < bol *)
    (* TODO: test locations *)
    Location.make loc_start loc_end *)

let stri_of_error error =
  let open Caml_solving in
  let open Ast_builder.Default in
  let Caml_error.{ err_tag = tag; err_loc = loc } = error in
  let loc = loc_of_ligo_location ~loc in
  let message = Format.asprintf "%a" Caml_error.pp_hum_error_tag tag in
  let label = { txt = "ocaml.error"; loc } in
  let content = pstr_eval ~loc (estring ~loc message) [] in
  pstr_extension ~loc (label, PStr [ content ]) []

let env =
  (* TODO: disable stdlib *)
  lazy
    (Compmisc.init_path ();
     Compmisc.initial_env ())

let check_str str =
  (* TODO: better env thingy? *)
  let env = Lazy.force_val env in
  let tstr, _, _, _, _ = Typemod.type_structure env str in
  tstr

let check_extract str =
  let open Caml_solving in
  let ( let* ) v f = Result.bind v f in
  let* str = Caml_extract.extract_str str in
  let* str = Caml_solve.solve_module str in
  let errors = Caml_error_collect.collect_module str in
  Ok (List.map stri_of_error errors)

let check_extract str =
  (* TODO: this should not be like this *)
  match check_extract @@ check_str str with
  | Ok errors -> errors
  | Error error -> [ stri_of_error error ]
  | exception _exn ->
    Format.eprintf "%s\n%!" (Printexc.get_backtrace ());
    (* TODO: properly manage this *)
    assert false

let () =
  let impl str =
    let additional_errors, str =
      match str with
      | { pstr_desc =
            Pstr_attribute
              { attr_name = { txt = "ligo"; loc = _ }; attr_payload; attr_loc = _ }
        ; pstr_loc = loc
        }
        :: str ->
        assert (attr_payload = PStr []);
        [], stdlib ~loc @ str
      | _ ->
        (* TODO: double locations *)
        let error_missing_ligo_attribute =
          let loc = Simple_utils.Location.dummy in
          stri_of_error @@ { err_tag = E_missing_ligo_attribute; err_loc = loc }
        in
        let loc = Location.none in
        (* TODO: is it okay to drop str here? *)
        [ error_missing_ligo_attribute ], stdlib ~loc @ str
    in
    let errors = check_extract str in
    (* TODO: @? *)
    additional_errors @ errors @ str
  in
  let impl str =
    try impl str with
    | exn ->
      let loc = Simple_utils.Location.dummy in
      let error_stri =
        stri_of_error @@ { err_tag = E_unexpected_error exn; err_loc = loc }
      in
      error_stri :: str
  in
  Driver.register_transformation "ppx_caml_ligo" ~impl
