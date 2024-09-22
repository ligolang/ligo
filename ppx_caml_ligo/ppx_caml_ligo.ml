open Ppxlib

let stdlib ~loc =
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
    type chest_key [@@ligo.internal.predef]]

let () =
  let impl str =
    List.fold_right
      (fun stri str ->
        let { pstr_desc; pstr_loc } = stri in
        match pstr_desc with
        | Pstr_attribute
            { attr_name = { txt = "ligo"; loc = _ }
            ; attr_payload = PStr []
            ; attr_loc = _
            } -> stdlib ~loc:pstr_loc @ str
        | _ -> stri :: str)
      str
      []
  in
  Driver.register_transformation "ppx_caml_ligo" ~impl
