let michelson_predef ~loc =
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

let michelson_predef ~loc =
  let open Ppxlib.Ast_builder.Default in
  let ocaml_predef = pmod_structure ~loc @@ michelson_predef ~loc in
  [%str include ([%m ocaml_predef] : sig end) [@@ligo.internal.ocaml.predef]]
