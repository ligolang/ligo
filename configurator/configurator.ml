open! Core
module C = Configurator.V1
open Core

let () =
  let f _ =
    C.Flags.write_sexp
      "platform-specific-linker-flags.sexp"
      (if String.(C.ocaml_config_var_exn (C.create "") "system" = "macosx")
      then [ "-cclib"; "-framework Security" ]
      else [])
  in
  C.main ~name:"" f
