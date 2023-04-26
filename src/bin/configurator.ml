module C = Configurator.V1

let () =
  let f _ =
      C.Flags.write_sexp
        "platform-specific-compiler-flags.sexp"
        Caml.(
          if C.ocaml_config_var_exn (C.create "") "os_type" = "Win32"
          then ["-cclib"; "-lole32"; "-cclib"; "-luserenv"; "-cclib"; "-lbcrypt"]
          else []);
      C.Flags.write_sexp
        "platform-specific-linker-flags.sexp"
        Caml.(
          if C.ocaml_config_var_exn (C.create "") "system" = "macosx"
          then ["-cclib"; "-framework Security"]
          else [])
  in
  C.main ~name:"" f
