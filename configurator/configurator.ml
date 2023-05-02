module C = Configurator.V1

let () =
  let f _ =
    let ntdll_flags =
      match Sys.getenv_opt "LIGO_NTDLL_PATH" with
      | Some v -> [ v ^ "/ligoNtdll.a" ]
      | None -> []
    in
    C.Flags.write_sexp
      "platform-specific-compiler-flags.sexp"
      Caml.(
        if C.ocaml_config_var_exn (C.create "") "os_type" = "Win32"
        then
          [ "-cclib"; "-lole32"; "-cclib"; "-luserenv"; "-cclib"; "-lbcrypt"; "-cclib" ]
          @ ntdll_flags
        else []);
    C.Flags.write_sexp
      "platform-specific-linker-flags.sexp"
      Caml.(
        if C.ocaml_config_var_exn (C.create "") "system" = "macosx"
        then [ "-cclib"; "-framework Security" ]
        else [])
  in
  C.main ~name:"" f
