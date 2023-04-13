module C = Configurator.V1

let () =
  C.main ~name:"" (fun _ ->
      C.Flags.write_sexp
        "win32-flags.sexp"
        Caml.(
          if C.ocaml_config_var_exn (C.create "") "os_type" = "Win32"
          then [ "-cclib"; "-lole32"; "-cclib"; "-luserenv"; "-cclib"; "-lbcrypt" ]
          else []))
