let () = 
  let argv = Sys.get_argv () in
  let result = Cli.run ~argv () in
  Stdlib.exit result
