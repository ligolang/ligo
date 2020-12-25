open Cmdliner

let () = 
  let result = Cli.run () in
  let buffer = Buffer.contents Cli.buffer in
  if buffer = "ligo: \n" then 
    exit 1
  else (
    print_string buffer;
    Term.exit result
  )