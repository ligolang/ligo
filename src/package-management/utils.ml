let remove_unfriendly_filename_chars fname =
  (* In a filename ignore the following
     1. null char
     2. /
     3. \
     4. space *)
  let r = Str.regexp "[\000/\\ ]" in
  Str.global_replace r "_" fname


let with_logging ~before ?(after = "Done") fn =
  let () = Printf.printf "==> %s... %!" before in
  match fn () with
  | Ok v ->
    let () = Printf.printf "%s\n%!" after in
    Ok v
  | Error e ->
    let () = Printf.printf "\n%!" in
    Error e
