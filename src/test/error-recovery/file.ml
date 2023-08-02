type file = string

type origin =
  | Test
  | Original
  | Recovered
  | Original_generated

type request =
  | Blank
  | Errors
  | Cst
  | Tokens
  | Cst_symbols

let to_string (file : file) request origin =
  let dir =
    match origin with
    | Test -> "."
    | Original -> "original"
    | Recovered -> "recovered"
    | Original_generated -> "generated"
  in
  match request with
  | None -> dir
  | Some request ->
    let file =
      match request with
      | Blank -> file
      | Errors -> file ^ ".errors"
      | Cst -> file ^ ".cst"
      | Tokens -> file ^ ".tokens"
      | Cst_symbols -> file ^ ".cst_symbols"
    in
    Filename.concat dir file


let files_in_dir () : file list =
  Caml.Sys.readdir "."
  |> Array.to_list
  |> List.filter ~f:(fun x ->
         match Filename.split_extension x with
         | _, Some "mligo" | _, Some "jsligo" | _, Some "ligo" -> true
         | _ -> false)
