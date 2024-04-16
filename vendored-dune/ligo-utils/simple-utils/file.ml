let exists ?dir file_path =
  let path =
    match dir with
    | Some dir when String.(dir = "." || dir = "") -> file_path
    | Some dir -> dir ^ "/" ^ file_path
    | None -> file_path
  in
  match Sys_unix.file_exists path with
  | `Yes -> Some path
  | `Unknown | `No -> None

let read path =
  let ic = open_in path in
  try Some (In_channel.input_all ic) with
  | _ -> None
