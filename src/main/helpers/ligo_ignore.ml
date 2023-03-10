type t = Re.re list

let default = [ "/.ligo"; "/_esy"; "/node_modules"; "/esy.lock"; "/.git" ]

let read ~ligoignore_path : t =
  let entries =
    try
      ligoignore_path
      |> In_channel.read_lines
      |> (* Ignore comments (line that start with #) *)
      List.filter ~f:(Fn.compose not (String.is_prefix ~prefix:"#"))
      |> (* Ignore empty lines *)
      List.filter ~f:(Fn.compose not String.is_empty)
    with
    | _ -> []
  in
  let ligoignore = List.dedup_and_sort ~compare:String.compare @@ entries @ default in
  List.map ligoignore ~f:(fun glob -> Re.Glob.glob glob |> Re.compile)


let matches ligoignore path = List.exists ligoignore ~f:(fun glob -> Re.execp glob path)
