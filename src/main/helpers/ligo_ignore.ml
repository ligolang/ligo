type t = Re.re list

let default = [ ".ligo"; "_esy"; "node_modules"; "esy.lock"; ".git" ]

let read ~ligoignore_path : t =
  let entries =
    try In_channel.read_lines ligoignore_path with
    | _ -> []
  in
  let ligoignore =
    List.dedup_and_sort ~compare:String.compare @@ entries @ default
  in
  List.map ligoignore ~f:(fun glob ->
      Re.seq [ Re.bos; Re.Glob.glob glob; Re.eos ] |> Re.compile)


let matches ligoignore path =
  List.exists ligoignore ~f:(fun glob -> Re.execp glob path)
