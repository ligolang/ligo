module Defs = struct
  type left = string
  type right = string
  type eq = unit
  type diff = unit
  type state = unit
end

module Define = Diffing.Define (Defs)

module Arg = struct
  let weight : Define.change -> int = function
  | Diffing.Delete _ -> 1
  | Diffing.Insert _ -> 1
  | Diffing.Keep _ -> 0
  | Diffing.Change _ -> 3
  let test : unit -> string -> string -> (unit, unit) result =
  fun _ s1 s2 -> if String.equal s1 s2 then Ok () else Error ()
  let update : Define.change -> unit -> unit = fun _ _ -> ()
end
module Diff : sig
  val diff_files : Buffer.t -> Buffer.t -> int
  val diff_errors : string -> string list -> (int, string) result
end = struct
  include Define.Simple (Arg)

  let eval : Define.change -> int = function
  | Diffing.Delete _ -> 1
  | Diffing.Insert _ -> 1
  | Diffing.Keep _ -> 0
  | Diffing.Change _ -> failwith "Unexpected diff result"

  let measure x y =
    let res = diff () x y in
    List.fold (List.map res ~f:eval) ~f:(+) ~init:0

  let convert x = List.to_array @@ String.split_lines (Buffer.contents x)

  let diff_files x y = measure (convert x) (convert y)

  let diff_errors parser_error recovered_errors_list =
    let err_diff = List.length recovered_errors_list - 1 in
    if not @@ List.mem recovered_errors_list parser_error ~equal:String.equal
      then Error ""
      else Ok err_diff
end
