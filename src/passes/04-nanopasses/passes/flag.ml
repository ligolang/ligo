module With_arg (F : sig
  type flag
end) =
struct
  type flag_arg = F.flag

  let flag : (bool * flag_arg) option ref = ref None
  let set_flag ~enable x = flag := Some (enable, x)

  let is_enabled () =
    match !flag with
    | Some (enabled, _) -> enabled
    | None -> failwith "One pass flag have not been set"


  let get_flag () =
    match !flag with
    | Some (_, x) -> x
    | None -> failwith "One pass flag have not been set"
end

module No_arg () = With_arg (struct
  type flag = unit
end)
