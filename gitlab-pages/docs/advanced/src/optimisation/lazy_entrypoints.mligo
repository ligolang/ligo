module LazyEntrypoint = struct
  type storage_type = {
    large_entrypoint_map : (bool, int -> int) big_map;
    value : int
  }
  type return_type = operation list * storage_type

  (* Load the code from the big-map *)
  let load_large_ep (storage : storage_type) : (int -> int) =
    let large_entrypoint_opt =
      Big_map.find_opt true storage.large_entrypoint_map in
    match large_entrypoint_opt with
      Some ep -> ep
    | None -> failwith "Internal error"

  (* Run the code from the big-map *)
  [@entry]
  let large_entry_point (param : int) (storage : storage_type) : return_type =
    [], {storage with value = (load_large_ep storage) param}

  (* Do something that doesn't require the large code *)
  [@entry]
  let small_entry_point (param : int) (storage : storage_type) : return_type =
    [], {storage with value = param}

  (* Other entrypoints... *)

end