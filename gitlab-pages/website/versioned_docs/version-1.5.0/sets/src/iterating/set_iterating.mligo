let assert_all_greater_than_3 (s : int set) : unit =
  Set.iter (fun i -> assert (i > 3)) s