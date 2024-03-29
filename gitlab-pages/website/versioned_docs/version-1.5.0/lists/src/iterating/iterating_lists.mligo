let assert_all_greater_than_three (l : int list) : unit =
  List.iter (fun i -> assert (i > 3)) l