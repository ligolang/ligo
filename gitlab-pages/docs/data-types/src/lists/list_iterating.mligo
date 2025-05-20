let assert_all_greater_than_3 (l : int list) : unit =
  List.iter (fun i -> Assert.assert (i > 3)) l