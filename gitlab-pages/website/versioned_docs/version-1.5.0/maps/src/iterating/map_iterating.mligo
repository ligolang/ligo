let assert_all_greater_than_3 (m : (int, int) map) : unit =
  Map.iter (fun (_,v) -> assert (v > 3)) m  // The key is discarded