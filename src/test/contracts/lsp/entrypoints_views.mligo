module Storage = struct
  type t =
    {
     data : int;
     metadata : unit
    }

end

[@entry]
let good_ep : unit -> Storage.t -> operation list * Storage.t =
  fun () s -> [], s

[@entry]
let good_ep_2 : unit -> Storage.t -> operation list * Storage.t =
  fun () s -> [], {s with data = 0}

[@view]
let good_view = fun x (s : Storage.t) -> x * s.data

[@view]
let bad_view_not_func = 3
