type storage = {
  title : string ;
  candidates : (string , int) map ;
  voters : address set ;
  beginning_time : timestamp ;
  finish_time : timestamp ;
}

type init_action = {
  title : string ;
  beginning_time : timestamp ;
  finish_time : timestamp ;
}

type action =
  | Vote of string
  | Init of init_action

let init (init_params_s : init_action * storage) =
  let candidates = Map.literal [
      ("Yes" , 0) ;
      ("No" , 0)
    ] in
  (
    ([] : operation list),
    {
      title = init_params_s.0.title ;
      candidates = candidates ;
      voters = (Set.empty : address set) ;
      beginning_time = init_params_s.0.beginning_time ;
      finish_time = init_params_s.0.finish_time ;
    }
  )

let vote (ps : string * storage) =
  let now = Current.time in
  (* let _ = assert (now >= ps.1.beginning_time && ps.1.finish_time > now) in *)
  let addr = Current.sender in
  (* let _ = assert (not Set.mem addr ps.1.voters) in *)
  let x = Map.find ps.0 ps.1.candidates in
  (
    ([] : operation list),
    {
      title = ps.1.title ;
      candidates = Map.update ps.0 (Some (x + 1)) ps.1.candidates ;
      voters = Set.add addr ps.1.voters ;
      beginning_time = ps.1.beginning_time ;
      finish_time = ps.1.finish_time ;
    }
  )

let main (a_s : action * storage) =
  match a_s.0 with
  | Vote p -> vote (p, a_s.1)
  | Init ps -> init (ps, a_s.1)

