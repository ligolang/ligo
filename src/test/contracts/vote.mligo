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

let init (init_params : init_action) (_ : storage) =
  let candidates = Map.literal [
      ("Yes" , 0) ;
      ("No" , 0)
    ] in
  (
    ([] : operation list),
    {
      title = init_params.title ;
      candidates = candidates ;
      voters = (Set.empty : address set) ;
      beginning_time = init_params.beginning_time ;
      finish_time = init_params.finish_time ;
    }
  )

let vote (parameter : string) (storage : storage) =
  let now = Current.time in
  (* let _ = assert (now >= storage.beginning_time && storage.finish_time > now) in *)
  let addr = Current.source in
  (* let _ = assert (not Set.mem addr storage.voters) in *)
  let x = Map.find parameter storage.candidates in
  (
    ([] : operation list),
    {
      title = storage.title ;
      candidates = Map.update parameter (Some (x + 1)) storage.candidates ;
      voters = Set.add addr storage.voters ;
      beginning_time = storage.beginning_time ;
      finish_time = storage.finish_time ;
    }
  )

let main (action : action) (storage : storage) =
  match action with
  | Vote p -> vote p storage
  | Init ps -> init ps storage
