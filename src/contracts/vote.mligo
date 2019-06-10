type storage = {
  title : string ;
  candidates : (string , int) map ;
  voters : address set ;
  beginning_time : timestamp ;
  finish_time : timestamp ;
}

type init_action = (string * timestamp * timestamp)

type action =
  | Vote of string
  | Init of (string * timestamp * timestamp)

let init (init_params : init_action) (_ : storage) =
  let (title , s , t) = init_params in
  let candidates = Map [
      ("Yes" , 0) ;
      ("No" , 0)
    ] in
  (
    ([] : operation list),
    {
      title = title ;
      candidates = candidates ;
      voters = (Set [] : address set) ;
      beginning_time = s ;
      finish_time = t ;
    }
  )

let vote (parameter : string) (storage : storage) =
  let now = Current.time () in
  assert (now >= storage.beginning_time && storage.finish_time < now) ;

  let addr = Current.source () in
  assert (not Set.mem addr storage.voters) ;

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
						             )
let main (action : action) (storage : storage) =
  match action with
  | Vote p -> vote p storage
  | Init ps -> init ps storage
