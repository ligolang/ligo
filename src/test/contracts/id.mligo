type id = int

type id_details = {
  owner: address;
  controller: address;
  profile: bytes
}

type buy = bytes * address option
type update_owner = id * address
type update_details = id * bytes option * address option

type action =
| Buy of buy
| Update_owner of update_owner
| Update_details of update_details
| Skip

(* The prices kept in storage can be changed by bakers, though they
   should only be adjusted down over time, not up. *)

type storage = (id, id_details) big_map * int * (tez * tez)

type return = operation list * storage

(* Preliminary thoughts on ids:

I very much like the simplicity of http://gurno.com/adam/mne/.  5 three
letter words means you have a 15 character identity, not actually more
annoying than an IP address and a lot more memorable than the raw
digits. This can be stored as a single integer which is then
translated into the corresponding series of 5 words.

I in general like the idea of having a 'skip' mechanism, but it does
need to cost something so people don't eat up the address space. 256^5
means you have a lot of address space, but if people troll by skipping
a lot that could be eaten up.  Should probably do some napkin
calculations for how expensive skipping needs to be to deter people
from doing it just to chew up address space.  *)

let buy (parameter, storage: (bytes * address option) * storage) =
  let void : unit =
    if Tezos.amount <> storage.2.0
    then (failwith "Incorrect amount paid.": unit) in
  let profile, initial_controller = parameter in
  let identities, new_id, prices = storage in
  let controller : address =
    match initial_controller with
    | Some addr -> addr
    | None -> sender in
  let new_id_details: id_details = {
    owner = sender;
    controller = controller;
    profile = profile} in
  let updated_identities : (id, id_details) big_map =
    Big_map.update new_id (Some new_id_details) identities
  in ([]: operation list), (updated_identities, new_id + 1, prices)

let update_owner (parameter, storage : (id * address) * storage) =
  if amount <> 0tez
  then (failwith "Updating owner doesn't cost anything.": return)
  else
    let id, new_owner = parameter in
    let identities, last_id, prices = storage in
    let current_id_details : id_details =
      match Big_map.find_opt id identities with
      | Some id_details -> id_details
      | None -> (failwith "This ID does not exist." : id_details) in
    let is_allowed : bool =
      if Tezos.sender = current_id_details.owner
      then true
      else (failwith "You are not the owner of this ID." : bool) in
  let updated_id_details : id_details = {
    owner = new_owner;
    controller = current_id_details.controller;
    profile = current_id_details.profile} in
  let updated_identities =
    Big_map.update id (Some updated_id_details) identities
  in ([]: operation list), (updated_identities, last_id, prices)

let update_details (parameter, storage: (id * bytes option * address option) * storage) =
  if Tezos.amount <> 0tez
  then
    (failwith "Updating details doesn't cost anything." : return)
  else
    let id, new_profile, new_controller = parameter in
    let identities, last_id, prices = storage in
    let current_id_details: id_details =
      match Big_map.find_opt id identities with
      | Some id_details -> id_details
      | None -> (failwith "This ID does not exist.": id_details) in
    let is_allowed : bool =
      if Tezos.sender = current_id_details.controller
         || Tezos.sender = current_id_details.owner
      then true
      else
        (failwith ("You are not the owner or controller of this ID.")
         : bool) in
    let owner : address = current_id_details.owner in
    let profile : bytes =
      match new_profile with
      | None -> (* Default *) current_id_details.profile
      | Some new_profile -> new_profile in
    let controller : address =
      match new_controller with
      | None -> (* Default *) current_id_details.controller
      | Some new_controller -> new_controller in
    let updated_id_details: id_details = {
        owner = owner;
        controller = controller;
        profile = profile} in
  let updated_identities: (id, id_details) big_map  =
    Big_map.update id (Some updated_id_details) identities
  in ([]: operation list), (updated_identities, last_id, prices)

(* Let someone skip the next identity so nobody has to take one that's
undesirable *)

let skip (p, storage: unit * storage) =
  let void : unit =
    if Tezos.amount <> storage.2.1
    then (failwith "Incorrect amount paid." : unit) in
  let identities, last_id, prices = storage in
  ([]: operation list), (identities, last_id + 1, prices)

let main (action, storage : action * storage) : return =
  match action with
  | Buy b -> buy (b, storage)
  | Update_owner uo -> update_owner (uo, storage)
  | Update_details ud -> update_details (ud, storage)
  | Skip -> skip ((), storage)
