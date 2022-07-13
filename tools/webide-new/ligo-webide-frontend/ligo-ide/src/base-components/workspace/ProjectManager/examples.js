'use strict'

const incrementM = `
type storage = int

type parameter =
  Increment of int
| Decrement of int
| Reset

type return = operation list * storage

// Two entrypoints

let add (store, delta : storage * int) : storage = store + delta
let sub (store, delta : storage * int) : storage = store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)
   
let main (action, store : parameter * storage) : return =
 ([] : operation list),    // No operations
 (match action with
   Increment (n) -> add (store, n)
 | Decrement (n) -> sub (store, n)
 | Reset         -> 0)

`

const incrementL = `
type storage is int

type parameter is
  Increment of int
| Decrement of int
| Reset

type return is list (operation) * storage

// Two entrypoints

function add (const store : storage; const delta : int) : storage is 
  store + delta

function sub (const store : storage; const delta : int) : storage is 
  store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)
   
function main (const action : parameter; const store : storage) : return is
 ((nil : list (operation)),    // No operations
  case action of
    Increment (n) -> add (store, n)
  | Decrement (n) -> sub (store, n)
  | Reset         -> 0
  end)

`

const incrementR = `
type storage = int;

type parameter =
  Increment (int)
| Decrement (int)
| Reset;

type return = (list (operation), storage);

// Two entrypoints

let add = ((store, delta) : (storage, int)) : storage => store + delta;
let sub = ((store, delta) : (storage, int)) : storage => store - delta;

/* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. */
   
let main = ((action, store) : (parameter, storage)) : return => {
 (([] : list (operation)),    // No operations
 (switch (action) {
  | Increment (n) => add ((store, n))
  | Decrement (n) => sub ((store, n))
  | Reset         => 0}))
};

`

const incrementJ = `
type storage = int;

type parameter =
| ["Increment", int]
| ["Decrement", int]
| ["Reset"];

type return_ = [list <operation>, storage];

/* Two entrypoints */
let add = ([store, delta] : [storage, int]) : storage => store + delta;
let sub = ([store, delta] : [storage, int]) : storage => store - delta;

/* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. */
let main = ([action, store] : [parameter, storage]) : return_ => {
 return [
   (list([]) as list <operation>),    // No operations
   (match (action, {
    Increment: (n: int) => add ([store, n]),
    Decrement: (n: int) => sub ([store, n]),
    Reset:     ()  => 0}))
  ]
};

`

const idM = `
type id = int

type id_details = {
  owner: address;
  controller: address;
  profile: bytes;
}

type buy = {
  profile: bytes;
  initial_controller: address option;
}

type update_owner = {
  id: id;
  new_owner: address;
}

type update_details = {
  id: id;
  new_profile: bytes option;
  new_controller: address option;
}

type action =
| Buy of buy
| Update_owner of update_owner
| Update_details of update_details
| Skip of unit

(* The prices kept in storage can be changed by bakers, though they should only be
   adjusted down over time, not up. *)
type storage = {
  identities: (id, id_details) big_map;
  next_id: int;
  name_price: tez;
  skip_price: tez;
}

(** Preliminary thoughts on ids:

I very much like the simplicity of http://gurno.com/adam/mne/

Five three letter words means you have a 15 character identity, not actually more
annoying than an IP address and a lot more memorable than the raw digits. This
can be stored as a single integer which is then translated into the corresponding
series of 5 words.

I, in general like the idea of having a 'skip' mechanism, but it does need to cost
something so people don't eat up the address space. 256 ^ 5 means you have a lot
of address space, but if people troll by skipping a lot that could be eaten up.
Should probably do some napkin calculations for how expensive skipping needs to
be to deter people from doing it just to chew up address space.
*)

let buy (parameter, storage: buy * storage) =
  let void: unit = 
    if amount = storage.name_price 
    then () 
    else (failwith "Incorrect amount paid.": unit)
  in
  let profile = parameter.profile in
  let initial_controller = parameter.initial_controller in
  let identities = storage.identities in
  let new_id = storage.next_id in
  let controller: address =
    match initial_controller with
    | Some addr -> addr
    | None -> sender
  in
  let new_id_details: id_details = {
    owner = sender ;
    controller = controller ;
    profile = profile ;
  }
  in
  let updated_identities: (id, id_details) big_map =
    Big_map.update new_id (Some new_id_details) identities
  in
  ([]: operation list), {identities = updated_identities;
                         next_id = new_id + 1; 
                         name_price = storage.name_price;
                         skip_price = storage.skip_price;
                        }

let update_owner (parameter, storage: update_owner * storage) =
  if (amount <> 0mutez)
  then (failwith "Updating owner doesn't cost anything.": (operation list) * storage)
  else
  let id = parameter.id in
  let new_owner = parameter.new_owner in
  let identities = storage.identities in
  let current_id_details: id_details =
    match Big_map.find_opt id identities with
    | Some id_details -> id_details
    | None -> (failwith "This ID does not exist.": id_details)
  in
  let is_allowed: bool =
    if sender = current_id_details.owner
    then true
    else (failwith "You are not the owner of this ID.": bool)
  in
  let updated_id_details: id_details = {
    owner = new_owner;
    controller = current_id_details.controller;
    profile = current_id_details.profile;
  }
  in
  let updated_identities = Big_map.update id (Some updated_id_details) identities in
  ([]: operation list), {identities = updated_identities; 
                         next_id = storage.next_id;
                         name_price = storage.name_price;
                         skip_price = storage.skip_price;
                        }

let update_details (parameter, storage: update_details * storage) =
  if (amount <> 0mutez)
  then (failwith "Updating details doesn't cost anything.": (operation list) * storage)
  else
  let id = parameter.id in
  let new_profile = parameter.new_profile in
  let new_controller = parameter.new_controller in
  let identities = storage.identities in
  let current_id_details: id_details =
    match Big_map.find_opt id identities with
    | Some id_details -> id_details
    | None -> (failwith "This ID does not exist.": id_details)
  in
  let is_allowed: bool =
    if (sender = current_id_details.controller) || (sender = current_id_details.owner)
    then true
    else (failwith ("You are not the owner or controller of this ID."): bool)
  in
  let owner: address = current_id_details.owner in
  let profile: bytes =
    match new_profile with
    | None -> (* Default *) current_id_details.profile
    | Some new_profile -> new_profile
  in
  let controller: address =
    match new_controller with
    | None -> (* Default *) current_id_details.controller
    | Some new_controller -> new_controller
  in
  let updated_id_details: id_details = {
    owner = owner;
    controller = controller;
    profile = profile;
  }
  in
  let updated_identities: (id, id_details) big_map  =
    Big_map.update id (Some updated_id_details) identities in
  ([]: operation list), {identities = updated_identities;
                         next_id = storage.next_id;
                         name_price = storage.name_price;
                         skip_price = storage.skip_price;
                        }

(* Let someone skip the next identity so nobody has to take one that's undesirable *)
let skip (p,storage: unit * storage) =
  let void: unit =
    if amount = storage.skip_price
    then ()
    else (failwith "Incorrect amount paid.": unit)
  in
  ([]: operation list), {identities = storage.identities;
                         next_id = storage.next_id + 1;
                         name_price = storage.name_price;
                         skip_price = storage.skip_price;
                        }

let main (action, storage: action * storage) : operation list * storage =
  match action with
  | Buy b -> buy (b, storage)
  | Update_owner uo -> update_owner (uo, storage)
  | Update_details ud -> update_details (ud, storage)
  | Skip s -> skip ((), storage)

`

const idL = `
type id is int

type id_details is
  record [
    owner: address;
    controller: address;
    profile: bytes;
  ]

type buy is
  record [
    profile: bytes;
    initial_controller: option(address);
  ]

type update_owner is
  record [
    id: id;
    new_owner: address;
  ]

type update_details is
  record [
    id: id;
    new_profile: option(bytes);
    new_controller: option(address);
  ]

type action is
  | Buy of buy
  | Update_owner of update_owner
  | Update_details of update_details
  | Skip of unit

(* The prices kept in storage can be changed by bakers, though they should only be
   adjusted down over time, not up. *)
type storage is
  record [
    identities: big_map (id, id_details);
    next_id: int;
    name_price: tez;
    skip_price: tez;
  ]

(** Preliminary thoughts on ids:

I very much like the simplicity of http://gurno.com/adam/mne/.
5 three letter words means you have a 15 character identity, not actually more
annoying than an IP address and a lot more memorable than the raw digits. This
can be stored as a single integer which is then translated into the corresponding
series of 5 words.

I in general like the idea of having a 'skip' mechanism, but it does need to cost
something so people don't eat up the address space. 256 ^ 5 means you have a lot
of address space, but if people troll by skipping a lot that could be eaten up.
Should probably do some napkin calculations for how expensive skipping needs to
be to deter people from doing it just to chew up address space.
*)

function buy (const parameter : buy; const storage : storage) : list(operation) * storage is
  begin
    if amount = storage.name_price
    then skip
    else failwith("Incorrect amount paid.");
    const profile : bytes = parameter.profile;
    const initial_controller : option(address) = parameter.initial_controller;
    var identities : big_map (id, id_details) := storage.identities;
    const new_id : int = storage.next_id;
    const controller : address =
      case initial_controller of
        Some(addr) -> addr
      | None -> sender
      end;
    const new_id_details: id_details =
      record [
              owner = sender ;
              controller = controller ;
              profile = profile ;
      ];
    identities[new_id] := new_id_details;
  end with ((nil : list(operation)), record [
                              identities = identities;
                              next_id = new_id + 1;
                              name_price = storage.name_price;
                              skip_price = storage.skip_price;
                              ])

function update_owner (const parameter : update_owner; const storage : storage) :
         list(operation) * storage is
  begin
    if (amount =/= 0mutez)
    then
      begin
        failwith("Updating owner doesn't cost anything.");
      end
    else skip;
    const id : int = parameter.id;
    const new_owner : address = parameter.new_owner;
    var identities : big_map (id, id_details) := storage.identities;
    var id_details : id_details :=
      case identities[id] of
        Some(id_details) -> id_details
      | None -> (failwith("This ID does not exist."): id_details)
      end;
    var is_allowed : bool := False;
    if sender = id_details.owner
    then is_allowed := True
    else failwith("You are not the owner of this ID.");
    id_details.owner := new_owner;
    identities[id] := id_details;
  end with ((nil: list(operation)), record [
                                     identities = identities;
                                     next_id = storage.next_id;
                                     name_price = storage.name_price;
                                     skip_price = storage.skip_price;
                                   ])

function update_details (const parameter : update_details; const storage : storage ) :
         list(operation) * storage is
  begin
    if (amount =/= 0mutez)
    then failwith("Updating details doesn't cost anything.")
    else skip;
    const id : int = parameter.id;
    const new_profile : option(bytes) = parameter.new_profile;
    const new_controller : option(address) = parameter.new_controller;
    var identities : big_map (id, id_details) := storage.identities;
    var id_details: id_details :=
      case identities[id] of
        Some(id_details) -> id_details
      | None -> (failwith("This ID does not exist."): id_details)
      end;
    var is_allowed : bool := False;
    if (sender = id_details.controller) or (sender = id_details.owner)
    then is_allowed := True
    else failwith("You are not the owner or controller of this ID.");
    const owner: address = id_details.owner;
    const profile: bytes =
      case new_profile of
        None -> (* Default *) id_details.profile
      | Some(new_profile) -> new_profile
      end;
    const controller: address =
    case new_controller of
      None -> (* Default *) id_details.controller
    | Some(new_controller) -> new_controller
    end;
    id_details.owner := owner;
    id_details.controller := controller;
    id_details.profile := profile;
    identities[id] := id_details;
  end with ((nil: list(operation)), record [
                                     identities = identities;
                                     next_id = storage.next_id;
                                     name_price = storage.name_price;
                                     skip_price = storage.skip_price;
                                    ])

(* Let someone skip the next identity so nobody has to take one that's undesirable *)
function skip_ (const p: unit; const storage: storage) : list(operation) * storage is
  begin
    if amount = storage.skip_price
    then skip
    else failwith("Incorrect amount paid.");
  end with ((nil: list(operation)), record [
                                     identities = storage.identities;
                                     next_id = storage.next_id + 1;
                                     name_price = storage.name_price;
                                     skip_price = storage.skip_price;
                                   ])

function main (const action : action; const storage : storage) : list(operation) * storage is
  case action of
  | Buy(b) -> buy (b, storage)
  | Update_owner(uo) -> update_owner (uo, storage)
  | Update_details(ud) -> update_details (ud, storage)
  | Skip(s) -> skip_ (unit, storage)
  end;

`

const idR = `
/* */

type id = int

type id_details = {
  owner: address,
  controller: address,
  profile: bytes,
}

type buy = {
  profile: bytes,
  initial_controller: option(address),
}

type update_owner = {
  id: id,
  new_owner: address,
}

type update_details = {
  id: id,
  new_profile: option(bytes),
  new_controller: option(address),
}

type action =
| Buy(buy)
| Update_owner(update_owner)
| Update_details(update_details)
| Skip(unit)

/* The prices kept in storage can be changed by bakers, though they should only be
   adjusted down over time, not up. */
type storage = {
  identities: big_map (id, id_details),
  next_id: int,
  name_price: tez,
  skip_price: tez,
}

/** Preliminary thoughts on ids:

I very much like the simplicity of http://gurno.com/adam/mne/.
5 three letter words means you have a 15 character identity, not actually more
annoying than an IP address and a lot more memorable than the raw digits. This
can be stored as a single integer which is then translated into the corresponding
series of 5 words.

I in general like the idea of having a 'skip' mechanism, but it does need to cost
something so people don't eat up the address space. 256 ^ 5 means you have a lot
of address space, but if people troll by skipping a lot that could be eaten up.
Should probably do some napkin calculations for how expensive skipping needs to
be to deter people from doing it just to chew up address space.
*/

let buy = ((parameter, storage): (buy, storage)) : (list(operation), storage) => {
  let void: unit =
    if (amount == storage.name_price) { (); }
    else { failwith("Incorrect amount paid."); };
  let profile = parameter.profile;
  let initial_controller = parameter.initial_controller;
  let identities = storage.identities;
  let new_id = storage.next_id;
  let controller: address =
    switch (initial_controller) {
      | Some(addr) => addr
      | None => sender
    };
  let new_id_details: id_details = {
    owner : sender,
    controller : controller,
    profile : profile,
  };
  let updated_identities: big_map (id, id_details) =
    Big_map.update(new_id, Some(new_id_details), identities);
  (([]: list(operation)), {
                           identities : updated_identities,
                           next_id : new_id + 1,
                           name_price : storage.name_price,
                           skip_price : storage.skip_price,
                        });
  };

let update_owner = ((parameter, storage): (update_owner, storage)) : (list(operation), storage) => {
  let void: unit =
    if (amount != 0mutez) {
      failwith("Updating owner doesn't cost anything.");
    }
    else { (); };
  let id : int = parameter.id;
  let new_owner = parameter.new_owner;
  let identities = storage.identities;
  let current_id_details: id_details =
    switch (Big_map.find_opt(id, identities)) {
      | Some(id_details) => id_details
      | None => (failwith("This ID does not exist."): id_details)
    };
  let is_allowed: bool =
    if (sender == current_id_details.owner) { true; }
    else { (failwith("You are not the owner of this ID."): bool); };
  let updated_id_details: id_details = {
    owner : new_owner,
    controller : current_id_details.controller,
    profile : current_id_details.profile,
  };
  let updated_identities = Big_map.update(id, (Some updated_id_details), identities);
  (([]: list(operation)), {
                           identities : updated_identities,
                           next_id : storage.next_id,
                           name_price : storage.name_price,
                           skip_price : storage.skip_price,
                        });
  };

let update_details = ((parameter, storage): (update_details, storage)) :
                   (list(operation), storage) => {
  let void : unit =
    if (amount != 0mutez) {
      failwith("Updating details doesn't cost anything.");
    }
    else { (); };
  let id = parameter.id;
  let new_profile = parameter.new_profile;
  let new_controller = parameter.new_controller;
  let identities = storage.identities;
  let current_id_details: id_details =
    switch (Big_map.find_opt(id, identities)) {
      | Some(id_details) => id_details
      | None => (failwith("This ID does not exist."): id_details)
    };
  let is_allowed: bool =
    if ((sender != current_id_details.controller) &&
        (sender != current_id_details.owner)) {
      (failwith ("You are not the owner or controller of this ID."): bool)
    }
    else { true; };
  let owner: address = current_id_details.owner;
  let profile: bytes =
    switch (new_profile) {
      | None => /* Default */ current_id_details.profile
      | Some(new_profile) => new_profile
    };
  let controller: address =
    switch (new_controller) {
      | None => /* Default */ current_id_details.controller
      | Some new_controller => new_controller
    };
  let updated_id_details: id_details = {
    owner : owner,
    controller : controller,
    profile : profile,
  };
  let updated_identities: big_map (id, id_details) =
    Big_map.update(id, (Some updated_id_details), identities);
  (([]: list(operation)), {
                            identities : updated_identities,
                            next_id : storage.next_id,
                            name_price : storage.name_price,
                            skip_price : storage.skip_price,
                          });
  };

/* Let someone skip the next identity so nobody has to take one that's undesirable */
let skip = ((p,storage): (unit, storage)) => {
  let void : unit =
    if (amount != storage.skip_price) {
      failwith("Incorrect amount paid.");
    }
    else { (); };
  (([]: list(operation)), {
                            identities : storage.identities,
                            next_id : storage.next_id + 1,
                            name_price : storage.name_price,
                            skip_price : storage.skip_price,
                          });
  };

let main = ((action, storage): (action, storage)) : (list(operation), storage) => {
  switch (action) {
    | Buy(b) => buy((b, storage))
    | Update_owner(uo) => update_owner((uo, storage))
    | Update_details ud => update_details((ud, storage))
    | Skip s => skip(((), storage))
  };
};

`

const idJ = `
/* */

type id = int;

type id_details = {
  owner: address,
  controller: address,
  profile: bytes,
};

type buy = {
  profile: bytes,
  initial_controller: option<address>,
};

type update_owner = {
  id: id,
  new_owner: address,
};

type update_details = {
  id: id,
  new_profile: option<bytes>,
  new_controller: option<address>,
}

type action =
| ["Buy", buy]
| ["Update_owner", update_owner]
| ["Update_details", update_details]
| ["Skip"];

/* The prices kept in storage can be changed by bakers, though they should only be
   adjusted down over time, not up. */
type storage = {
  identities: big_map<id, id_details>,
  next_id: int,
  name_price: tez,
  skip_price: tez,
};

/** Preliminary thoughts on ids:

I very much like the simplicity of http://gurno.com/adam/mne/.
5 three letter words means you have a 15 character identity, not actually more
annoying than an IP address and a lot more memorable than the raw digits. This
can be stored as a single integer which is then translated into the corresponding
series of 5 words.

I in general like the idea of having a 'skip' mechanism, but it does need to cost
something so people don't eat up the address space. 256 ^ 5 means you have a lot
of address space, but if people troll by skipping a lot that could be eaten up.
Should probably do some napkin calculations for how expensive skipping needs to
be to deter people from doing it just to chew up address space.
*/

let buy = ([parameter, storage]: [buy, storage]) : [list<operation>, storage] => {
  if (amount != storage.name_price)
    failwith("Incorrect amount paid.");
  let profile = parameter.profile;
  let initial_controller = parameter.initial_controller;
  let identities = storage.identities;
  let new_id = storage.next_id;
  let controller: address =
    match (initial_controller, {
      Some: addr => addr,
      None: () => sender
    });
  let new_id_details: id_details = {
    owner : sender,
    controller : controller,
    profile : profile,
  };
  let updated_identities: big_map<id, id_details> =
    Big_map.update(new_id, Some(new_id_details), identities);
  return [(list([]) as list<operation>), {
                           identities : updated_identities,
                           next_id : new_id + 1,
                           name_price : storage.name_price,
                           skip_price : storage.skip_price,
                        }];
  };

let update_owner = ([parameter, storage]: [update_owner, storage]) : [list<operation>, storage] => {
  if (amount != (0 as mutez)) {
    failwith("Updating owner doesn't cost anything.");
  }
  let id : int = parameter.id;
  let new_owner = parameter.new_owner;
  let identities = storage.identities;
  let current_id_details: id_details =
    match (Big_map.find_opt(id, identities), {
      Some: id_details => id_details,
      None: () => (failwith("This ID does not exist.") as id_details)
    });

  if (sender != current_id_details.owner)
    { failwith("You are not the owner of this ID."); }

  let updated_id_details: id_details = {
    owner : new_owner,
    controller : current_id_details.controller,
    profile : current_id_details.profile,
  };
  let updated_identities = Big_map.update(id, Some(updated_id_details), identities);
  return [(list([]) as list<operation>), {
                           identities : updated_identities,
                           next_id : storage.next_id,
                           name_price : storage.name_price,
                           skip_price : storage.skip_price,
                        }];
  };

let update_details = ([parameter, storage]: [update_details, storage]) :
                   [list<operation>, storage] => {
  if (amount != (0 as mutez)) {
    failwith("Updating details doesn't cost anything.");
  }
  let id = parameter.id;
  let new_profile = parameter.new_profile;
  let new_controller = parameter.new_controller;
  let identities = storage.identities;
  let current_id_details: id_details =
    match (Big_map.find_opt(id, identities), {
      Some: id_details => id_details,
      None: () => (failwith("This ID does not exist.") as id_details)
    });

  if ((sender != current_id_details.controller) &&
        (sender != current_id_details.owner)) {
      failwith ("You are not the owner or controller of this ID.");
  }

  let owner: address = current_id_details.owner;
  let profile: bytes =
    match (new_profile, {
      None: () => /* Default */ current_id_details.profile,
      Some: new_profile => new_profile
    });
  let controller: address =
    match (new_controller, {
      None: () => /* Default */ current_id_details.controller,
      Some: new_controller => new_controller
    });
  let updated_id_details: id_details = {
    owner : owner,
    controller : controller,
    profile : profile,
  };
  let updated_identities: big_map<id, id_details> =
    Big_map.update(id, Some(updated_id_details), identities);
  return [(list([]) as list<operation>), {
                            identities : updated_identities,
                            next_id : storage.next_id,
                            name_price : storage.name_price,
                            skip_price : storage.skip_price,
                          }];
  };

/* Let someone skip the next identity so nobody has to take one that's undesirable */
let skip = ([p,storage]: [unit, storage]) : [list<operation>, storage] => {
  if (amount != storage.skip_price) {
    failwith("Incorrect amount paid.");
  }

  return [(list([]) as list<operation>), {
                            identities : storage.identities,
                            next_id : storage.next_id + 1,
                            name_price : storage.name_price,
                            skip_price : storage.skip_price,
                          }];
  };

let main = ([action, storage]: [action, storage]) : [list<operation>, storage] => {
  return match (action, {
    Buy: b => buy([b, storage]),
    Update_owner: uo => update_owner([uo, storage]),
    Update_details: ud => update_details([ud, storage]),
    Skip: s => skip([unit, storage])
  });
};

`

const hashlockM = `
type commit = {
  date        : timestamp;
  salted_hash : bytes;
}

type commit_set = (address, commit) big_map

type storage = {
  hashed  : bytes;
  unused  : bool;
  commits : commit_set
}

type reveal = {
  hashable : bytes;
  message  : unit -> operation list
}

type parameter =
  Commit of bytes
| Reveal of reveal

type return = operation list * storage

(* We use hash-commit so that a baker can not steal *)

let commit (p, s : bytes * storage) : return =
  let commit : commit =
    {date = Tezos.now + 86_400; salted_hash = p} in
  let updated_map: commit_set =
    Big_map.update Tezos.sender (Some commit) s.commits in
  let s = {s with commits = updated_map}
  in ([] : operation list), s

let reveal (p, s : reveal * storage) : return =
  if not s.unused
  then
    (failwith "This contract has already been used." : return)
  else
    let commit : commit =
      match Big_map.find_opt sender s.commits with
    | Some c -> c
    | None ->
       (failwith "You have not made a commitment to hash against yet."
        : commit)
    in
    if Tezos.now < commit.date
    then
      (failwith "It has not been 24 hours since your commit yet.": return)
    else
      let salted =
        Crypto.sha256 (Bytes.concat p.hashable (Bytes.pack sender)) in
      if salted <> commit.salted_hash
      then
        (failwith "This reveal does not match your commitment.": return)
      else
        if s.hashed = Crypto.sha256 p.hashable
        then
          let s : storage = {s with unused = false}
          in p.message (), s
        else (failwith "Your commitment did not match the storage hash."
              : return)

let main (p, s : parameter * storage) : return =
  match p with
  | Commit c -> commit (c,s)
  | Reveal r -> reveal (r,s)

`

const hashlockL = `
type commit is record [
  date        : timestamp;
  salted_hash : bytes;
]

type commit_set is big_map(address, commit)

type storage is record [
  hashed  : bytes;
  unused  : bool;
  commits : commit_set
]

type reveal is record [
  hashable : bytes;
  message  : unit -> list(operation)
]

type parameter is
  Commit of bytes
| Reveal of reveal

type return is list(operation) * storage

(* We use hash-commit so that a baker can not steal *)

function commit (const p : bytes; var s: storage) : return is
  begin
    const commit : commit = record [date = Tezos.now + 86_400; salted_hash = p];
    const updated_map: commit_set = Big_map.update(Tezos.sender, Some(commit), s.commits);
    s := s with record [commits = updated_map];    
  end with ((nil : list(operation)), s)

function reveal (const p: reveal; var s: storage) : return is
  begin
    if not s.unused
    then failwith("This contract has already been used.")
    else skip;
    var commit : commit := record [date = (0: timestamp); salted_hash = ("": bytes)];
    case Big_map.find_opt(sender, s.commits) of
    | Some (c) -> commit := c
    | None -> failwith("You have not made a commitment to hash against yet.")
    end;
    if Tezos.now < commit.date
    then failwith("It has not been 24 hours since your commit yet.");
    else skip;
    const salted : bytes =
      Crypto.sha256(
        Bytes.concat(p.hashable, Bytes.pack(sender))
      );
    if salted =/= commit.salted_hash
    then failwith("This reveal does not match your commitment.")
    else skip;
    if s.hashed = Crypto.sha256(p.hashable)
    then s := s with record [unused = False]
    else failwith("Your commitment did not match the storage hash.");
  end with (p.message(unit), s)

function main (const p: parameter; const s: storage) : return is
  case p of
  | Commit (c) -> commit (c,s)
  | Reveal (r) -> reveal (r,s)
  end

`

const hashlockR = `
/*
*/
type commit = {
  date:        timestamp,
  salted_hash: bytes,
}

type commit_set = big_map(address, commit)

type storage = {
  hashed: bytes,
  unused: bool,
  commits: commit_set
}

type reveal = {
  hashable: bytes,
  message: (unit => list(operation))
}

type parameter =
  Commit (bytes)
| Reveal (reveal);

type return = (list(operation), storage)

/* We use hash-commit so that a baker can not steal */

let commit = ((p, s) : (bytes, storage)) : return => {
  let commit : commit = {date: Tezos.now + 86_400, salted_hash: p};
  let updated_map: commit_set = Big_map.update(Tezos.sender, Some(commit), s.commits);
  let s = {...s, commits: updated_map};
  (([] : list(operation)), s);
};

let reveal = ((p, s): (reveal, storage)) : return => {
  if (!s.unused) {
    failwith("This contract has already been used.");
  }
  else { (); };
  let commit_ : commit =
    switch (Big_map.find_opt(sender, s.commits)) {
    | Some (c) => c
    | None =>
       (failwith("You have not made a commitment to hash against yet."): commit)
    };
  if (Tezos.now < commit_.date) {
    failwith("It has not been 24 hours since your commit yet.");
  }
  else { (); };
  let salted : bytes =
    Crypto.sha256(
      Bytes.concat(p.hashable, Bytes.pack(Tezos.sender))
    );
  if (salted != commit_.salted_hash) {
    failwith("This reveal does not match your commitment.");
  }
  else { (); };
  if (s.hashed == Crypto.sha256(p.hashable)) {
    let s : storage = {...s, unused: false};
    (p.message(), s)
  }
  else {
    (failwith("Your commitment did not match the storage hash.") : return);
  };
};

let main = ((p, s): (parameter, storage)) : return => {
  switch (p) {
  | Commit (c) => commit((c,s))
  | Reveal (r) => reveal((r,s))
  };
};

`

const hashlockJ = `
/*
*/
type commit = {
  date:        timestamp,
  salted_hash: bytes,
};

type commit_set = big_map<address, commit>;

type storage = {
  hashed: bytes,
  unused: bool,
  commits: commit_set
};

type reveal = {
  hashable: bytes,
  message: ((_u:unit) => list<operation>)
};

type parameter =
| ["Commit", bytes]
| ["Reveal", reveal];

type return_ = [list<operation>, storage];

/* We use hash-commit so that a baker can not steal */

let commit = ([p, s] : [bytes, storage]) : return_ => {
  let commit_ : commit = {date: Tezos.now + 86_400, salted_hash: p};
  let updated_map: commit_set = Big_map.update(Tezos.sender, Some(commit_), s.commits);
  let s_ = {...s, commits: updated_map};
  return [(list([]) as list<operation>), s_];
};

let reveal = ([p, s]: [reveal, storage]) : return_ => {
  if (!s.unused) {
    failwith("This contract has already been used.");
  };

  let commit_ : commit =
    match (Big_map.find_opt(sender, s.commits), {
    Some: c => c,
    None: () =>
       (failwith("You have not made a commitment to hash against yet.") as commit)
    });
  if (Tezos.now < commit_.date) {
    failwith("It has not been 24 hours since your commit yet.");
  };

  let salted : bytes =
    Crypto.sha256(
      Bytes.concat(p.hashable, Bytes.pack(Tezos.sender))
    );
  if (salted != commit_.salted_hash) {
    failwith("This reveal does not match your commitment.");
  };

  if (s.hashed != Crypto.sha256(p.hashable)) {
    failwith("Your commitment did not match the storage hash.");
  };

  let s : storage = {...s, unused: false};
  return [p.message(), s];
};

let main = ([p, s]: [parameter, storage]) : return_ => {
  return match (p, {
    Commit: c => commit([c,s]),
    Reveal: r => reveal([r,s])
  });
};

`

const config = (name) => `{
  "main": "./contracts/${name}.mligo",
  "deploy": "./contracts/${name}.tz",
  "framework": "#framework",
  "compilers": {
    "solc": "0.6.12"
  }
}
`

export const getExamples = (name, template) => {
  if (template === 'increment') {
    return {
      storageM: { name: `.workspaces/${name}/contracts/Increment.mligo`, content: incrementM },
      storageL: { name: `.workspaces/${name}/contracts/Increment.ligo`, content: incrementL },
      storageR: { name: `.workspaces/${name}/contracts/Increment.religo`, content: incrementR },
      storageJ: { name: `.workspaces/${name}/contracts/Increment.jsligo`, content: incrementJ },
      readme: { name: `.workspaces/${name}/README.md`, content: `# ${name}` },
      config: { name: `.workspaces/${name}/config.json`, content: config('Increment') }
    }
  } else if (template === 'id') {
    return {
      storageM: { name: `.workspaces/${name}/contracts/ID.mligo`, content: idM },
      storageL: { name: `.workspaces/${name}/contracts/ID.ligo`, content: idL },
      storageR: { name: `.workspaces/${name}/contracts/ID.religo`, content: idR },
      storageJ: { name: `.workspaces/${name}/contracts/ID.jsligo`, content: idJ },
      readme: { name: `.workspaces/${name}/README.md`, content: `# ${name}` },
      config: { name: `.workspaces/${name}/config.json`, content: config('ID') }
    }
  } else if (template === 'hashlock') {
    return {
      storageM: { name: `.workspaces/${name}/contracts/Hashlock.mligo`, content: hashlockM },
      storageL: { name: `.workspaces/${name}/contracts/Hashlock.ligo`, content: hashlockL },
      storageR: { name: `.workspaces/${name}/contracts/Hashlock.religo`, content: hashlockR },
      storageJ: { name: `.workspaces/${name}/contracts/Hashlock.jsligo`, content: hashlockJ },
      readme: { name: `.workspaces/${name}/README.md`, content: `# ${name}` },
      config: { name: `.workspaces/${name}/config.json`, content: config('Hashlock') }
    }
  }

  return {
    storage: { name: `.workspaces/${name}/contracts/Contract.mligo`, content: '' },
    readme: { name: `.workspaces/${name}/README.md`, content: `# ${name}` },
    config: { name: `.workspaces/${name}/config.json`, content: config('Contract') }
  }
}
