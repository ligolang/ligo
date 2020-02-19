type commit = {
  date: timestamp;
  salted_hash: bytes;
}

type commit_set = (address, commit) big_map

type storage = {
  hashed: bytes;
  unused: bool;
  commits: commit_set;
}

type reveal = {
  hashable: bytes;
  message: unit -> operation list;
}

type parameter =
| Commit of bytes
| Reveal of reveal

(* We use hash-commit so that a baker can't steal *)
let commit ((p,s): bytes * storage) : operation list * storage =
  let commit: commit = {date = Current.time + 86400; salted_hash = p;} in
  let updated_map: commit_set = Big_map.update sender (Some commit) s.commits in
  let s = {hashed = s.hashed; unused = s.unused; commits = updated_map} in
  (([]: operation list), s)

let reveal ((p,s): reveal * storage) : operation list * storage =
  if not s.unused
  then (failwith "This contract has already been used.": operation list * storage)
  else
  let commit: commit =
    match (Big_map.find_opt sender s.commits) with
    | Some c -> c
    | None -> (failwith "You haven't made a commitment to hash against yet.": commit)
  in
  if Current.time < commit.date
  then (failwith "It hasn't been 24 hours since your commit yet.": operation list * storage)
  else
  let salted = Crypto.sha256 (Bytes.concat p.hashable (Bytes.pack sender)) in
  if (salted <> commit.salted_hash)
  then (failwith "This reveal doesn't match your commitment.": operation list * storage)
  else
  if (s.hashed = Crypto.sha256 p.hashable) 
  then
    let s: storage = {hashed = s.hashed; unused = false; commits = s.commits} in
    ((p.message ()), s)
  else (failwith "Your commitment did not match the storage hash.":
          operation list * storage)

let main ((p,s): parameter * storage) : operation list * storage =
  match p with
  | Commit c -> commit (c, s)
  | Reveal r -> reveal (r, s)
