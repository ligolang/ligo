[@@@ligo]

(* contract *)
type storage =
  | A
  | B
  | C

type return = operation list * storage

let x : nat =
  (let module M = struct
     external magic : unit -> 'a = "%identity"
   end
   in
  M.magic ()) [@ligo.internal.literal 1n]

let add (x : nat) (y : nat) : nat =
  ((let module M = struct
      external magic : unit -> 'a = "%identity"
    end
    in
   M.magic ())
     x
     y [@ocaml.warning "-20"] [@ligo.internal.constant "ADD"])

let next () (storage : storage) : return = [], storage
