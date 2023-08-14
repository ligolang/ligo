// this tests that contextual expressions (e.g. let in ; type in ; mod in)
// propagate typing information to their bodies

type t = {one : int}

let f1 =
  (fun (i : unit) -> let _v = i in
     fun s -> s.one + 1
   : unit -> t -> int)

let f2 =
  (fun () -> type tt = int in
     fun s -> s.one + 1
   : unit -> t -> int)

let f3 =
  (fun () -> module A = Tezos in
     fun s -> s.one + 1
   : unit -> t -> int)
