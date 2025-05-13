type store =
  {
   a : int;
   b : nat;
   c : tez;
   d : address
  }

let foo (s : store) (_ : int) : store =
  let v : unit option = Tezos.call_view "foo" (Tezos.get_sender (), s.a) s.d in
  let () =
    match v with
      Some _ -> ()
    | None -> () in
  let v : unit option = Tezos.call_view "bar" s.b s.d in
  match v with
    Some _ -> s
  | None -> s

[@entry]
let main (_ : unit) (s : store) : operation list * store =
  let z = Some 1 in
  let u =
    match z with
      Some _ -> foo s 42
    | None -> s in
  ([] : operation list), u
