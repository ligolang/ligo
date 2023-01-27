type storage = int list
type parameter = bool

let main (p : parameter) (store : storage) : operation list * storage =
  let aux (i:int) : int = i + 1 in
  (([]: operation list) , (if p then List.map aux store else store))

let test =
  let big_list =
    let rec aux (acc: int list) (p: int) : int list =
      if p = 0 then acc else aux (p::acc) (p-1)
    in aux ([]:int list) 2000
  in
  let (ta, _, _) =  Test.originate main big_list 0tez in
  let c : parameter contract = Test.to_contract ta in
  let _caching =
    (* some caching is happening on the first transaction *)
    Test.transfer_to_contract c true 0tez
  in
  let tx1 = Test.transfer_to_contract c false 0tez in
  let tx2 = Test.transfer_to_contract c true 0tez in
  let tx3 = Test.transfer_to_contract c true 0tez in
  match (tx1 , tx2, tx3) with
  | Success cons1 , Success cons2, Success cons3 ->
    let () = assert ((cons1 < cons2) && (cons2 = cons3)) in
    (cons1,cons2,cons3)
  | _ ->
    (failwith "one of the transcations failed" : nat * nat * nat)
