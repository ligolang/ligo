module Test = Test.Next

module C = struct
  // This is mutation-contract.mligo
  type storage = int

  type parameter =
    Increment of int
  | Decrement of int

  type return = operation list * storage

  // Two entrypoints
  let add (store, delta : storage * int) : storage = store + delta
  [@no_mutation] let sub (store, delta : storage * int) : storage = store - delta

  (* Main access point that dispatches to the entrypoints according to
    the smart contract parameter. *)
  [@entry]
  let main (action : parameter) (store : storage) : return =
    [@no_mutation] let _ = Assert.assert (0 = 0) in
    ([] : operation list),    // No operations
    (match action with
      Increment (n) -> add (store, n)
    | Decrement (n) -> sub (store, n))
end

let originate_and_test contract =
  let initial_storage = 7 in
  let orig = Test.Originate.contract contract initial_storage 0tez in
  let _ = Test.Typed_address.transfer_exn orig.taddr (Main (Increment (7)) : C parameter_of) 1mutez in
  Test.Assert.assert (Test.Typed_address.get_storage orig.taddr = initial_storage + 7)

let test = originate_and_test (contract_of C)

let test_mutation =
  match Test.Mutation.func (contract_of C) originate_and_test with
    None -> ()
  | Some (_, mutation) -> let () = Test.IO.log(mutation) in
                          failwith "Some mutation also passes the tests! ^^"

let test_mutation_all =
  match Test.Mutation.All.func (contract_of C) originate_and_test with
    [] -> ()
  | (_, mutation) :: _ -> let () = Test.IO.log(mutation) in
                          failwith "Some mutation also passes the tests! ^^"
