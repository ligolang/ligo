module C = struct
  [@entry] let main (p : int*int) () =
    [Tezos.emit "%foo" p ; Tezos.emit "%foo" p.0],()
end

let test_foo =
  let orig = Test.Next.Originate.contract (contract_of C) () 0tez in
  let _: nat = Test.Next.Typed_address.transfer_exn orig.taddr (Main (1,2)) 0tez in
  (Test.Next.State.last_events orig.taddr "foo" : (int*int) list),(Test.Next.State.last_events orig.taddr "foo" : int list)