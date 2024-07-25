module Test = Test.Next

module C = struct
  type parameter = Foo of int | Bar of nat

  [@entry]
  let main (p : parameter) (s : int) : operation list * int =
    let k = match p with | Foo i -> i | Bar n -> n * 1 in
    [], s + k
end

let test =
  let {taddr=ta; code=_; size=_} =
    Test.Originate.contract (contract_of C) 0 0tez in
  let c = Test.Typed_address.to_contract ta in
  let a = Tezos.address c in
  let () = Assert.some (Tezos.get_entrypoint_opt "%foo" a : (int contract) option) in
  let _ = (Tezos.get_entrypoint "%foo" a : (int contract)) in
  let () = Assert.some (Tezos.get_contract_opt a : (C parameter_of contract) option) in
  let _ = (Tezos.get_contract a : (C parameter_of contract)) in
  let _ = (Tezos.get_contract_with_error a "foo" : (int contract)) in
  ()
