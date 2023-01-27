type parameter = Foo of int | Bar of nat

let main (p : parameter) (s : int) : operation list * int =
  let k = match p with | Foo i -> i | Bar n -> n * 1 in
  [], s + k

let test =
  let (ta, _, _) = Test.originate main 0 0tez in
  let c = Test.to_contract ta in
  let a = Tezos.address c in
  let () = assert_some (Tezos.get_entrypoint_opt "%foo" a : (int contract) option) in
  let _ = (Tezos.get_entrypoint "%foo" a : (int contract)) in
  let () = assert_some (Tezos.get_contract_opt a : (parameter contract) option) in
  let _ = (Tezos.get_contract a : (parameter contract)) in
  let _ = (Tezos.get_contract_with_error a "foo" : (parameter contract)) in
  ()
