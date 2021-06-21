let cut = "./contract_under_test/contract_record_storage_ty.mligo"

let test =
  let baker = Test.nth_bootstrap_account 0 in
  let src = Test.nth_bootstrap_account 1 in

  let init_storage = Test.compile_expression (Some cut) [%cameligo ({| {foo = 0 ; bar = "bar"} |} : ligo_program) ] in
  let (addr, code, size) = Test.originate_from_file cut "main" init_storage 0tez in
  let ovens_map = Test.compile_expression_subst (Some cut) [%cameligo ({|
      (fun (x : storage) -> x.foo) ($store : storage)
    |} : ligo_program) ]
    [ ("store", Test.get_storage_of_address addr)]
  in
  ()
