let testme_test = "./testme.mligo"

let test_prg (prg : ligo_program) = 
  let init_storage = Test.compile_expression (Some testme_test) [%cameligo ({| (10 : int) |} : ligo_program) ] in
  let (addr, _, _) = Test.originate_from_file testme_test "main" init_storage 0tez in
  let param = Test.compile_expression (Some testme_test) prg in
  let transfer_result = Test.transfer addr param 0tez in
  let result = Test.get_storage_of_address addr in
  let check_ = Test.compile_expression (None : string option) [%cameligo ({| (42: int) |} : ligo_program)] in
  let _ = Test.log prg in
  let _ = Test.log result in
  Test.michelson_equal result check_

let test =
  let prg : ligo_program = [%cameligo ({| Increment(32) |} : ligo_program)] in
  let count : nat = Test.mutate_count prg in
  let seeds : nat list = [1n; 2n; 3n; 4n; 5n] in
  let prgs : ligo_program list = List.map (fun (n : nat) -> Test.mutate_expression n prg) seeds in
  let bs = List.map (fun (prg : ligo_program) -> test_prg prg) prgs in
  let b = List.fold (fun (rb : bool * bool) -> rb.0 || rb.1) bs false in
  not b
