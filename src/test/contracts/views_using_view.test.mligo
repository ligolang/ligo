module Proxy = struct
  type param =
      Basic of address * address 
    | Not_funny of address
    | Get_storage of address
    | Get_address of address
    | Super_not_funny of address

  type store = Address of address | Integer of int

  [@entry]
  let main (p : param) (_ : store) : operation list * store
    = [], (match p with
            Basic (v,a)       -> Integer (Option.unopt (Tezos.call_view "basic" a v))
          | Get_storage v     -> Integer (Option.unopt (Tezos.call_view "get_storage" () v))
          | Not_funny v       -> Integer (Option.unopt (Tezos.call_view "not_funny" () v) )
          | Get_address v     -> Address (Option.unopt (Tezos.call_view "get_address" () v))
          | Super_not_funny v -> Integer (Option.unopt (Tezos.call_view "super_not_funny" () v)))
end

type orig1 = (| Default, int) origination_result

let test_basic =
  let orig1 : orig1 = Test.originate_from_file "./views_using_view.jsligo" 999 1tez in
  let addr = Test.to_address orig1.addr in
  let orig2 = Test.originate (contract_of Proxy) (Integer 1) 1tez in
  let _ = Test.transfer orig2.addr (Main (Basic (addr, addr))) 1tez in
  let s = Test.get_storage orig2.addr in
  s = (Integer 999)

let test_not_funny =
  let orig1 : orig1 = Test.originate_from_file "./views_using_view.jsligo" 999 1tez in
  let addr = Test.to_address orig1.addr in
  let orig2 = Test.originate (contract_of Proxy) (Integer 1) 1tez in 
  let _ = Test.transfer orig2.addr (Main (Not_funny addr)) 1tez in
  let s = Test.get_storage orig2.addr in
  s = (Integer 999)

let test_get_storage =
  let orig1 : orig1 = Test.originate_from_file "./views_using_view.jsligo" 999 1tez in
  let addr = Test.to_address orig1.addr in
  let orig2 = Test.originate (contract_of Proxy) (Integer 1) 1tez in 
  let _ = Test.transfer orig2.addr (Main (Get_storage addr)) 1tez in
  let s = Test.get_storage orig2.addr in
  s = (Integer 999)

let test_get_address =
  let orig1 : orig1 = Test.originate_from_file "./views_using_view.jsligo" 999 1tez in
  let addr = Test.to_address orig1.addr in
  let orig2 = Test.originate (contract_of Proxy) (Integer 1) 1tez in 
  let _ = Test.transfer orig2.addr (Main (Get_address addr)) 1tez in
  let s = Test.get_storage orig2.addr in
  s = Address (Test.to_address orig2.addr)

let test_super_not_funny =
  let orig1 : orig1 = Test.originate_from_file "./views_using_view.jsligo" 999 1tez in
  let addr = Test.to_address orig1.addr in
  let orig2 = Test.originate (contract_of Proxy) (Integer 1) 1tez in 
  let _ = Test.transfer orig2.addr (Main (Super_not_funny addr)) 1tez in
  let s = Test.get_storage orig2.addr in
  s = (Integer (999 + 999))
