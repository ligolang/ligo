[@private] let proxy_transfer_contract (type vt whole_p)
    ( mk_param : vt ticket -> whole_p)
    ( (p,_)    : ((vt * nat) * address) * unit )
    : operation list * unit =
  let ((v,amt),dst_addr) = p in
  let ticket = Option.unopt (Tezos.create_ticket v amt) in
  let tx_param = mk_param ticket in
  let c : whole_p contract = Tezos.get_contract_with_error dst_addr "Testing proxy: you provided a wrong address" in
  let op = Tezos.transaction tx_param 1mutez c in
  [op], ()

[@private] let proxy_originate_contract (type vt whole_s vp)
    ( mk_storage : vt ticket -> whole_s)
    ( main       : vp * whole_s -> operation list * whole_s)
    ( (p,_)      : (vt * nat) * address option )
    : operation list * address option =
  let (v,amt) = p in
  let ticket = Option.unopt (Tezos.create_ticket v amt) in
  let init_storage : whole_s = mk_storage ticket in
  let op,addr = Tezos.create_contract_uncurried main (None: key_hash option) 0mutez init_storage in
  [op], Some addr



type 'v proxy_address = (('v * nat) * address , unit) typed_address

let init_transfer (type vt whole_p) (mk_param: vt ticket -> whole_p) : vt proxy_address =
  let proxy_transfer : ((vt * nat) * address) * unit -> operation list * unit =
    proxy_transfer_contract mk_param
  in
  let (taddr_proxy, _, _) = Test.originate_uncurried proxy_transfer () 1tez in
  taddr_proxy

let transfer (type vt)
    (taddr_proxy : vt proxy_address)
    (info        : (vt * nat) * address) : test_exec_result = 
  let ticket_info, dst_addr = info in
  Test.transfer_to_contract (Test.to_contract taddr_proxy) (ticket_info , dst_addr) 1mutez

let originate (type vt whole_s vp)
    (ticket_info : vt * nat)
    (mk_storage : vt ticket -> whole_s)
    (contract: vp * whole_s -> operation list * whole_s) : address =
  let proxy_origination : (vt * nat) * address option -> operation list * address option =
    proxy_originate_contract mk_storage contract
  in
  let (taddr_proxy, _, _) = Test.originate_uncurried proxy_origination (None : address option) 1tez in
  let _ = Test.transfer_to_contract_exn (Test.to_contract taddr_proxy) ticket_info 0tez in
  match Test.get_storage taddr_proxy with
  | Some addr ->
    let _taddr = (Test.cast_address addr : (vp,whole_s) typed_address) in
    addr
  | None -> failwith "internal error"