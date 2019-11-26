// storage type
type threshold_t is nat
type max_proposal_t is nat
type max_message_size_t is nat
type addr_set_t is set(address)
type message_store_t is map(bytes,addr_set_t)
type counter_store_t is map(address,nat)

type storage_t is record
  threshold : threshold_t ;
  max_proposal : max_proposal_t ;
  max_message_size : max_message_size_t ;
  auth : addr_set_t ;
  message_store : message_store_t ;
  counter_store : counter_store_t ;
end

// I/O types
type message_t is (unit -> list(operation))
type send_pt is message_t
type withdraw_pt is message_t

type contract_return_t is (list(operation) * storage_t)

type entry_point_t is
| Send of send_pt
| Withdraw of withdraw_pt

function send (const param : send_pt; const s : storage_t) : contract_return_t is block {

  if not set_mem(sender,s.auth) then failwith("Unauthorized address") else skip ;
  
  var message : message_t := param ;
  var new_store : addr_set_t := set_empty ;
  var ret_ops : list(operation) := (nil : list(operation)) ;

  const packed_msg : bytes = bytes_pack(message) ;
  if size(packed_msg) > s.max_message_size then failwith("Message size exceed maximum limit")
  else skip ;

  case map_get(packed_msg, s.message_store) of
  | Some(voters) -> block {
    if set_mem(sender,voters) then skip
    else s.counter_store[sender] := get_force(sender,s.counter_store) + 1n ;
    new_store := set_add(sender,voters)
  }
  | None -> block {
    s.counter_store[sender] := get_force(sender,s.counter_store) + 1n ;
    new_store := set [sender];
  }
  end ;

  var sender_proposal_counter : nat := get_force(sender,s.counter_store) ;
  if sender_proposal_counter > s.max_proposal then failwith("Maximum number of proposal reached")
  else skip ;

  if size(new_store) >= s.threshold then block {
    remove packed_msg from map s.message_store ;
    ret_ops := message(unit) ;
    s.counter_store[sender] := abs (sender_proposal_counter - 1n) ;
  } else
    s.message_store[packed_msg] := new_store
  
} with ( ret_ops , s)

function withdraw (const param : withdraw_pt; const s : storage_t) : contract_return_t is block {

  var message : message_t := param ;
  const packed_msg : bytes = bytes_pack(message) ;

  case map_get(packed_msg, s.message_store) of
  | Some(voters) -> block {
    const new_set : addr_set_t = set_remove(sender,voters) ;

    if size(voters) =/= size(new_set) then
      s.counter_store[sender] := abs (get_force(sender,s.counter_store) - 1n)
    else skip ;

    if size(new_set) = 0n then remove packed_msg from map s.message_store
    else s.message_store[packed_msg] := new_set
  }
  | None -> skip end

} with ( (nil: list(operation)) , s)

function main(const param : entry_point_t; const s : storage_t) : contract_return_t is 
  case param of
  | Send (p) -> send(p,s)
  | Withdraw (p) -> withdraw(p,s)
end