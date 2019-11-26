// storage type
type threshold_t is nat
type max_proposal_t is nat
type max_message_size_t is nat
type state_hash_t is bytes
type addr_set_t is set(address)
type message_store_t is map(bytes,addr_set_t)
type proposal_counters_t is map(address,nat)

type storage_t is record
  state_hash : state_hash_t ;
  threshold : threshold_t ;
  max_proposal : max_proposal_t ;
  max_message_size : max_message_size_t ;
  authorized_addresses : addr_set_t ;
  message_store : message_store_t ;
  proposal_counters : proposal_counters_t ;
end

// I/O types
type message_t is (bytes -> list(operation))
type send_pt is message_t
type withdraw_pt is message_t
type default_pt is unit

type contract_return_t is (list(operation) * storage_t)

type entry_point_t is
| Send of send_pt
| Withdraw of withdraw_pt
| Default of default_pt


function send (const param : send_pt; const s : storage_t) : contract_return_t is block {

  // check sender against the authorized addresses
  if not set_mem(sender,s.authorized_addresses) then failwith("Unauthorized address") else skip ;

  // check message size against the stored limit
  var message : message_t := param ;
  const packed_msg : bytes = bytes_pack(message) ;
  if size(packed_msg) > s.max_message_size then failwith("Message size exceed maximum limit")
  else skip ;

  // compute the new set of addresses associated with the message and update counters
  var new_store : addr_set_t := set_empty ;
  case map_get(packed_msg, s.message_store) of
  | Some(voters) -> block { // the message is already stored
    // increment the counter only if the sender isn't already associated with the message
    if set_mem(sender,voters) then skip
    else s.proposal_counters[sender] := get_force(sender,s.proposal_counters) + 1n ;

    new_store := set_add(sender,voters)
  } 
  | None -> block { // the message has never been received before
    s.proposal_counters[sender] := get_force(sender,s.proposal_counters) + 1n ;
    new_store := set [sender];
  }
  end ;

  // check sender counters against the maximum number of proposal
  var sender_proposal_counter : nat := get_force(sender,s.proposal_counters) ;
  if sender_proposal_counter > s.max_proposal then failwith("Maximum number of proposal reached")
  else skip ;

  // check the threshold
  var ret_ops : list(operation) := (nil : list(operation)) ;
  if size(new_store) >= s.threshold then block {
    remove packed_msg from map s.message_store ;
    ret_ops := message(s.state_hash) ;
    // update the state hash
    s.state_hash := sha_256 ( bytes_concat (s.state_hash , packed_msg) ) ;
    // decrement the counters
    for addr -> ctr in map s.proposal_counters block {
      if set_mem(addr,new_store) then
        s.proposal_counters[addr] := abs (ctr - 1n)
      else skip ;
    }
  } else
    s.message_store[packed_msg] := new_store
  
} with ( ret_ops , s)

function withdraw (const param : withdraw_pt; const s : storage_t) : contract_return_t is block {

  var message : message_t := param ;
  const packed_msg : bytes = bytes_pack(message) ;

  case map_get(packed_msg, s.message_store) of
  | Some(voters) -> block { // the message is stored
    const new_set : addr_set_t = set_remove(sender,voters) ;

    // decrement the counter only if the sender was already associated with the message
    if size(voters) =/= size(new_set) then
      s.proposal_counters[sender] := abs (get_force(sender,s.proposal_counters) - 1n)
    else skip ;

    // if the message is left without any associated addresses, remove the corresponding message_store field
    if size(new_set) = 0n then remove packed_msg from map s.message_store
    else s.message_store[packed_msg] := new_set
  }
  | None -> skip end // the message isn't stored, ignore

} with ( (nil: list(operation)) , s)

function default (const p : default_pt; const s : storage_t) : contract_return_t is
  ((nil: list(operation)) , s)

function main(const param : entry_point_t; const s : storage_t) : contract_return_t is 
  case param of
  // propagate message p if the number authorized addresses having
  // voted for the same message p equals the threshold
  | Send     (p) -> send(p,s)
  // withraw vote for message p
  | Withdraw (p) -> withdraw(p,s)
  // use this entry-point to transfer tez to the contract
  | Default  (p) -> default(p,s)
end