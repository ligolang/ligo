// storage type
type threshold_t is nat
type addr_set_t is set(address)
type message_store_t is big_map(bytes,addr_set_t)

type storage_t is record
  threshold : threshold_t ;
  auth : addr_set_t ;
  message_store : message_store_t ;
end

// I/O types
type message_t is (unit -> list(operation))
type send_pt is message_t

type contract_return_t is (list(operation) * storage_t)

type entry_point_t is
| Send of send_pt

function send (const param : send_pt; const s : storage_t) : contract_return_t is block {

  if not set_mem(sender,s.auth) then failwith("Unauthorized address") else skip ;
  
  var message : message_t := param ;
  const packed_msg : bytes = bytes_pack(message) ;
  var ret_ops : list(operation) := (nil : list(operation)) ;

  var new_store : addr_set_t :=
    case map_get(packed_msg, s.message_store) of
    | Some(voters) -> set_add(sender,voters)
    | None         -> set sender end end
  ;

  if size(new_store) >= s.threshold then block {
    remove packed_msg from map s.message_store ;
    ret_ops := message(unit) ;
  } else
    s.message_store[packed_msg] := new_store

} with ( ret_ops , s)

function main(const param : entry_point_t; const s : storage_t) : contract_return_t is 
  case param of
  | Send (p) -> send(p,s)
end