// storage type
type threshold_t is nat
type addr_set_t is set(address)
type message_store_t is map(bytes,addr_set_t)

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
  var message : message_t := param ;
  var ret_ops : list(operation) := (nil : list(operation)) ;

  if set_mem(source,s.auth) then block {
    const packed_msg : bytes = bytes_pack(message) ;
    var store_patch : addr_set_t := set_empty ;

    case map_get(packed_msg, s.message_store) of
    | Some(voters) ->
      if set_mem(source,voters) then failwith("Already accounted message")
      else store_patch := set_add(source,voters)
    | None -> store_patch := set source end
    end;

    if size(store_patch) >= s.threshold then block {
      remove packed_msg from map s.message_store ;
      ret_ops := message(unit) ;
    } else
      patch s.message_store with map [packed_msg -> store_patch]
  }
  else
    failwith("Unauthorized address");

} with ( ret_ops , s)

function main(const param : entry_point_t; const s : storage_t) : contract_return_t is 
  case param of
  | Send (p) -> send(p,s)
end