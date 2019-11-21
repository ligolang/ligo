// storage type
type counter_t is nat
type threshold_t is nat
type authorized_keys_t is list(key)
type id_t is string

type storage_t is record
  id : id_t ;
  counter : counter_t ;
  threshold : threshold_t ;
  auth : authorized_keys_t ;
end

// I/O types
type message_t is (unit -> list(operation))
type signatures_t is list(key_hash * signature)
type check_message_pt is record
  counter : counter_t ;
  message : message_t ;
  signatures : signatures_t ;
end

type contract_return_t is (list(operation) * storage_t)

type entry_point_t is
| CheckMessage of check_message_pt

function check_message (const param : check_message_pt;
                        const s : storage_t) : contract_return_t is block {
  var message : message_t := param.message ;

  if param.counter =/= s.counter then
    failwith ("Counters does not match")
  else block {
    const packed_payload : bytes =
      bytes_pack((message , param.counter , s.id , get_chain_id));
    var valid : nat := 0n ;

    var keys : authorized_keys_t := s.auth ;
    for pkh_sig in list param.signatures block {
      case keys of
      | nil -> skip
      | key # tl -> block {
        keys := tl ;
        if pkh_sig.0 = crypto_hash_key(key) then
          if crypto_check(key,pkh_sig.1,packed_payload) then valid := valid + 1n ;
          else failwith ("Invalid signature")
        else skip;
      }
      end
    };

    if valid < s.threshold then
      failwith ("Not enough signatures passed the check")
    else s.counter := s.counter + 1n ;
  }
} with (message(unit), s)

function main(const param : entry_point_t; const s : storage_t) : contract_return_t is 
  case param of
  | CheckMessage (p) -> check_message(p,s)
end