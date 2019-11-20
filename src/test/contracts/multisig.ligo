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
type check_message_pt is record
  counter : counter_t ;
  message : (unit -> list(operation)) ;
  signatures : list(signature) ;
end

type contract_return_t is (list(operation) * storage_t)

type entry_point_t is
| CheckMessage of check_message_pt

function check_message (const param : check_message_pt;
                        const s : storage_t) : contract_return_t is block {
  var message : (unit -> list(operation)) := param.message ;

  if param.counter =/= s.counter then
    failwith ("Counters does not match")
  else block {
    const packed_payload : bytes =
      bytes_pack((message , param.counter , s.id , get_chain_id));
    var valid : nat := 0n ;

    for sig in list param.signatures block {
      var is_valid : bool := False ;

      for pk in list s.auth block {
        if crypto_check(pk,sig,packed_payload) then is_valid := True
        else skip;
      };

      if is_valid then valid := valid + 1n
      else failwith ("Invalid signature")
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