module C = struct
  type storage = {
    participants : address set ;
    secrets : (address, bool) map ;
    metadata: (string,bytes) big_map
  }
  type parameter = unit
  type return = operation list * storage

  [@entry]
  let main (() : parameter) (s : storage) : operation list * storage =
      let committed = fun (acc, elt : bool * address) : bool -> match Map.find_opt elt s.secrets with
        | None -> acc && false
        | Some _x -> acc && true
      in
      let _foo = Set.fold committed s.participants true in
      // let _foo = Set.fold committed s.participants true in
      let revealed = fun (acc, elt : bool * address) : bool -> match Map.find_opt elt (Map.empty : (address, bool) map) with
          | None -> acc && false
          | Some _x -> acc && true
      in
      let _foo = Set.fold revealed s.participants true in
      [], s

  let init_storage : storage = {
    participants = (Set.empty : address set) ;
    secrets = (Map.empty : (address, bool) map) ;
    metadata = (Big_map.empty : (string,bytes) big_map) ;
  }
end

let test_x = Test.originate (contract_of C) C.init_storage 0mutez
