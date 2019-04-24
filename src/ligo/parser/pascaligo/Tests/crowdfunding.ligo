type store is
  record [
    goal     : nat;
    deadline : timestamp;
    backers  : map (address, nat);
    funded   : bool;
  ]

entrypoint contribute (storage store : store;
                       const sender  : address;
                       const amount  : mutez)
  : store * list (operation) is
  var operations : list (operation) := nil
     //  const s : list (int) = list [1; 2; 3]
//const t : set (int) = set []
  begin
    if now > store.deadline then
      fail "Deadline passed";
    else
      case store.backers[sender] of [
        None -> store.backers[sender] := Some (amount);
//        None -> patch store.backers with map sender -> amount end
      |    _ -> skip
      ]
  end with (store, operations)

entrypoint withdraw (storage store : store; const sender : address)
  : store * list (operation) is
  var operations : list (operation) := list end
  begin
//    if set ["a"; "b"] contains x then skip else skip;
    if sender = owner then
      if now (Unit) >= store.deadline then
        if balance >= store.goal then {
             store.funded := True;
//           patch store with record funded = True end;
             operations := list [Transfer (owner, balance)];
        };
        else fail "Below target"
      else block { fail "Too soon"; }
    else skip
  end with (store, operations)

entrypoint claim (storage store : store; const sender : address)
  : store * list (operation) is
  var operations : list (operation) := list []
  var amount : mutez := 0
  begin
    if now <= store.deadline then
      fail "Too soon"
    else
      case store.backers[sender] of
        None ->
          fail "Not a backer"
      | Some (amount) ->
          if balance >= store.goal or store.funded then
            fail "Cannot refund"
          else
            begin
              operations := list [Transfer (sender, amount)];
              remove sender from map store.backers
            end
      end
  end with (store, operations)
