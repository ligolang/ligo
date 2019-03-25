type store is
  record
    goal     : nat;
    deadline : timestamp;
    backers  : map (address, nat);
    funded   : bool;
  end

entrypoint contribute (storage store : store;
                       const sender  : address;
                       const amount  : mutez)
  : store * list (operation) is
  var operations : list (operation) := []
  block {
    if now > store.deadline then
      fail "Deadline passed";
    else
      case store.backers[sender] of
        None -> store.backers[sender] := Some (amount)
//        None -> patch store.backers with map sender -> amount end
      |    _ -> skip
      end
  } with (store, operations)

entrypoint withdraw (storage store : store; const sender : address)
  : store * list (operation) is
  var operations : list (operation) := []
  begin
    if sender = owner then
      if now (Unit) >= store.deadline then
        if balance >= store.goal then {
             store.funded := True;
//           patch store with record funded = True end;
             operations := [Transfer (owner, balance)];
        };
        else fail "Below target"
      else { fail "Too soon"; }
    else skip
  end with (store, operations)

entrypoint claim (storage store : store; const sender : address)
  : store * list (operation) is
  var operations : list (operation) := []
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
              operations := [Transfer (sender, amount)];
              remove sender from map store.backers
            end
      end
  end with (store, operations)
