type store is
  record [
    goal     : mutez;
    deadline : timestamp;
    backers  : map (address, nat);
    funded   : bool;
  ]

function back (var store : store) : list (operation) * store is
  var operations : list (operation) := list []
  begin
    if now > store.deadline then
      failwith ("Deadline passed");
    else
      case store.backers[sender] of [
        None -> store.backers[sender] := amount
 // or: None -> patch store.backers with map sender -> amount end
      |    _ -> skip
      ]
  end with (operations, store)

function claim (var store : store) : list (operation) * store is
  var operations : list (operation) := nil
  begin
    if now <= store.deadline then
      failwith ("Too soon.")
    else
      case store.backers[sender] of
        None ->
          failwith ("Not a backer.")
      | Some (amount) ->
          if balance >= store.goal or store.funded then
            failwith ("Goal reached: no refund.")
          else
            begin
              operations := list [transaction (unit, sender, amount)];
              remove sender from map store.backers
            end
      end
  end with (operations, store)

function withdraw (var store : store) : list (operation) * store is
  var operations : list (operation) := list end
  begin
    if sender = owner then
      if now >= store.deadline then
        if balance >= store.goal then {
             store.funded := True;
 // or:      patch store with record funded = True end;
             operations := list [Transfer (owner, balance)];
        };
        else failwith ("Below target.")
      else fail "Too soon.";
    else skip
  end with (operations, store)
