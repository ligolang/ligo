type state is
  record
    goal     : nat;
    deadline : timestamp;
    backers  : map (address, nat);
    funded   : bool
  end

entrypoint contribute (storage store : state;
                       const sender  : address;
                       const amount  : mutez)
  : state * list (operation) is
  var operations : list (operation) := []
  begin
    if now > store.deadline then
      fail "Deadline passed"
    else
      match store.backers[sender] with
        None ->
          store :=
            copy store with
              record
                backers = add_binding ((sender, amount), store.backers)
              end
      | _ -> skip
      end
  end with (store, operations)

entrypoint withdraw (storage store : state; const sender : address)
  : state * list (operation) is
  var operations : list (operation) := []
  begin
    if sender = owner then
      if now >= store.deadline then
        if balance >= store.goal then
          begin
            store := copy store with record funded = True end;
            operations := [Transfer (owner, balance)]
          end
        else fail "Below target"
      else fail "Too soon"
    else skip
  end with (store, operations)

entrypoint claim (storage store : state; const sender : address)
  : state * list (operation) is
  var operations : list (operation) := []
  var amount : mutez := 0
  begin
    if now <= store.deadline then
      fail "Too soon"
    else
      match store.backers[sender] with
        None ->
          fail "Not a backer"
      | Some (amount) ->
          if balance >= store.goal || store.funded then
            fail "Cannot refund"
          else
            begin
              amount := store.backers[sender];
              store :=
                copy store with
                  record
                    backers = remove_entry (sender, store.backers)
                  end;
              operations := [Transfer (sender, amount)]
            end
      end
  end with (store, operations)
