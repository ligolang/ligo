let main = (p: key_hash) : list (operation) => {
  let unused : operation = (Tezos.set_delegate (Some (p)));
  ([] : list (operation));
} ;
