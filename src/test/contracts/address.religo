let main = (p : key_hash) : address => {
  let c : contract(unit) = Current.implicit_account(p) ;
  Current.address(c) ;
};
