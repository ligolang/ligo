let main = (p: key_hash) : list(operation) => {
  let unused: operation = (Operation.set_delegate(Some(p)));
  ([]: list(operation));
} ;
