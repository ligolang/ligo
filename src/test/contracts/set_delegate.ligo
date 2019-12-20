function main (const p: key_hash) : list(operation) is
  begin
    const unused: operation = set_delegate(Some(p)) ;
    const dummy: list(operation) = nil;
  end with dummy

