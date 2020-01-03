type storage_ is timestamp

function main(const p : unit; const s : storage_) : list(operation) * storage_ is
  block {
    var toto : timestamp := ("badtimestamp" : timestamp);
  }
  with ((nil: list(operation)), toto)