type parameter is unit
type storage is timestamp
type return is list (operation) * storage

function main (const p : parameter; const s : storage) : return is
  {
    var stamp : timestamp := ("badtimestamp" : timestamp)
  }
  with ((nil : list (operation)), stamp)
