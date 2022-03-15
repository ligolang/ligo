function sub (const store : tez; const delta : tez) : option (tez) is 
  store - delta

function main (const _ : unit; const store : tez) : list (operation) * tez is
  ((nil : list (operation)), Option.unopt (sub (store, 1tez)))