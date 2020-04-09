let y (_ : unit) : nat =
  let x : nat = 1n in
  begin
    (let x : nat = 2n in unit) ;
    (let x : nat = 23n in unit) ;
    (let x : nat = 42n in unit) ;
    x
  end
