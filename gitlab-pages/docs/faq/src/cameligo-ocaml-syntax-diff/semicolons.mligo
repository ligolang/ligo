(* In CameLIGO *)
type storage = int

[@entry]
let main (_p : unit) (s : storage) : operation list * storage =
  let tests =
    begin
      Assert.assert (1 = 1);
      Assert.assert (2 = 2) // no semicolon here
    end
  in
  [], s