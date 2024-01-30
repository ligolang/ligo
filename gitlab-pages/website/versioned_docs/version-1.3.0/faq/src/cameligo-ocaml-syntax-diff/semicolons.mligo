(* In CameLIGO *)
type storage = int

[@entry]
let main (_p : unit) (s : storage) : operation list * storage =
  let tests =
    begin
      assert (1 = 1);
      assert (2 = 2) // no semicolon here
    end
  in
  [], s