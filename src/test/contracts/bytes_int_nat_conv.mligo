[@entry] let main (_ : unit) (_ : unit) : operation list * unit =
  let b = 0x123456 in 
  (* bytes => nat => bytes *)
  let () = assert (b = bytes(nat(b))) in
  (* bytes => int => bytes *)
  let () = assert (b = bytes(int(b))) in
  (* int => bytes => int *)
  let () = assert (1234 = int(bytes(1234))) in
  (* nat => bytes => nat *)
  let () = assert (4567n = nat(bytes(4567n))) in
  [], ()