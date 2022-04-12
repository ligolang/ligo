module TestProperty = struct
  let gen (type a) (p : a -> bool) : nat -> a option =
    fun (n : nat) ->
      let rec f (n : nat) : a option =
        if (n = 0n) then None
        else let x = (Test.random () : a) in
             if (p x) then
               f (abs (n - 1))
             else
               Some x in
      f n

  let run (type a) (r : nat -> a option) : nat -> unit =
    fun (n : nat) ->
      match r n with
      | None -> ()
      | Some x -> assert false
end

(* This function is almost like identity... *)
let almost_id (xs : int list) =
  if (List.length xs = 5n) then [] else xs

(* Let's check if it really is *)
let test =
  (* We generate the property *)
  let g = TestProperty.gen (fun (xs : int list) -> almost_id xs = xs) in
  (* And run it *)
  TestProperty.run g 100n
