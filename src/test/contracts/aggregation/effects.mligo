let a = 42

[@entry]
let main (_p : int) (s : int) : operation list * int =
  let xx =
    let _w =
      (let () = failwith "foo" in
       fun (y : int) -> y + a) in
    (failwith "bar" : int -> int) in
  ([] : operation list), (xx s)
