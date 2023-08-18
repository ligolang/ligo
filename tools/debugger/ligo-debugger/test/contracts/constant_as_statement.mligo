module EURO =
  struct
    let some_const = 10
  end

let get_val () = 42

let get_verbatim () = {|100|}

let get_const () = EURO.some_const

[@entry]
let main (_, s : unit * int) : operation list * int =
  let some_int = Some (1000 - 7) in
  let res =
    match some_int with
    | Some _ -> 100
    | None -> 42
  in
  let y = 12 in
  (([] : operation list), s + res + y + get_val() + String.length(get_verbatim()) + get_const())
