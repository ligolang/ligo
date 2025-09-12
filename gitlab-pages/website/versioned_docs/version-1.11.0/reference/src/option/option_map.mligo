
let v : int option = Some 1

let foo (_ : int) : string = "foo"

let foo_option : string option = Option.map foo v (* Some "foo" *)

let none : string option = Option.map foo (None : int option) (* None *)
