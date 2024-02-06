module X = struct
  type t = Foo | Bar of int

  module Y = struct
    type t = Baz
  end
end

type t = Foo

let _ =
  let _ = (Foo : t) in
  let _ = (Foo : X.t) in
  let _ = (Baz : X.Y.t) in
  let _ =
    match (Aaa : Aaa | Bbb) with
    | Aaa -> 42
    | Bbb -> 24
  in
  _
