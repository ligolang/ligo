module A = struct
  type parameter1 =
    {
     [@annot AAA] fooA : nat;
     fooB : nat;
     [@annot cCC] fooC : nat
    }

  [@entry]
  let main (_ : parameter1) (_ : unit) : operation list * unit =
    (([] : operation list), ())

end

module B = struct
  type parameter2 =
  | [@annot AAA] FooA of nat
  | FooB of nat
  | [@annot cCC] FooC of nat

  [@entry]
  let main (_ : parameter2) (_ : unit) : operation list * unit =
    (([] : operation list), ())

end
