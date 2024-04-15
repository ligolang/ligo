type foo =
  { foo : nat ;
    bar : int ;
    baz : string }

module Foo = struct
  [@entry]
  let foo (_ : foo) (s : unit) : operation list * unit =
    ([], s)

  (* dummy entrypoint to avoid bug with single entrypoint :( *)
  [@entry]
  let dummy (_ : unit) (s : unit) : operation list * unit =
    ([], s)
end

module Bar = struct
  [@entry]
  let bar (addr : address) (s : unit) : operation list * unit =
    let arg : foo = {foo = 1n; bar = 2; baz = "three"} in
    let amt : tez = 0tz in
    let dst : foo contract = Tezos.get_entrypoint "%foo" addr in
    let tx = Tezos.transaction arg amt dst in
    ([tx], s)

  (* dummy entrypoint to avoid bug with single entrypoint :( *)
  [@entry]
  let dummy (_ : unit) (s : unit) : operation list * unit =
    ([], s)
end

let test_interaction () =
  let orig_foo = Test.originate (contract_of Foo) () 0tz in
  let foo_addr = Test.to_address orig_foo.addr in
  let orig_bar = Test.originate (contract_of Bar) () 0tz in
  Test.transfer_exn orig_bar.addr (Bar foo_addr) 0tz
type tree_record =
  [@layout tree]
  { foo : nat ;
    bar : int ;
    baz : string }

type tree_variant =
  [@layout tree]
  | Foo of nat
  | Bar of int
  | Baz of string

type tree_tuple =
  [@layout tree] (nat * int * string)
  (* the parentheses are required, else the @layout attribute will
     attach to the first tuple field instead of the tuple type *)

let anon_tree_tuple (p : [@layout tree] (nat * int * string)) : [@layout tree] (nat * int * string) = p