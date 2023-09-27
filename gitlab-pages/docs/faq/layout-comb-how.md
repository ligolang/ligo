---
id: layout-comb-how
title: How to deal with the change of the default datatype layout to `@layout comb` ?
---

See [Why did the default datatype layout change to `@layout comb` ?](layout-comb-why.md)
for background about this change.

## Easy way out

To take the easy way out, set the `LIGO_LEGACY_LAYOUT_TREE`
environment variable:

```shell
export LIGO_LEGACY_LAYOUT_TREE=
```

This will cause LIGO to undo the change, letting `@layout tree` remain
the default.

This is only a temporary solution, intended to let people easily
migrate to LIGO 1.0, which should be considered already deprecated
as it will eventually be removed.

## What's the problem?

If you do not take the easy way out, it's important to understand the
problem.

In theory, it is possible that after upgrading to LIGO 1.0, your
contracts could compile successfully, your `ligo run test` could pass,
and even "integration" tests (e.g. using a sandbox or test Tezos
network) could pass, yet after deployment you might still find that
your contracts are catastrophically broken.

This can happen if your contracts need to interact with other
contracts, which are either:

- already deployed on mainnet, or
- compiled using an older version of LIGO, or
- compiled using a non-LIGO compiler, or
- implementing some standardized interface

If the compiled interface types (parameter, view, ...) for these
interactions change when you upgrade to LIGO 1.0, because you
previously (implicitly) used `@layout tree` and now the types use
`@layout comb`, this will break compatibility, and the interactions
will fail.

For example, consider the following test, involving communication
between two contracts over a parameter type `foo`:

<Syntax syntax="cameligo">

```cameligo
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
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo
type foo = {
  foo : nat,
  bar : int,
  baz : string
}

namespace Foo {
  @entry
  const foo = (_: foo, s: unit) : [list<operation>, unit] => [list([]), s];

  // dummy entrypoint to avoid bug with single entrypoint :(
  @entry
  const dummy = (_: unit, s: unit) : [list<operation>, unit] => [list([]), s];
};

namespace Bar {
  @entry
  const bar = (addr: address, s: unit) : [list<operation>, unit] => {
    const arg : foo = {foo: 1n, bar: 2, baz: "three"};
    const amt : tez = 0tz;
    const dst : contract<foo> = Tezos.get_entrypoint("%foo", addr);
    const tx = Tezos.transaction(arg, amt, dst);
    return [list([tx]), s];
  }

  // dummy entrypoint to avoid bug with single entrypoint :(
  @entry
  const dummy = (_: unit, s: unit) : [list<operation>, unit] => [list([]), s];
};

const test_interaction = do{
  const orig_foo = Test.originate(contract_of(Foo), unit, 0tz);
  const foo_addr = Test.to_address (orig_foo.addr);
  const orig_bar = Test.originate(contract_of(Bar), unit, 0tz);
  Test.transfer_exn(orig_bar.addr, Bar(foo_addr), 0tz);
};
```

</Syntax>

Note that this test will pass after upgrading to LIGO 1.0, because
when running the test, both `Foo` and `Bar` use the `comb` layout for
the parameter type `foo`. However, if there is already a `Foo`
contract deployed on mainnet, e.g. compiled using an older LIGO
version, then a newly compiled `Bar` contract will be unable to
communicate with it, because the new `foo` type will be incompatible!

So, in order to test that your contracts still work correctly, you
must either manually check that the interface types have changed, or
test interactions with the actual contracts you will need to interact
with.

In particular, if you are testing compatibility for interactions, you
should NOT only test interactions against contracts compiled using
LIGO 1.0, e.g. using `ligo test`, because those contracts will use the
new layout too!

If, on the other hand, you will deploy a _new_ set of contracts which
only interact amongst themselves, then you have nothing to worry
about. It is only interactions with pre-existing or standardized
contracts that can cause trouble.

## How to deal with compatibility problems?

If you have compatibility problems like this, and you don't take the
easy way out with `LIGO_LEGACY_LAYOUT_TREE`, you might need to switch
some types back to the tree layout.

Here are some examples of how to do that:

<Syntax syntax="cameligo">

```cameligo
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
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo
type tree_variant =
  @layout("tree")
  | ["Foo", nat]
  | ["Bar", int]
  | ["Baz", string];

type tree_record =
  @layout("tree")
  {
    foo : int,
    bar : int
  };

// in JsLIGO, you must assign names to tuple types in order to put
// them back into tree layout, and then use these names at use sites instead
// of repeating the tuple type

type tree_tuple = @layout("tree") [nat, int, string];
```

</Syntax>

## Type errors from layouts

In some cases, you may get type errors due to mismatched type layouts.

Here is an example. Before LIGO 1.0, this code worked OK:

<Syntax syntax="cameligo">

```cameligo skip
type record1 =
  { foo : nat ;
    bar : int ;
    baz : string }

type record2 =
  { baz : string ;
    bar : int ;
    foo : nat }

let id (r : record1) : record2 = r
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
type record1 = {
  foo : nat,
  bar : int,
  baz : string
}

type record2 = {
  baz : string,
  bar : int,
  foo : nat
}

const id = (r: record1): record2 => r;
```

</Syntax>

However, as of LIGO 1.0, this example gives a type error:

```
Invalid type(s)
Cannot unify "record1" with "record2" due to differing layouts "({ name: foo }, { name: bar }, { name: baz })" and "({ name: baz }, { name: bar }, { name: foo })"
```

The reason for this is that in the old `tree` layout, the fields were sorted alphabetically, but in the `comb` layout they are taken in the declared order.

You can fix this problem either by switching back to the `tree` layout (see the previous section) or by writing the fields in a consistent order.

<!-- updated use of entry -->