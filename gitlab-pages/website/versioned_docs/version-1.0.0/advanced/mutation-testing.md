---
id: mutation-testing
title: Mutation testing
---

import Syntax from '@theme/Syntax';
import Link from '@docusaurus/Link';

We assume that the reader is familiar with LIGO's testing framework. A
reference can be found [here](testing.md).

## A simple testing example

To demonstrate how to use the mutation primitives in the testing
framework, we will have a look at a basic function that we would like
to test. Suppose we want to construct a function that takes an integer
argument and doubles it, tentatively the following one:

<Syntax syntax="cameligo">

```cameligo test-ligo group=twice
let twice (x : int) = x + x
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo test-ligo group=twice
const twice = (x: int) => x + x;
```

</Syntax>

Assume that we want to make sure that this function works as expected,
because it will be used as part of a major development. We could write
the following tests:

<Syntax syntax="cameligo">

```cameligo test-ligo group=twice
let simple_tests (f : int -> int) =
  (* Test 1 *)
  let () = assert (Test.michelson_equal (Test.run f 0) (Test.eval 0)) in
  (* Test 2 *)
  let () = assert (Test.michelson_equal (Test.run f 2) (Test.eval 4))
  in ()

let test = simple_tests twice
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo test-ligo group=twice
const simple_tests = (f : ((input: int) => int)) : unit => {
  /* Test 1 */
  assert (Test.michelson_equal(Test.run(f, 0), Test.eval(0)));
  /* Test 2 */
  assert (Test.michelson_equal(Test.run(f, 2), Test.eval(4)));
};

const test = simple_tests(twice);
```

</Syntax>

These tests check that `twice`:

- when run on input `0`, it returns `0`.
- when run on input `2`, it returns `4`.

The function implemented (`twice`) above passes the tests:

<Syntax syntax="cameligo">

```shell
ligo run test gitlab-pages/docs/advanced/src/mutation.mligo
# Outputs:
# Everything at the top-level was executed.
# - test exited with value ().
```

</Syntax>

<Syntax syntax="jsligo">

```shell
ligo run test gitlab-pages/docs/advanced/src/mutation.jsligo
# Outputs:
# Everything at the top-level was executed.
# - test exited with value ().
```

</Syntax>

The implementation is, in fact, correct. However, it is easy to
make a mistake and write the following implementation instead:

<Syntax syntax="cameligo">

```cameligo test-ligo group=frontpage
let twice (x : int) = x * x
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo test-ligo group=frontpage
const twice = (x: int): int => x * x;
```

</Syntax>

And, in fact, when we run `simple_tests` on this faulty
implementation, we will see that it also passes the tests.

This is because `0 * 0 = 0 + 0 = 0` and `2 * 2 = 2 + 2 = 4`. What
lessons can we draw from this?

The function was tested, but nothing guaranteed that
the tests are complete enough.

Mutation testing tries to help in this area by modifying functions
while keeping the same tests fixed, and alerting if some of the
modified functions pass all of the tests: in that situation, the tests
were not good enough to separate a good implementation from the
(possibly) incorrect ones.

We can see now how to do mutation testing in LIGO for the original
implementation for `twice` (`x + x`). The primitive from the testing
framework that we will use is

<Syntax syntax="cameligo">

```cameligo skip
val Test.mutation_test : 'a -> ('a -> 'b) -> ('b * mutation) option
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
val Test.mutation_test : (value: 'a, tester: ('a -> 'b)) => option <['b, mutation]>
```

</Syntax>

which takes a value to mutate and and a function to apply to altered
versions of that value (testing function). As soon as the function
correctly terminates (i.e. does not fail) in some value mutation,
`Test.mutation_test` will stop and return the result of the function
application, together with a `mutation` describing the change in the
value. If all of the mutations tested fail, then `Test.mutation_test`
will return `None`.

Typically, the values to mutate are functions (i.e. `'a` will be
a function type), and these functions' return type (i.e. `'b`) will be
`unit`.

For the example above, the function that will be applied is `simple_tests`,
and the value to mutate is `twice`:

<Syntax syntax="cameligo">

```cameligo test-ligo group=twice
let test_mutation =
  match Test.mutation_test twice simple_tests with
    None -> ()
  | Some (_, mutation) ->
      let () = Test.log mutation in
      Test.println "Some mutation also passes the tests! ^^"
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo test-ligo group=twice
const test_mutation =
  match(Test.mutation_test(twice, simple_tests)) {
    when(None()): unit;
    when(Some(pmutation)): do {
      Test.log(pmutation[1]);
      Test.println("Some mutation also passes the tests! ^^")
    }
  };
```

</Syntax>

Running the tests again, the following output is obtained:

<Syntax syntax="cameligo">

```shell
ligo run test gitlab-pages/docs/advanced/src/mutation.mligo
# Outputs:
# Mutation at: File "gitlab-pages/docs/advanced/src/mutation.mligo", line 1, characters 22-27:
#   1 | let twice (x : int) = x + x
#   2 |
#
# Replacing by: MUL(x ,
# x).
# File "gitlab-pages/docs/advanced/src/mutation.mligo", line 17, character 26 to line 18, character 76:
#  16 |     None -> ()
#  17 |   | Some (_, mutation) -> let () = Test.log(mutation) in
#  18 |                           failwith "Some mutation also passes the tests! ^^"
#
# Test failed with "Some mutation also passes the tests! ^^"
```

</Syntax>

<Syntax syntax="jsligo">

```shell
ligo run test gitlab-pages/docs/advanced/src/mutation.jsligo
# Outputs:
# Mutation at: File "gitlab-pages/docs/advanced/src/mutation.jsligo", line 1, characters 31-36:
#   1 | let twice = (x : int) : int => x + x;
#   2 |
#
# Replacing by: MUL(x ,
# x).
# File "gitlab-pages/docs/advanced/src/mutation.jsligo", line 18, characters 25-77:
#  17 |     Some: pmutation => { Test.log(pmutation[1]);
#  18 |                          failwith ("Some mutation also passes the tests! ^^") }
#  19 |   });
#
# Test failed with "Some mutation also passes the tests! ^^"
```

</Syntax>

The primitive `Test.mutation_test` tries out various mutations on
`twice`, and sees if they pass all of the tests. In this scenario, it
was discovered that the mutation `MUL(x,x)` also passes the tests:
this is the precise case we discussed earlier, when the incorrect
implementation `x * x` would not be detected by the tests. We need to
update the test suite. In this case, we could propose to add a new
test:

<Syntax syntax="cameligo">

```cameligo skip
let simple_tests (f : int -> int) =
  (* Test 1 *)
  let () = assert (Test.michelson_equal (Test.run f 0) (Test.eval 0)) in
  (* Test 2 *)
  let () = assert (Test.michelson_equal (Test.run f 2) (Test.eval 4)) in
  (* Test 3 *)
  let () = assert (Test.michelson_equal (Test.run f 1) (Test.eval 2))
  in ()
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
const simple_tests = (f : ((input: int) => int)) : unit => {
  /* Test 1 */
  assert (Test.michelson_equal(Test.run(f, 0), Test.eval(0)));
  /* Test 2 */
  assert (Test.michelson_equal(Test.run(f, 2), Test.eval(4)));
  /* Test 3 */
  assert (Test.michelson_equal(Test.run(f, 1), Test.eval(2)));
};
```

</Syntax>

this verifies that when input `1` is given, output `2` is returned.
Running the mutation testing again after this adjustment, no mutation
(among those tried) will pass the tests, giving extra confidence in
the tests proposed:

<Syntax syntax="cameligo">

```shell
ligo run test gitlab-pages/docs/advanced/src/mutation.mligo
# Outputs:
# Everything at the top-level was executed.
# - test exited with value ().
# - test_mutation exited with value ().
```

</Syntax>

<Syntax syntax="jsligo">

```shell
ligo run test gitlab-pages/docs/advanced/src/mutation.jsligo
# Outputs:
# Everything at the top-level was executed.
# - test exited with value ().
# - test_mutation exited with value ().
```

</Syntax>

## Mutating a contract

The following is an example on how to mutate a contract. For that, we
will use a variation of the canonical LIGO contract with only two
entrypoints `Add` and `Sub`:

<Syntax syntax="cameligo">

```cameligo test-ligo group=frontpage
// This is mutation-contract.mligo
module C = struct
  type storage = int

  // Two entrypoints
  [@entry] let add (delta : int) (store : storage) : operation list * storage = [],store + delta
  [@entry] let sub (delta : int) (store : storage) : operation list * storage = [],store - delta
end
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo test-ligo group=frontpage
// This is mutation-contract.jsligo
namespace C {
  type storage = int;

  // Two entrypoints
  @entry const add = (delta: int, store: storage): [list<operation>, storage] => [list([]),store + delta];
  @entry const sub = (delta: int, store: storage): [list<operation>, storage] => [list([]),store - delta];
}
```

</Syntax>

Doing mutation testing on a contract with multiple entrypoints can
help in finding out entrypoints that are not covered by the tests.

Consider the following test, which deploys a contract passed as
an argument, and then tests that
the entrypoint `Add(7)` works as intended on an initial storage
`5`:

<Syntax syntax="cameligo">

```cameligo test-ligo group=mutation-contract-test
(* This is mutation-contract-test.mligo *)

#import "gitlab-pages/docs/advanced/src/mutation-contract.mligo" "MutationContract"

type storage = int
type param = MutationContract parameter_of
let initial_storage = 7

let tester (taddr : (param, storage) typed_address) (_: (param ,storage) michelson_contract) (_:int) : unit =
  let _ = Test.transfer_exn taddr (Add 7) 1mutez in
  assert (Test.get_storage taddr = initial_storage + 7)

let test_original =
  let orig = Test.originate (contract_of MutationContract) initial_storage 0tez in
  tester orig.addr
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo test-ligo group=mutation-contract-test
// This is mutation-contract-test.jsligo

#import "gitlab-pages/docs/advanced/src/mutation-contract.jsligo" "MutationContract"
type storage = int;
type param = parameter_of MutationContract;
const initial_storage = 7;

const tester = (taddr : typed_address<param, storage>, _c : michelson_contract<param, storage> , _ : int) : unit => {
  let _xfer = Test.transfer_exn(taddr, Add(7), 1mutez);
  assert(Test.get_storage(taddr) == initial_storage + 7);
}

const test_original = (() => {
  let orig = Test.originate(contract_of(MutationContract), initial_storage, 0tez);
  return tester(orig.addr);
})();
```

</Syntax>

For performing mutation testing as before, we write the following test:

<Syntax syntax="cameligo">

```cameligo test-ligo group=mutation-contract-test
let test_mutation =
  match Test.originate_module_and_mutate (contract_of MutationContract) initial_storage 0tez tester with
    None -> ()
  | Some (_, mutation) ->
    let () = Test.log(mutation) in
    (* In a real program, one would write `failwith "A mutation passes"`
       Since we want to demonstrate the issue without an actual error
       a milder println is used in this document. *)
    Test.println "A mutation of the contract still passes the tests!"
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo test-ligo group=mutation-contract-test
const test_mutation =
  match(Test.originate_module_and_mutate(contract_of(MutationContract), initial_storage, 0tez, tester)) {
    when(None()): unit;
    when(Some(pmutation)): do {
      let _l = Test.log(pmutation[1]);
      // In a real program, one would write `failwith "A mutation passes"`
      // Since we want to demonstrate the issue without an actual error
      // a milder println is used in this document.
      let _p = Test.println("A mutation of the contract still passes the tests!");
    }
  };
```

</Syntax>

Running this test, the following output is obtained:

<Syntax syntax="cameligo">

```shell
ligo run test gitlab-pages/docs/advanced/src/mutation-contract-test.mligo
# Outputs:
# File "gitlab-pages/docs/advanced/src/mutation-contract-test.mligo", line 25, characters 4-65:
#  24 |     let () = Test.log(mutation) in
#  25 |     failwith "A mutation of the contract still passes the tests!"
#  26 | 
# 
# An uncaught error occured:
# Failwith: "A mutation of the contract still passes the tests!"
# Trace:
# File "gitlab-pages/docs/advanced/src/mutation-contract-test.mligo", line 25, characters 4-65
# Mutation at: File "gitlab-pages/docs/advanced/src/mutation-contract.mligo", line 8, characters 64-77:
#   7 | [@entry] let add (delta : int) (store : storage) : result = [], store + delta
#   8 | [@entry] let sub (delta : int) (store : storage) : result = [], store - delta
# 
# Replacing by: store + delta.
```

</Syntax>

<Syntax syntax="jsligo">

```shell
ligo run test gitlab-pages/docs/advanced/src/mutation-contract-test.jsligo
# Outputs:
# File "gitlab-pages/docs/advanced/src/mutation-contract-test.jsligo", line 27, characters 6-68:
#  26 |       Test.log(pmutation[1]);
#  27 |       failwith("A mutation of the contract still passes the tests!");
#  28 |     }
# 
# An uncaught error occured:
# Failwith: "A mutation of the contract still passes the tests!"
# Trace:
# File "gitlab-pages/docs/advanced/src/mutation-contract-test.jsligo", line 27, characters 6-68
# Mutation at: File "gitlab-pages/docs/advanced/src/mutation-contract.jsligo", line 8, characters 73-86:
#   7 | @entry const add = (delta : int, store : storage) : result => [list([]), store + delta];
#   8 | @entry const sub = (delta : int, store : storage) : result => [list([]), store - delta];
# 
# Replacing by: store + delta.
```

</Syntax>

The mutation testing found that the operation `sub` (corresponding to
the entrypoint `Sub`) can be changed with no consequences in the
test: we take this as a warning signalling that the test above does not
cover the `Sub` entrypoint. We can fix this by adding a new call
to the `Sub` entrypoint in the test above:

<Syntax syntax="cameligo">

```cameligo test-ligo group=mutation-contract-test
let tester_add_and_sub (taddr : (param, storage) typed_address) (_ : (param, storage) michelson_contract) (_ : int) : unit =
  let _ = Test.transfer_exn taddr (Add 7) 1mutez in
  let () = assert (Test.get_storage taddr = initial_storage + 7) in
  let _ = Test.transfer_exn taddr (Sub 3) 1mutez in
  assert (Test.get_storage taddr = initial_storage + 4)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo test-ligo group=mutation-contract-test
const tester_add_and_sub = (taddr : typed_address<param, storage>, _c : michelson_contract<param, storage>, _i : int) : unit => {
  let _xfer1 = Test.transfer_exn(taddr, Add(7), 1mutez);
  assert(Test.get_storage(taddr) == initial_storage + 7);
  let _xfer2 = Test.transfer_exn(taddr, Sub(3), 1mutez);
  assert(Test.get_storage(taddr) == initial_storage + 4);
}
```

</Syntax>

Running the updated test, we see that this time no mutation on `sub`
will give the same result.

## Multiple mutations

There is an alternative version of `Test.mutation_test` and `Test.originate_module_and_mutate` that will
collect all mutants that make the passed function correctly terminate.
Its type is similar to that of `Test.mutation_test`, but instead of
returning an optional type, it returns a list:

<Syntax syntax="cameligo">

```cameligo skip
Test.mutation_test_all : 'a -> ('a -> 'b) -> ('b * mutation) list
Test.originate_and_mutate_all : (('param, 'storage) module_contract) -> 'storage -> tez -> (('param, 'storage) typed_address -> ('param, 'storage) michelson_contract -> int -> b) -> ('b * mutation) list
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
Test.mutation_test_all : (value: 'a, tester: ('a -> 'b)) => list <['b, mutation]>;
Test.originate_and_mutate_all : (contract: module_contract<'p, 's>, init: 's, balance: tez, (tester: (originated_address: typed_address<'p, 's>, code: michelson_contract<'p, 's>, size: int) => 'b)) => list<['b, mutation]>
```

</Syntax>

The example above can be modified to collect first all mutants, and
then process the list:

<Syntax syntax="cameligo">

```cameligo test-ligo group=mutation-contract-test
let test_mutation_all =
  match Test.originate_and_mutate_all (contract_of MutationContract) initial_storage 0tez tester_add_and_sub with
    [] -> ()
  | ms -> let () = List.iter (fun ((_, mutation) : unit * mutation) ->
                              let path = Test.save_mutation "." mutation in
                              let () = Test.log "saved at:" in
                              Test.log path) ms in
          Test.println "Some mutations also pass the tests!"
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo test-ligo group=mutation-contract-test
const test_mutation_all =
  match(Test.originate_and_mutate_all(contract_of(MutationContract), initial_storage, 0tez, tester_add_and_sub)) {
    when([]): unit;
    when([hd,...tl]): do {
      let ms = list([hd,...tl]);
      let _p = Test.println("Some mutations also pass the tests!");
      for (const m of ms) {
        let [_, mutation] = m;
        let path = Test.save_mutation(".", mutation);
        let _l = Test.log("saved at:");
        let _p = Test.log(path);
      };
    }
  };
```

</Syntax>

In this case, the list of mutants is processed by saving each mutant
to a file with the help of:

<Syntax syntax="cameligo">

```cameligo skip
Test.save_mutation : string -> mutation -> string option
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
Test.save_mutation : (path: string, mutation: mutation) => option <string>
```

</Syntax>

where the first argument represents the path where the mutation is to
be saved, and the second argument is the mutation. This function
returns an optional string, representing either: the name of the file
where the mutation was saved or a failure.

## Preventing mutation

<Syntax syntax="cameligo">

In some cases, it might be a good idea to prevent mutation in certain
places. A good example of this can be an assertion that is checking
some invariant. To prevent such mutations, the attribute
`@no_mutation` can be used:

```cameligo test-ligo group=no_mutation
(* This is mutation-contract.mligo *)
type storage = int

type result = operation list * storage

(* Two entrypoints *)
[@entry]
let add (delta : int) (store : storage) : result =
  [@no_mutation] let _ = assert (0 = 0) in
  [], store + delta

[@entry] [@no_mutation]
let sub (delta : int) (store : storage) : result =
  [], store - delta
```

</Syntax>

<Syntax syntax="jsligo">

In some cases, it might be a good idea to prevent mutation in certain
places. A good example of this can be an assertion that is checking
some invariant. To prevent such mutations, the decorator
`@no_mutation` can be used:

```jsligo test-ligo group=no_mutation
// This is mutation-contract.mligo
type storage = int;

type result = [list<operation>, storage];

// Two entrypoints
@entry
const add = (delta : int, store : storage) : result => {
  @no_mutation let _a = assert (0 == 0);
  return [list([]), store + delta];
};

@entry @no_mutation
const sub = (delta : int, store : storage) : result => {
  return [list([]), store - delta];
};
```

</Syntax>

In the example, two mutations are prevented. The first one, 
The second one, it is on
the function `sub`, which prevents the mutations presented in the
example from the previous sections. is an assertion
of a silly invariant, `0` equals `0`, that should not be mutated to
things like: `0` less than `0`, `0` equal `1`, etc.

<!-- updated use of entry -->