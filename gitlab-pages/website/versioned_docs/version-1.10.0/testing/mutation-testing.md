---
id: mutation-testing
title: Mutation testing
---

import Syntax from '@theme/Syntax';

Mutation testing evaluates the quality of software tests.
Mutation testing works by creating slightly different versions of code blocks and verifying that they do not pass the same tests that the original code passes.
If the mutated versions pass the tests, the tests may not be precise enough to catch potential problems.

LIGO's testing suite provides mutation testing tools to ensure that contracts are tested in a precise, thorough way and that the tests catch changes in the contracts.

## Mutation testing a simple function

For an example of mutation testing, take this simple function that accepts an integer and doubles it:

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

To test this function, you might provide some input and output values as test cases and verify that they match:

<Syntax syntax="cameligo">

```cameligo test-ligo group=twice
module Test = Test.Next
let simple_tests (f : int -> int) =
  (* Test 1 *)
  let () = Assert.assert (Test.Compare.eq (Test.Michelson.run f 0) (Test.Michelson.eval 0)) in
  (* Test 2 *)
  let () = Assert.assert (Test.Compare.eq (Test.Michelson.run f 2) (Test.Michelson.eval 4))
  in ()

let test = simple_tests twice
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo test-ligo group=twice
import Test = Test.Next;
const simple_tests = (f : ((input: int) => int)) : unit => {
  /* Test 1 */
  Assert.assert(Test.Compare.eq(Test.Michelson.run(f, 0), Test.Michelson.eval(0)));
  /* Test 2 */
  Assert.assert(Test.Compare.eq(Test.Michelson.run(f, 2), Test.Michelson.eval(4)));
};

const test = simple_tests(twice);
```

</Syntax>

These tests check these use cases of the `twice` function:

- When run on input `0`, it returns `0`.
- When run on input `2`, it returns `4`.

The `twice` function passes the tests:

<Syntax syntax="cameligo">

```shell run
ligo run test gitlab-pages/docs/testing/src/mutation-testing/twice.mligo
# Outputs:
# Everything at the top-level was executed.
# - test exited with value ().
```

</Syntax>

<Syntax syntax="jsligo">

```shell run
ligo run test gitlab-pages/docs/testing/src/mutation-testing/twice.jsligo
# Outputs:
# Everything at the top-level was executed.
# - test exited with value ().
```

</Syntax>

The implementation does what it intends to do.
However, the implemented function is not the only function that passes the tests.
Suppose a programmer made a mistake and wrote the implementation of multiplying the input by itself instead of adding it to itself, as in this example:

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

This faulty implementation passes the above tests because its output for each test case is the same as the correct implementation.
That result suggests that the tests are not good enough to distinguish a good implementation from incorrect implementations.

To help you add more test cases and ensure that the tests are complete, you can use mutation testing to identify different versions of the function (known as _mutations_) that pass all of the tests.

The `Test.Next.Mutation.func` function takes a value to mutate (usually a function) and a test case function to apply to mutated versions of that value.
If the test case function terminates correctly, the `Test.Next.Mutation.func` function stops trying mutations and returns a `Some` option with the mutation that passed all of the tests.
If no mutation passes the test case function, the `Test.Next.Mutation.func` function returns `None`.

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

For example, this code tests mutations of the correct `twice` function implementation with the two test cases in the `simple_tests` function:

<Syntax syntax="cameligo">

```cameligo test-ligo group=twice
let test_mutation =
  match Test.Mutation.func twice simple_tests with
    None -> ()
  | Some (_, mutation) ->
      let () = Test.IO.log mutation in
      Test.IO.println "Some mutation also passes the tests! ^^"
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo test-ligo group=twice
const test_mutation =
  match(Test.Mutation.func(twice, simple_tests)) {
    when(None()): unit;
    when(Some(pmutation)): do {
      Test.IO.log(pmutation[1]);
      Test.IO.println("Some mutation also passes the tests! ^^")
    }
  };
```

</Syntax>

The mutation test returns information about a function that passes all of the tests, in this case the function `x * x`:

<Syntax syntax="cameligo">

```shell run
ligo run test gitlab-pages/docs/testing/src/mutation-testing/twice.mligo
# Outputs:
# Mutation at: File "gitlab-pages/docs/testing/src/mutation-testing/twice.mligo", line 1, characters 22-27:
#   1 | let twice (x : int) = x + x
#   2 | module Test = Test.Next
#
# Replacing by: x * x.
#
# Some mutation also passes the tests! ^^
```

</Syntax>

<Syntax syntax="jsligo">

```shell run
ligo run test gitlab-pages/docs/testing/src/mutation-testing/twice.jsligo
# Outputs:
# Mutation at: File "/Users/timothymcmackin/tezos/ligo/gitlab-pages/docs/testing/src/mutation-testing/twice.jsligo", line 1, characters 26-31:
#   1 | const twice = (x: int) => x + x;
#   2 | import Test = Test.Next;
#
# Replacing by: x * x.
#
# Some mutation also passes the tests! ^^
```

</Syntax>

You can use this information to add a test case that the mutation fails, as in this example:

<Syntax syntax="cameligo">

```cameligo skip
let simple_tests (f : int -> int) =
  (* Test 1 *)
  let () = Assert.assert (Test.Compare.eq (Test.Michelson.run f 0) (Test.Michelson.eval 0)) in
  (* Test 2 *)
  let () = Assert.assert (Test.Compare.eq (Test.Michelson.run f 2) (Test.Michelson.eval 4)) in
  (* Test 3 *)
  let () = Assert.assert (Test.Compare.eq (Test.Michelson.run f 1) (Test.Michelson.eval 2))
  in ()
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
const simple_tests = (f : ((input: int) => int)) : unit => {
  /* Test 1 */
  Assert.assert (Test.Compare.eq(Test.Michelson.run(f, 0), Test.Michelson.eval(0)));
  /* Test 2 */
  Assert.assert (Test.Compare.eq(Test.Michelson.run(f, 2), Test.Michelson.eval(4)));
  /* Test 3 */
  Assert.assert (Test.Compare.eq(Test.Michelson.run(f, 1), Test.Michelson.eval(2)));
};
```

</Syntax>

The new test case verifies that when input `1` is given, output `2` is returned.
Now you can run the mutation test again and see that no mutation that the test suite tried passes every test, giving extra confidence in the tests:

<Syntax syntax="cameligo">

```shell run
ligo run test gitlab-pages/docs/testing/src/mutation-testing/twice.mligo
# Outputs:
# Everything at the top-level was executed.
# - test exited with value ().
# - test_mutation exited with value ().
```

</Syntax>

<Syntax syntax="jsligo">

```shell run
ligo run test gitlab-pages/docs/testing/src/mutation-testing/twice.jsligo
# Outputs:
# Everything at the top-level was executed.
# - test exited with value ().
# - test_mutation exited with value ().
```

</Syntax>

## Mutating a contract

Mutation testing can also help ensure that tests cover a smart contract thoroughly.
For example, this contract has two entrypoints: one that adds a number to a value in storage and one that subtracts a number from a value in storage:

<Syntax syntax="cameligo">

```cameligo test-ligo group=mutation-contract
// This is mutation-contract.mligo
module AddSub = struct
  type storage = int

  [@entry] let add (delta : int) (storage : storage) : operation list * storage = [], storage + delta
  [@entry] let sub (delta : int) (storage : storage) : operation list * storage = [], storage - delta
end
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo test-ligo group=mutation-contract
// This is mutation-contract.jsligo
export namespace AddSub {
  export type storage = int;

  @entry
  const add = (delta: int, storage: storage): [list<operation>, storage] => [[], storage + delta];
  @entry
  const sub = (delta: int, storage: storage): [list<operation>, storage] => [[], storage - delta];
}
```

</Syntax>

Doing mutation testing on a contract with multiple entrypoints can help find entrypoints that are not covered by the tests.

For example, this test deploys a contract and tests that the `Add` entrypoint works.
Note that the test uses a function named `tester` to deploy the contract and run the tests on it:

<Syntax syntax="cameligo">

```cameligo test-ligo group=mutation-contract-test
(* This is mutation-contract-test.mligo *)

#import "gitlab-pages/docs/testing/src/mutation-testing/mutation-contract.mligo" "MutationContract"
module Test = Test.Next
type storage = MutationContract.AddSub.storage
type param = MutationContract.AddSub parameter_of
let initial_storage = 7

let tester (taddr : (param, storage) typed_address) (_ : (param ,storage) michelson_contract) (_:int) : unit =
  let _ = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "add" taddr) 7 0tez in
  Assert.assert (Test.Typed_address.get_storage taddr = initial_storage + 7)

let test_original =
  let orig = Test.Originate.contract (contract_of MutationContract.AddSub) initial_storage 0tez in
  tester orig.taddr
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo test-ligo group=mutation-contract-test
// This is mutation-contract-test.jsligo

#import "gitlab-pages/docs/testing/src/mutation-testing/mutation-contract.jsligo" "MutationContract"
import Test = Test.Next;
type storage = int;
type param = parameter_of MutationContract.AddSub;
const initial_storage = 7;

const tester = (taddr: typed_address<param, storage>, _c: michelson_contract<param, storage> , _: int): unit => {
  Test.Contract.transfer_exn(Test.Typed_address.get_entrypoint("add", taddr), 7, 0tez);
  Assert.assert(Test.Typed_address.get_storage(taddr) == initial_storage + 7);
}

const test_original = (() => {
  let orig = Test.Originate.contract(contract_of(MutationContract.AddSub), initial_storage, 0tez);
  return tester(orig.taddr);
})();
```

</Syntax>

This test runs mutation tests on the contract by passing mutations of it to the `tester` function:

<Syntax syntax="cameligo">

```cameligo test-ligo group=mutation-contract-test
let test_mutation =
  match Test.Mutation.contract (contract_of MutationContract.AddSub) initial_storage 0tez tester with
    None -> ()
  | Some (_, mutation) ->
    let () = Test.IO.log(mutation) in
    (* In a real program, one would write `failwith "A mutation passes"`
       Since we want to demonstrate the issue without an actual error
       a milder println is used in this document. *)
    Test.IO.println "A mutation of the contract still passes the tests!"
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo test-ligo group=mutation-contract-test
const test_mutation =
  match(Test.Mutation.contract(contract_of(MutationContract.AddSub), initial_storage, 0tez, tester)) {
    when(None()): unit;
    when(Some(pmutation)): do {
      Test.IO.log(pmutation[1]);
      // In a real program, one would write `failwith "A mutation passes"`
      // Because we want to demonstrate the issue without an actual error
      // a milder println is used in this document.
      Test.IO.println("A mutation of the contract still passes the tests!");
    }
  };
```

</Syntax>

The test prints a warning about the `Sub` entrypoint:

<Syntax syntax="cameligo">

```shell run
ligo run test --library . gitlab-pages/docs/testing/src/mutation-testing/mutation-contract-test.mligo
# Outputs:
# Mutation at: File "/Users/timothymcmackin/tezos/ligo/gitlab-pages/docs/testing/src/mutation-testing/mutation-contract.mligo", line 6, characters 86-101:
#   5 |   [@entry] let add (delta : int) (storage : storage) : operation list * storage = [], storage + delta
#   6 |   [@entry] let sub (delta : int) (storage : storage) : operation list * storage = [], storage - delta
#   7 | end
#
# Replacing by: storage + delta.
#
# A mutation of the contract still passes the tests!
```

</Syntax>

<Syntax syntax="jsligo">

```shell run
ligo run test --library . gitlab-pages/docs/testing/src/mutation-testing/mutation-contract-test.jsligo
# Outputs:
# Mutation at: File "gitlab-pages/docs/testing/src/mutation-testing/mutation-contract.jsligo", line 8, characters 81-96:
#   7 |   @entry
#   8 |   const sub = (delta: int, storage: storage): [list<operation>, storage] => [[], storage - delta];
#   9 | }
#
# Replacing by: storage + delta.
#
# A mutation of the contract still passes the tests!
```

</Syntax>

The mutation testing found that the `sub` function can be changed with no consequences in the test.
This warning signals that the test does not cover the `Sub` entrypoint thoroughly enough.
The following updated test adds a call to the `Sub` entrypoint to test it:

<Syntax syntax="cameligo">

```cameligo test-ligo group=mutation-contract-test
let tester_add_and_sub (taddr : (param, storage) typed_address) (_ : (param, storage) michelson_contract) (_ : int) : unit =
  let _ = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "add" taddr) 7 0tez in
  let () = Assert.assert (Test.get_storage taddr = initial_storage + 7) in
  let _ = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "sub" taddr) 3 0tez in
  Assert.assert (Test.get_storage taddr = initial_storage + 4)

let test_mutation_sub =
  match Test.Mutation.contract (contract_of MutationContract.AddSub) initial_storage 0tez tester_add_and_sub with
    None -> ()
  | Some (_, mutation) ->
    let () = Test.IO.log(mutation) in
    Test.IO.println "A mutation of the contract still passes the tests!"
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo test-ligo group=mutation-contract-test
const tester_add_and_sub = (taddr: typed_address<param, storage>, _c: michelson_contract<param, storage>, _i: int): unit => {

  Test.Contract.transfer_exn(Test.Typed_address.get_entrypoint("add", taddr), 7, 0tez);
  Assert.assert(Test.Typed_address.get_storage(taddr) == initial_storage + 7);
  Test.Contract.transfer_exn(Test.Typed_address.get_entrypoint("sub", taddr), 3, 0tez);
  Assert.assert(Test.Typed_address.get_storage(taddr) == initial_storage + 4);
}

const test_mutation_sub =
  match(Test.Mutation.contract(contract_of(MutationContract.AddSub), initial_storage, 0tez, tester_add_and_sub)) {
    when(None()): unit;
    when(Some(pmutation)): do {
      Test.IO.log(pmutation[1]);
      Test.IO.println("A mutation of the contract still passes the tests!");
    }
  };
```

</Syntax>

When this test runs, it finds that no mutation of the `Sub` entrypoint passes all of the tests and therefore does not print a warning.

## Returning multiple mutations

In the previous examples, the functions `Test.Next.Mutation.func` and `Test.Next.Mutation.contract` return an option that contains either None or Some with a single mutation that passes the tests.
To speed up the process of eliminating mutations, you can use the `Test.Next.Mutation.All.func` and `Test.Next.Mutation.All.contract` functions to get every mutation that passes the tests.
These functions return a list of mutations instead of an option.

This example gets every mutation that passes the tests for the `twice` function:

<Syntax syntax="cameligo">

```cameligo test-ligo group=twice
let get_all_mutations =
  match Test.Mutation.All.func twice simple_tests with
    [] -> ()
  | ms ->
      let () = Test.IO.println "Some mutation also passes the tests! ^^" in
      List.iter (fun ((_, mutation) : unit * mutation) -> Test.IO.log mutation) ms
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo test-ligo group=twice
const get_all_mutations =
  match(Test.Mutation.All.func(twice, simple_tests)) {
    when([]): unit;
    when([hd,...tl]): do {
      let mutations = list([hd,...tl]);
      Test.IO.println("Some mutations also pass the tests!");
      for (const m of mutations) {
        let [_, mutation] = m;
        Test.IO.log(mutation);
      };
    }
  };
```

</Syntax>

In this case, the output is the same because only one mutation passed all of the tests.

Similarly, the `Test.Next.Mutation.All.contract` function returns a list of all contract mutations that pass the tests.
For example, this test adapts the contract test in the previous section to return every passing mutation:

<Syntax syntax="cameligo">

```cameligo test-ligo group=mutation-contract-test
let test_mutation_all =
  match Test.Mutation.All.contract (contract_of MutationContract.AddSub) initial_storage 0tez tester with
    [] -> ()
  | ms ->
      let () = Test.IO.println "Some mutation also passes the tests! ^^" in
      List.iter (fun ((_, mutation) : unit * mutation) -> Test.IO.log mutation) ms
```

In this case, the output shows that multiple mutations pass the tests:

```
Mutation at: File "gitlab-pages/docs/testing/src/mutation-testing/mutation-contract.mligo", line 6, characters 86-101:
  5 |   [@entry] let add (delta : int) (storage : storage) : operation list * storage = [], storage + delta
  6 |   [@entry] let sub (delta : int) (storage : storage) : operation list * storage = [], storage - delta
  7 | end

Replacing by: storage + delta.

A mutation of the contract still passes the tests!
Some mutation also passes the tests! ^^
Mutation at: File "gitlab-pages/docs/testing/src/mutation-testing/mutation-contract.mligo", line 6, characters 86-101:
  5 |   [@entry] let add (delta : int) (storage : storage) : operation list * storage = [], storage + delta
  6 |   [@entry] let sub (delta : int) (storage : storage) : operation list * storage = [], storage - delta
  7 | end

Replacing by: storage / delta.

Mutation at: File "gitlab-pages/docs/testing/src/mutation-testing/mutation-contract.mligo", line 6, characters 86-101:
  5 |   [@entry] let add (delta : int) (storage : storage) : operation list * storage = [], storage + delta
  6 |   [@entry] let sub (delta : int) (storage : storage) : operation list * storage = [], storage - delta
  7 | end

Replacing by: storage * delta.
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo test-ligo group=mutation-contract-test
const test_mutation_all =
  match(Test.Mutation.All.contract(contract_of(MutationContract.AddSub), initial_storage, 0tez, tester)) {
    when([]): unit;
    when([hd,...tl]): do {
      let mutations = list([hd,...tl]);
      let _p = Test.IO.println("Some mutations also pass the tests!");
      for (const m of mutations) {
        let [_, mutation] = m;
        Test.IO.log(mutation);
      };
    }
  };
```

In this case, the output shows that multiple mutations pass the tests:

```
Some mutations also pass the tests!
Mutation at: File "gitlab-pages/docs/testing/src/mutation-testing/mutation-contract.jsligo", line 8, characters 81-96:
  7 |   @entry
  8 |   const sub = (delta: int, storage: storage): [list<operation>, storage] => [[], storage - delta];
  9 | }

Replacing by: storage / delta.

Mutation at: File "gitlab-pages/docs/testing/src/mutation-testing/mutation-contract.jsligo", line 8, characters 81-96:
  7 |   @entry
  8 |   const sub = (delta: int, storage: storage): [list<operation>, storage] => [[], storage - delta];
  9 | }

Replacing by: storage * delta.

Mutation at: File "gitlab-pages/docs/testing/src/mutation-testing/mutation-contract.jsligo", line 8, characters 81-96:
  7 |   @entry
  8 |   const sub = (delta: int, storage: storage): [list<operation>, storage] => [[], storage - delta];
  9 | }

Replacing by: storage + delta.
```

</Syntax>

## Preventing mutation

<Syntax syntax="cameligo">

In some cases you may want to prevent mutations from changing certain parts of your code that should not change.
To prevent such mutations, apply the `@no_mutation` attribute.

This example uses this attribute to prevent mutations from changing an invariant, in this case that zero equals zero.
With this attribute, the tests will not test meaningless assertions such as that zero is less than zero or that zero equals one.
It also uses this attribute to prevent mutations from changing the `Sub` entrypoint, which prevents the warnings from the previous sections:

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

In some cases you may want to prevent mutations from changing certain parts of your code that should not change.
To prevent such mutations, apply the `@no_mutation` decorator.

This example uses this decorator to prevent mutations from changing an invariant, in this case that zero equals zero.
With this decorator, the tests will not test meaningless assertions such as that zero is less than zero or that zero equals one.
It also uses this decorator to prevent mutations from changing the `Sub` entrypoint, which prevents the warnings from the previous sections:

```jsligo test-ligo group=no_mutation
// This is mutation-contract.mligo
type storage = int;

type result = [list<operation>, storage];

// Two entrypoints
@entry
const add = (delta : int, store : storage) : result => {
  @no_mutation let _a = assert (0 == 0);
  return [[], store + delta];
};

@entry @no_mutation
const sub = (delta : int, store : storage) : result => {
  return [[], store - delta];
};
```

</Syntax>

<!-- updated use of entry -->
