---
id: testing
title: Testing
---

import Syntax from '@theme/Syntax';

The LIGO command-line interpreter provides commands to test your LIGO code.
It provides three main ways to test code:

* `ligo run test`: Runs automated tests in LIGO code

* `ligo run interpret`: Interprets a LIGO expression in the context of a LIGO file

* `ligo run dry-run`: Simulates running a contract based on a given parameter and storage value

:::warning
LIGO testing tools are in beta and may change.
No production test procedure should rely on these tools alone.
:::

## Testing with `ligo run test`

The command `ligo run test` runs automated tests on a contract.

When running the `ligo run test` command, LIGO code has access to an additional `Test` module.
This module provides ways of originating contracts and executing transactions in a simulated environment, as well as additional helper functions that allow you to control different parameters of the Tezos testing library.

:::note
The LIGO interpreter uses the [same library that Tezos internally uses for testing](https://gitlab.com/tezos/tezos/-/tree/master/src/proto_alpha/lib_protocol/test/helpers).
:::

To originate a contract in the test simulation, use the `Test.Next.originate` function, which accepts these parameters:

- The contract itself
- The initial storage value
- The starting balance of the contract in tez

The function returns an object that has these values:

- `taddr`: The address of the deployed contract in the simulation
- `size`: The size of the deployed contract in bytes, as an integer
- `code`: The Michelson code of the contract

You can get the storage of a deployed contract by passing the address of the contract to the `Test.Next.Typed_address.get_storage` function.

For example, this LIGO file includes a simple counter contract:

<Syntax syntax="cameligo">

```cameligo test-ligo group=mycontract
// This is mycontract.mligo
module MyContract = struct
  type storage = int
  type result = operation list * storage

  [@entry] let increment (delta : int) (storage : storage) : result = [],storage + delta
  [@entry] let decrement (delta : int) (storage : storage) : result = [],storage - delta
  [@entry] let reset () (_storage : storage) : result = [], 0
end
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo test-ligo group=mycontract
// This is mycontract.jsligo
export namespace MyContract {
  export type storage = int;
  export type result = [list<operation>, storage];

  @entry const increment = (delta : int, storage : storage) : result => [[], storage + delta];
  @entry const decrement = (delta : int, storage : storage) : result => [[], storage - delta];
  @entry const reset = (_u : unit, _storage : storage) : result => [[], 0];
}
```

</Syntax>

To test the contract, create a function to originate the contract in the test simulation, call it, and verify the result.
You can put the test functions in the same file or a separate file.

This example shows a test in a separate file.
It follows these basic steps:

1. It imports the contract file with the `import` directive.
1. It creates a function named `run_test1` for the test.
1. In the function, it creates a value for the initial storage of the contract.
1. It originates the contract to the test simulation with the initial storage.
1. It verifies that the deployed contract has the storage value.
1. It calls the `increment` entrypoint with the `Test.Next.Contract.transfer_exn` function, passing the entrypoint, the parameter, and 0 tez.
1. It verifies the updated storage value.

<Syntax syntax="cameligo">

```cameligo test-ligo group=mycontract-test
(* This is mycontract-test.mligo *)

#import "gitlab-pages/docs/testing/src/testing/mycontract.mligo" "MyContract"

let run_test1 =
  let initial_storage = 10 in
  let orig = Test.Next.Originate.contract (contract_of MyContract.MyContract) initial_storage 0tez in
  let () = Assert.assert (Test.Next.Typed_address.get_storage(orig.taddr) = initial_storage) in
  let _: nat = Test.Next.Contract.transfer_exn (Test.Next.Typed_address.get_entrypoint "increment" orig.taddr) 32 0tez in
  Assert.assert (Test.Next.Typed_address.get_storage(orig.taddr) = initial_storage + 32)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo test-ligo group=mycontract-test
// This is mycontract-test.jligo

#import "gitlab-pages/docs/testing/src/testing/mycontract.jsligo" "MyModule"

const run_test1 = () => {
    let initial_storage = 10;
    let orig = Test.Next.Originate.contract(contract_of(MyModule.MyContract), initial_storage, 0tez);
    Test.Next.Contract.transfer_exn(Test.Next.Typed_address.get_entrypoint("increment", orig.taddr), 5, 0tez);
    return Assert.assert(Test.Next.Typed_address.get_storage(orig.taddr) == initial_storage + 5);
};

const test1 = run_test1();
```

</Syntax>

The `run test` command evaluates all top-level definitions and prints any
entries that begin with the prefix `test` as well as the value that these
definitions evaluate to. If any of the definitions fail, it prints a message
with the line number where the problem occurred.
You can also log messages to the console with the `Test.Next.IO.log` function.

To run the tests, pass the file with the tests to the `run test` command.
If the file imports other files, pass the folders that contain these files in the `--library` argument, as in this example:

<Syntax syntax="cameligo">

```shell
ligo run test --library gitlab-pages/docs/testing/src/testing/ gitlab-pages/docs/testing/src/testing/mycontract-test.mligo
```

</Syntax>

<Syntax syntax="jsligo">

```shell
ligo run test --library gitlab-pages/docs/testing/src/testing/ gitlab-pages/docs/testing/src/testing/mycontract-test.jsligo
```

</Syntax>

The response shows that the functions at the top level of the file ran successfully:

```
Everything at the top-level was executed.
- test1 exited with value ().
```

### Creating transactions

The function `Test.Next.Contract.transfer_exn` creates a transaction in the test simulation, as in the example in the previous section.
It takes these parameters:

- The target entrypoint or account to call
- The parameter to pass
- The amount of tez to include

If the transaction succeeds, it returns the gas consumption.
If it fails, it fails the test.

For greater control, such as to test error conditions and error messages, you can use the function `Test.Next.Contract.transfer`.
The function takes the same parameters but returns an option of the type `test_exec_result`, which is `Fail` if the transaction failed and `Success` if it succeeded.
In case of success the value is the gas consumed and in case of failure the value is an object of the type `test_exec_error` that describes the error.

:::warning
If you create a transaction with `Test.Next.Contract.transfer` and the transaction fails, the test does not automatically fail.
You must check the result of the transaction to see if it succeeded or failed.
:::

For example, this contract is similar to the contract in an earlier example, but it only allows the number in storage to change by 5 or less with each transaction:

<Syntax syntax="cameligo">

```cameligo group=mycontract-failures
module MyContract = struct
  type storage = int
  type result = operation list * storage

  [@entry] let increment (delta : int) (storage : storage) : result =
    if abs delta <= 5n then [], storage + delta else failwith "Pass 5 or less"
  [@entry] let decrement (delta : int) (storage : storage) : result =
    if abs delta <= 5n then [], storage - delta else failwith "Pass 5 or less"
  [@entry] let reset () (_storage : storage) : result = [], 0
end
```

This test verifies that the error works by passing a number larger than 5 and handling the error:

```cameligo group=mycontract-failures
let test_failure =
  let initial_storage = 10 in
  let orig = Test.Next.Originate.contract (contract_of MyContract) initial_storage 0tez in
  let result: test_exec_result = Test.Next.Contract.transfer (Test.Next.Typed_address.get_entrypoint "increment" orig.taddr) 50 0tez in
  match result with
    Fail _x -> Test.Next.IO.log "Failed as expected"
  | Success _s -> failwith "This should not succeed"
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=mycontract-failures
namespace MyContract {
  export type storage = int;
  export type result = [list<operation>, storage];

  @entry const increment = (delta : int, storage : storage) : result =>
    abs(delta) <= 5n ? [[], storage + delta] : failwith("Pass 5 or less");
  @entry const decrement = (delta : int, storage : storage) : result =>
    abs(delta) <= 5n ? [[], storage - delta] : failwith("Pass 5 or less");
  @entry const reset = (_u : unit, _storage : storage) : result => [[], 0];
}
```

This test verifies that the error works by passing a number larger than 5 and handling the error:

```jsligo group=mycontract-failures
const test_failure = () => {
  const initial_storage = 10 as int;
  const orig = Test.Next.Originate.contract(contract_of(MyContract), initial_storage, 0tez);
  const result = Test.Next.Contract.transfer(Test.Next.Typed_address.get_entrypoint("increment", orig.taddr), 50 as int, 0tez);

  match(result) {
    when(Fail(_x)): Test.Next.IO.log("Failed as expected");
    when(Success(_s)): failwith("This should not succeed")
  };
}
```

</Syntax>

### Generating test accounts

You can use test accounts to simulate real accounts in tests.
For example, assume that you want to allow only an administrator account to call the `reset` entrypoint in the contract from the previous example.
This version adds an administrator address to the contract storage.
It checks the sender of the transaction in the `reset` entrypoint and fails if the addresses don't match:

<Syntax syntax="cameligo">

```cameligo group=test-accounts
module Counter = struct
  type storage = int * address
  type return_type = operation list * storage

  [@entry] let increment (n : int) (storage : storage) : return_type =
    let (number, admin_account) = storage in
    [], (number + n, admin_account)

  [@entry] let sub (n : int) (storage : storage) : return_type =
    let (number, admin_account) = storage in
    [], (number - n, admin_account)

  [@entry] let reset (_ : unit) (storage : storage) : return_type =
    let (_number, admin_account) = storage in
    if Tezos.get_sender() = admin_account then
      [], (0, admin_account)
    else
      failwith "Only the owner can call this entrypoint"
end
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=test-accounts
namespace Counter {
  type storage = [int, address];
  type return_type = [list<operation>, storage];

  @entry
  const increment = (n: int, storage: storage): return_type => {
    const [number, admin_account] = storage;
    return [[], [number + n, admin_account]];
  }

  @entry
  const decrement = (n: int, storage: storage): return_type => {
    const [number, admin_account] = storage;
    return [[], [number - n, admin_account]];
  }

  @entry
  const reset = (_: unit, storage: storage): return_type => {
    const [_number, admin_account] = storage;

    if (Tezos.get_sender() != admin_account) {
      return failwith("Only the owner can call this entrypoint");
    }

    return [[], [0, admin_account]];
  }
};
```

</Syntax>

To generate test accounts, pass a nat to the `Test.Next.Account.address` function, which returns an address.
Then use the `Test.Next.State.set_source` function to set the source account for transactions.

This example creates an admin account and user account.
It attempts to call the `reset` entrypoint as the user account and expects it to fail.
Then it calls the `reset` entrypoint as the admin account and verifies that the entrypoint runs correctly:

<Syntax syntax="cameligo">

```cameligo group=test-accounts
let test_admin =
  let (admin_account, user_account) = (Test.Next.Account.address(0n), Test.Next.Account.address(1n)) in

  // Originate the contract with the admin account in storage
  let initial_storage = (10, admin_account) in
  let orig = Test.Next.Originate.contract (contract_of Counter) initial_storage 0tez in

  // Try to call the reset entrypoint as the user and expect it to fail
  let () = Test.Next.State.set_source user_account in
  let result = Test.Next.Contract.transfer (Test.Next.Typed_address.get_entrypoint "reset" orig.taddr ) unit 0tez in
  let () = match result with
    Fail _err -> Test.Next.IO.log "Test succeeded"
  | Success _s -> failwith "User should not be able to call reset" in

  // Call the reset entrypoint as the admin and expect it to succeed
  let () = Test.Next.State.set_source admin_account in
  let _: nat = Test.Next.Contract.transfer_exn (Test.Next.Typed_address.get_entrypoint "reset" orig.taddr) unit 0tez in

  let (newNumber, _admin_account) = Test.Next.Typed_address.get_storage orig.taddr in
  Assert.assert (newNumber = 0)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=test-accounts
const test_admin = (() => {
  const admin_account = Test.Next.Account.address(0n);
  const user_account = Test.Next.Account.address(1n);

  // Originate the contract with the admin account in storage
  const initial_storage = [10 as int, admin_account];
  const orig = Test.Next.Originate.contract(contract_of(Counter), initial_storage, 0tez);

  // Try to call the reset entrypoint as the user and expect it to fail
  Test.Next.State.set_source(user_account);
  const result = Test.Next.Contract.transfer(Test.Next.Typed_address.get_entrypoint("reset", orig.taddr), unit, 0tez);
  match(result) {
    when(Fail(_err)): Test.Next.IO.log("Test succeeded");
    when (Success(_s)): failwith("User should not be able to call reset");
  };

  // Call the reset entrypoint as the admin and expect it to succeed
  Test.Next.State.set_source(admin_account);
  Test.Next.Contract.transfer_exn(Test.Next.Typed_address.get_entrypoint("reset", orig.taddr), unit, 0tez);

  const [newNumber, _admin_account] = Test.Next.Typed_address.get_storage(orig.taddr);
  Assert.assert(newNumber == 0);
}) ()
```

</Syntax>

By default, the test simulation has two test accounts.
To create more, pass the number of accounts and a list of their balances or an empty list to use the default balance to the `Test.Next.State.Reset` function, as in the following example.
The default balance is 4000000 tez minus %5 that is frozen so the account can act as a validator.

<Syntax syntax="cameligo">

```cameligo test-ligo group=reset
let test_accounts =
  let initial_balances : tez list = [] in
  let () = Test.Next.State.reset 3n initial_balances in
  let admin_account = Test.Next.Account.address(0n) in
  let user_account1 = Test.Next.Account.address(1n) in
  let user_account2 = Test.Next.Account.address(2n) in

  let () = Test.Next.IO.log (Test.Next.Address.get_balance admin_account) in
  // 3800000000000mutez
  let () = Test.Next.IO.log (Test.Next.Address.get_balance user_account1) in
  // 3800000000000mutez
  Test.Next.IO.log (Test.Next.Address.get_balance user_account2)
  // 3800000000000mutez
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo test-ligo group=reset
const test_accounts = () => {
  Test.Next.State.reset(3n, [] as list <tez>);
  const admin_account = Test.Next.Account.address(0n);
  const user_account1 = Test.Next.Account.address(1n);
  const user_account2 = Test.Next.Account.address(2n);

  Test.Next.IO.log(Test.Next.Address.get_balance(admin_account));
  // 3800000000000mutez
  Test.Next.IO.log(Test.Next.Address.get_balance(user_account1));
  // 3800000000000mutez
  Test.Next.IO.log(Test.Next.Address.get_balance(user_account2));
  // 3800000000000mutez
}
```

</Syntax>

### Testing events

To test events, emit them as usual with the `Tezos.emit` function and use the `Test.Next.State.last_events` function to capture the most recent events, as in this example:

<Syntax syntax="cameligo">

```cameligo test-ligo group=test_ex
module C = struct
  [@entry] let main (p : int*int) () =
    [Tezos.emit "%foo" p ; Tezos.emit "%foo" p.0],()
end

let test_foo =
  let orig = Test.Next.Originate.contract (contract_of C) () 0tez in
  let _: nat = Test.Next.Typed_address.transfer_exn orig.taddr (Main (1,2)) 0tez in
  (Test.Next.State.last_events orig.taddr "foo" : (int*int) list),(Test.Next.State.last_events orig.taddr "foo" : int list)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo test-ligo group=test_ex
namespace C {
  @entry
  const main = (p: [int, int], _: unit) => {
    const op1 = Tezos.emit("%foo", p);
    const op2 = Tezos.emit("%foo", p[0]);
    return [([op1, op2] as list<operation>), unit];
  };
}
const test = () => {
  const orig = Test.Next.Originate.contract(contract_of(C), unit, 0tez);
  Test.Next.Typed_address.transfer_exn(orig.taddr, Main ([1,2]), 0tez);
  return [Test.Next.State.last_events(orig.taddr, "foo") as list<[int, int]>, Test.Next.State.last_events(orig.taddr, "foo") as list<int>];
};
```

</Syntax>

### Unit testing functions

You can use the `run test` command to run unit tests of functions.

A common way of unit testing functions is to create a map of input values and expected output values and iterate over them.
For example, consider a map binding addresses to amounts and a function removing all entries in that map that have an amount less than a given threshold:

<Syntax syntax="cameligo">

```cameligo group=remove-balance
(* This is remove-balance.mligo *)

type balances = (address, tez) map

let remove_balances_under (b : balances) (threshold : tez) : balances =
  Map.fold
    (fun ((acc, (k, v)) : balances * (address * tez)) ->
       if v < threshold then Map.remove k acc else acc)
    b b
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=remove-balance
// This is remove-balance.jsligo

type balances = map <address, tez>;

const remove_balances_under = (b: balances, threshold: tez): balances => {
  let f = ([acc, kv]: [balances, [address, tez]] ): balances => {
    let [k, v] = kv;
    if (v < threshold) { return Map.remove (k, acc) } else {return acc}
  };
  return Map.fold (f, b, b);
}
```

</Syntax>

You can test this function against a range of thresholds with the LIGO test framework.

<!-- I divided unit-remove-balance in multiple part of clarity -->
First, include the file under test and reset the state with 5 bootstrap accounts:

<Syntax syntax="cameligo">

```cameligo test-ligo group=unit-remove-balance-mixed
#include "./gitlab-pages/docs/testing/src/testing/remove-balance.mligo"

let test_remove_balance =
  let () = Test.Next.State.reset 5n ([]: tez list) in
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo test-ligo group=unit-remove-balance-mixed
#include "./gitlab-pages/docs/testing/src/testing/remove-balance.jsligo"

const test_remove_balance = (() => {
  Test.Next.State.reset(5n, [] as list <tez>);
```

</Syntax>

Now build the `balances` map that serves as the test input:

<Syntax syntax="cameligo">

```cameligo test-ligo group=unit-remove-balance-mixed
let balances: balances =
  let a1, a2, a3 = Test.Next.Account.address 1n, Test.Next.Account.address 2n, Test.Next.Account.address 3n
  in Map.literal [(a1, 10tz); (a2, 100tz); (a3, 1000tz)] in
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo test-ligo group=unit-remove-balance-mixed
const balances: balances =
  Map.literal([[Test.Next.Account.address(1n), 10tez],
              [Test.Next.Account.address(2n), 100tez],
              [Test.Next.Account.address(3n), 1000tez]]);
```

</Syntax>

The test loop will call the function with the compiled map
defined above, get the size of the resulting map, and compare it to an
expected value with `Test.Next.Compare.eq`.

The call to `remove_balances_under` and the computation of the size of the resulting map is achieved through the primitive `Test.Next.Michelson.run`.
This primitive runs a function on an input, translating both (function and input)
to Michelson before running on the Michelson interpreter.
More concretely `Test.Next.Michelson.run f v` performs the following:

1. Compiles the function argument `f` to Michelson `f_mich`
2. Compiles the value argument `v` (which was already evaluated) to Michelson `v_mich`
3. Runs the Michelson interpreter on the code `f_mich` with the initial stack `[ v_mich ]`

The function that is being compiled is called `tester`.

We also print the actual and expected sizes for good measure.

<Syntax syntax="cameligo">

```cameligo test-ligo group=unit-remove-balance-mixed
List.iter
  (fun ((threshold , expected_size): tez * nat) ->
    let tester (balances, threshold: balances * tez) = Map.size (remove_balances_under balances threshold) in
    let size = Test.Next.Michelson.run tester (balances, threshold) in
    let expected_size = Test.Next.Michelson.eval expected_size in
    let () = Test.Next.IO.log ("expected", expected_size) in
    let () = Test.Next.IO.log ("actual", size) in
    Assert.assert (Test.Next.Compare.eq size expected_size)
  )
  [(15tez,2n); (130tez,1n); (1200tez,0n)]
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo test-ligo group=unit-remove-balance-mixed
return List.iter(([threshold, expected_size]: [tez, nat]): unit => {
    const tester = ([balances, threshold]: [balances, tez]): nat =>
      Map.size (remove_balances_under (balances, threshold));
    const size = Test.Next.Michelson.run(tester, [balances, threshold]);
    const expected_size_ = Test.Next.Michelson.eval(expected_size);
    Test.Next.IO.log(["expected", expected_size]);
    Test.Next.IO.log(["actual", size]);
    return (Assert.assert (Test.Next.Compare.eq(size, expected_size_)))
  },
  list ([ [15tez, 2n], [130tez, 1n], [1200tez, 0n]]) );
}) ()
```

</Syntax>

Here is the complete test file:

<Syntax syntax="cameligo">

```cameligo test-ligo group=unit-remove-balance-complete
#include "./gitlab-pages/docs/testing/src/testing/remove-balance.mligo"

let test_remove_balance =
  let () = Test.Next.State.reset 5n ([]: tez list) in
let balances: balances =
  let a1, a2, a3 = Test.Next.Account.address 1n, Test.Next.Account.address 2n, Test.Next.Account.address 3n
    in Map.literal [(a1, 10tz); (a2, 100tz); (a3, 1000tz)] in
  List.iter
    (fun ((threshold , expected_size): tez * nat) ->
      let tester (balances, threshold: balances * tez) = Map.size (remove_balances_under balances threshold) in
      let size = Test.Next.Michelson.run tester (balances, threshold) in
      let expected_size = Test.Next.Michelson.eval expected_size in
      let () = Test.Next.IO.log ("expected", expected_size) in
      let () = Test.Next.IO.log ("actual", size) in
      Assert.assert (Test.Next.Compare.eq size expected_size)
    )
    [(15tez,2n); (130tez,1n); (1200tez,0n)]
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo test-ligo group=unit-remove-balance-complete
#include "./gitlab-pages/docs/testing/src/testing/remove-balance.jsligo"

const test_remove_balance = (() => {
  Test.Next.State.reset(5n, [] as list <tez>);
  const balances: balances =
    Map.literal([[Test.Next.Account.address(1n), 10tez],
                 [Test.Next.Account.address(2n), 100tez],
                 [Test.Next.Account.address(3n), 1000tez]]);
  return List.iter(([threshold, expected_size]: [tez, nat]): unit => {
      const tester = ([balances, threshold]: [balances, tez]): nat =>
        Map.size (remove_balances_under (balances, threshold));
      const size = Test.Next.Michelson.run(tester, [balances, threshold]);
      const expected_size_ = Test.Next.Michelson.eval(expected_size);
      Test.Next.IO.log(["expected", expected_size]);
      Test.Next.IO.log(["actual", size]);
      return (Assert.assert (Test.Next.Compare.eq(size, expected_size_)))
    },
    list ([ [15tez, 2n], [130tez, 1n], [1200tez, 0n]]) );
}) ()
```

</Syntax>

You can now execute the test by running this command:

<Syntax syntax="cameligo">

```shell
ligo run test --library . gitlab-pages/docs/testing/src/testing/unit-remove-balance-mixed.mligo
```

</Syntax>

<Syntax syntax="jsligo">

```shell
ligo run test --library . gitlab-pages/docs/testing/src/testing/unit-remove-balance-mixed.jsligo
```

</Syntax>

The response shows the expected and actual results of each test run:

```
# Outputs:
# ("expected" , 2)
# ("actual" , 2)
# ("expected" , 1)
# ("actual" , 1)
# ("expected" , 0)
# ("actual" , 0)
# Everything at the top-level was executed.
# - test exited with value ().
```

## Testing with `ligo run interpret`

The command `ligo run interpret` interprets a LIGO expression in a
context initialised by a source file. The interpretation is done using
Michelson's interpreter.

For example, suppose you have a function that encodes input values into a specific format.
This function takes two input values and uses them as the key and value for an entry in a map:

<Syntax syntax="cameligo">

```cameligo group=interpret
// This is interpret.mligo
type myDataType = (int, string) map

let encodeEntry (a : int) (b : string): myDataType =
  Map.literal [(a, b)]
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=interpret
// This is interpret.jsligo
type myDataType = map<int, string>;

const encodeEntry = (a: int, b: string): myDataType => {
  return Map.literal([[a, b]]);
}
```

</Syntax>

To encode values with this function, pass the LIGO expression to call the function to the `run interpret` command and include the LIGO file in the `--init-file` argument:

<Syntax syntax="cameligo">

```shell
ligo run interpret 'encodeEntry 5 "hello"' --init-file gitlab-pages/docs/testing/src/testing/interpret.mligo
```

</Syntax>

<Syntax syntax="jsligo">

```shell
ligo run interpret 'encodeEntry(5, "hello")' --init-file gitlab-pages/docs/testing/src/testing/interpret.jsligo
```

</Syntax>

The response is the Michelson-encoded value of the output of the function:

```michelson
MAP_ADD(5 , "hello" , MAP_EMPTY())
```

You can use the `run interpret` command to interpret complex LIGO code and get the output in Michelson, such as formatting parameters for calls to entrypoints.

You can pass these arguments to set parameters for the interpretation:

- `--amount`: The amount of tez to send with the transaction; the default is 0
- `--balance`: The amount of tez in the contract; the default is 0
- `--now`: The current timestamp, such as `2000-01-01T10:10:10Z`
- `--sender`: The address for the sender of the transaction
- `--source`: The address for the source of the transaction

## Testing with `ligo run dry-run`

The `ligo run dry-run` command runs the contract in a simulated
environment. You can use it to test contracts with given parameters
and storage values. You pass these arguments to the command:

- The contract file to run
- The parameter to pass to the contract, as a LIGO expression
- The value of the contract storage, as a LIGO expression

For example, this contract stores a number and allows callers to
increment it by one:

<Syntax syntax="cameligo">

```cameligo group=dry-run-simple
module Counter = struct
  type storage_type = int
  type return_type = operation list * storage_type

  [@entry]
  let main (_action: unit) (storage: storage_type): return_type =
    [], storage + 1
end
```

This command tests the contract with the `run dry-run` command:

```bash
ligo run dry-run -m Counter gitlab-pages/docs/testing/src/testing/counter_simple.mligo 'unit' '4'
```

The result shows the new value of the storage:

```
( LIST_EMPTY() , 5 )
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=dry-run-simple
namespace Counter {
  type storage_type = int;
  type return_type = [list<operation>, storage_type];

  @entry
  const main = (_action: unit, storage: storage_type): return_type =>
    [[], storage + 1]
}
```

This command tests the contract with the `run dry-run` command:

```bash
ligo run dry-run -m Counter gitlab-pages/docs/testing/src/testing/counter_simple.jsligo 'unit' '4'
```

The result shows the new value of the storage:

```
( LIST_EMPTY() , 5 )
```

</Syntax>

For a more complicated example, this contract stores a map and
provides an entrypoint that updates elements in it:

<Syntax syntax="cameligo">

```cameligo group=dry-run-complex
module MyContract = struct
  type storage_type = (nat, string) map
  type return_type = operation list * storage_type

  [@entry]
  let update (param: nat * string) (storage: storage_type): return_type =
    let (index, value) = param in
    let updated_map = Map.add index value storage in
    [], updated_map

end
```

You can test the entrypoint and view the resulting operations and
storage by running this command, which uses an empty map of the same
type as the contract storage as the initial value of the storage:

```bash
ligo run dry-run -m MyContract gitlab-pages/docs/testing/src/testing/dry-run-complex.mligo \
  'Update(1n, "hi")' \
  '(Map.empty : (nat, string) map)'
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=dry-run-complex
namespace MyContract {
  type storage_type = map<nat, string>;
  type return_type = [list<operation>, storage_type];

  @entry
  const update = (param: [nat, string], storage: storage_type): return_type => {
    const [index, value] = param;
    const updated_map = Map.add(index, value, storage);
    return [[], updated_map];
  }
}
```

You can test the entrypoint and view the resulting operations and
storage by running this command, which uses an empty map of the same
type as the contract storage as the initial value of the storage:

```bash
ligo run dry-run -m MyContract gitlab-pages/docs/testing/src/testing/dry-run-complex.jsligo \
  'Update(1n, "new value")' \
  'Map.empty as map<nat, string>'
```

</Syntax>

Note that the values of the parameter and the initial storage state
are both LIGO expressions.

The result shows the empty list of operations and the new value of the
storage, expressed by adding elements to an empty map:

```
( LIST_EMPTY() , MAP_ADD(+1 , "new value" , MAP_EMPTY()) )
```

You can pass these arguments to set parameters for the transaction:

- `--amount`: The amount of tez to send with the transaction; the default is 0
- `--balance`: The amount of tez in the contract; the default is 0
- `--now`: The current timestamp, such as `2000-01-01T10:10:10Z`
- `--sender`: The address for the sender of the transaction
- `--source`: The address for the source of the transaction
