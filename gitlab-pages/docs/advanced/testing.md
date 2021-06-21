---
id: testing
title: Testing LIGO
---

import Syntax from '@theme/Syntax';
import Link from '@docusaurus/Link';

## Testing LIGO code

The LIGO command-line interpreter provides sub-commands to
directly test your LIGO code. The three main sub-commands we currently
support are:

* `test`

* `interpret`

* `dry-run`

We will show how to use the first two, while an example on how to use
the third one was already explained
[here](first-contract.md#dry-running-a-contract).

### Testing with `test`

The sub-command `test` can be used to test a contract using LIGO.

> ⚠️ Please keep in mind that this sub-command is still BETA, and that
> there are features that are work in progress and are subject to
> change. No real test procedure should rely on this sub-command
> alone.

#### Testing with `test`: internally

When running the `test` sub-command, LIGO code has access to an
additional `Test` module. This module provides ways of originating
contracts and executing transactions, as well as additional helper
functions that allow to control different parameters of the Tezos
testing library.

> ⚠️ The testing framework has been recently updated, changing a few of
> the basic functions in the module `Test`. Please check the next
> section in case you need to test files using the previous
> primitives.

The function `Test.originate` allows to deploy a contract in the
testing environment. It takes a contract, which is represented as a
function of type `'parameter * 'storage -> operation list * 'storage`,
an initial storage of type `'storage`, and an initial balance for the
contract being deployed. This function deploys the contract, and
returns the type
`('parameter, 'storage) typed_address`, the compiled program in
Michelson of type `michelson_program`, and the size of the program of
type `int`.

The storage of a deployed contract can be queried using the
`Test.get_storage` function, that given a typed address `('parameter,
'storage) typed_address`, returns the `'storage` value.

As a concrete example, suppose we have the following contract:
<Syntax syntax="pascaligo">

```pascaligo test-ligo group=frontpage
// This is testnew.ligo
type storage is int

type parameter is
  Increment of int
| Decrement of int
| Reset

type return is list (operation) * storage

// Two entrypoints
function add (const store : storage; const delta : int) : storage is
  store + delta
function sub (const store : storage; const delta : int) : storage is
  store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)
function main (const action : parameter; const store : storage) : return is
 ((nil : list (operation)),    // No operations
  case action of
    Increment (n) -> add (store, n)
  | Decrement (n) -> sub (store, n)
  | Reset         -> 0
  end)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo test-ligo group=frontpage
// This is testnew.mligo
type storage = int

type parameter =
  Increment of int
| Decrement of int
| Reset

type return = operation list * storage

// Two entrypoints
let add (store, delta : storage * int) : storage = store + delta
let sub (store, delta : storage * int) : storage = store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)
let main (action, store : parameter * storage) : return =
 ([] : operation list),    // No operations
 (match action with
   Increment (n) -> add (store, n)
 | Decrement (n) -> sub (store, n)
 | Reset         -> 0)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo test-ligo group=frontpage
// This is testme.religo
type storage = int;

type parameter =
  Increment (int)
| Decrement (int)
| Reset;

type return = (list (operation), storage);

// Two entrypoints
let add = ((store, delta) : (storage, int)) : storage => store + delta;
let sub = ((store, delta) : (storage, int)) : storage => store - delta;

/* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. */
let main = ((action, store) : (parameter, storage)) : return => {
 (([] : list (operation)),    // No operations
 (switch (action) {
  | Increment (n) => add ((store, n))
  | Decrement (n) => sub ((store, n))
  | Reset         => 0}))
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo test-ligo group=frontpage
// This is testnew.jsligo
type storage = int;

type parameter =
  ["Increment", int]
| ["Decrement", int]
| ["Reset"];

type return_ = [list<operation>, storage];

// Two entrypoints
let add = ([store, delta]: [storage, int]): storage => store + delta;
let sub = ([store, delta]: [storage, int]): storage => store - delta;

/* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. */
let main = ([action, store]: [parameter, storage]) : return_ => {
  return [
    list([]) as list<operation>,    // No operations
    match(action, {
      Increment:(n: int) => add ([store, n]),
      Decrement:(n: int) => sub ([store, n]),
      Reset: ()          => 0})
  ]
};
```

</Syntax>


We can deploy it and query the storage right after, to check that the
storage is in fact the one which we started with:

<Syntax syntax="pascaligo">

```pascaligo test-ligo group=frontpage
// This continues testnew.ligo

const test =
  block {
    const initial_storage = 42;
    const (taddr, _, _) = Test.originate(main, initial_storage, 0tez);
    const storage = Test.get_storage(taddr);
  } with (storage = initial_storage);

```

</Syntax>
<Syntax syntax="cameligo">

```cameligo test-ligo group=frontpage
// This continues testnew.mligo

let test =
  let initial_storage = 42 in
  let (taddr, _, _) = Test.originate main initial_storage 0tez in
  assert (Test.get_storage taddr = initial_storage)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo test-ligo group=frontpage
// This continues testnew.religo

let test =
  let initial_storage = 42;
  let (taddr, _, _) = Test.originate(main, initial_storage, 0tez);
  assert (Test.get_storage(taddr) == initial_storage)
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo test-ligo group=frontpage
// This continues testnew.jsligo

let _test = () : bool => {
  let initial_storage = 42 as int;
  let [taddr, _, _] = Test.originate(main, initial_storage, 0 as tez);
  return (Test.get_storage(taddr) == initial_storage);
};

let test = _test();
```

</Syntax>

The test subcommand will evaluate all top-level definitions and print any
entries that begin with the prefix `test` as well as the value that these
definitions evaluate to. If any of the definitions are found to have
failed, a message will be issued with the line number where the problem
occurred.

<Syntax syntax="pascaligo">

```shell
ligo test gitlab-pages/docs/advanced/src/testnew.ligo
// Outputs:
// Everything at the top-level was executed.
// - test exited with value true.
```

</Syntax>
<Syntax syntax="cameligo">

```shell
ligo test gitlab-pages/docs/advanced/src/testnew.mligo
// Outputs:
// Everything at the top-level was executed.
// - test exited with value true.
```

</Syntax>
<Syntax syntax="reasonligo">

```shell
ligo test gitlab-pages/docs/advanced/src/testnew.religo
// Outputs:
// Everything at the top-level was executed.
// - test exited with value true.
```

</Syntax>
<Syntax syntax="jsligo">

```shell
ligo test gitlab-pages/docs/advanced/src/testnew.jsligo
// Outputs:
// Everything at the top-level was executed.
// - test exited with value true.
```

</Syntax>


The function `Test.transfer_to_contract` allows to bake a transaction.
It takes a target account of type `'parameter contract`, the parameter
of type `'parameter` and an amount of type `tez`. This function
performs the transaction, and returns a `test_exec_result`, which
tells whether the transaction was successful or not, and in case it
was not, it contains a `test_exec_error` describing the error. There
is an alternative version, called `Test.transfer_to_contract_exn`
which performs the transaction and ignores the result, failing in case
that there was an error.

We can extend the previous example by executing a transaction that
increments the storage after deployment:
<Syntax syntax="pascaligo">

```pascaligo test-ligo group=frontpage
// This continues testnew.ligo

const test2 =
  block {
    const initial_storage = 42;
    const (taddr, _, _) = Test.originate(main, initial_storage, 0tez);
    const contr = Test.to_contract(taddr);
    const _ = Test.transfer_to_contract_exn(contr, Increment(1), 1mutez);
    const storage = Test.get_storage(taddr);
  } with (storage = initial_storage + 1);
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo test-ligo group=frontpage
// This continues testnew.mligo

let test2 =
  let initial_storage = 42 in
  let (taddr, _, _) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract taddr in
  let () = Test.transfer_to_contract_exn contr (Increment (1)) 1mutez in
  assert (Test.get_storage taddr = initial_storage + 1)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo test-ligo group=frontpage
// This continues testnew.religo

let test2 =
  let initial_storage = 42;
  let (taddr, _, _) = Test.originate(main, initial_storage, 0tez);
  let contr = Test.to_contract(taddr);
  let _ = Test.transfer_to_contract_exn(contr, (Increment (1)), 1mutez);
  assert (Test.get_storage(taddr) == initial_storage + 1)
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo test-ligo group=frontpage
// This continues testnew.jsligo

let _test2 = () : bool => {
  let initial_storage = 42 as int;
  let [taddr, _, _] = Test.originate(main, initial_storage, 0 as tez);
  let contr = Test.to_contract(taddr);
  let r = Test.transfer_to_contract_exn(contr, (Increment (1)), 1 as mutez);
  return (Test.get_storage(taddr) == initial_storage + 1);
}

let test2 = _test2();
```

</Syntax>

The environment assumes a source for the operations which can be set
using the function `Test.set_source : address -> unit`.


#### Testing with `test`: externally

To test the contract externally, we need to create a testing file that
is different from the file containing the contract we want to test.
This testing file has access to the additional `Test` module, it will
be interpreted, and implicitly update a global state (the tezos
context). To do that, the LIGO interpreter uses the [same library that
Tezos internally uses for
testing](https://gitlab.com/tezos/tezos/-/tree/master/src/proto_alpha/lib_protocol/test/helpers).
Here we will simulate that the contract is actually deployed to an
address, and check that the resulting storage is `42` after executing
a call to `Increment`:

> Note: the types present in the context of the testing file differ
> from the ones when writing a contract.

Code insertion are used to write code to be compiled in the context of a contract. Holding all the default types you are used to
and the ones you defined in your file (if specified).

<Syntax syntax="pascaligo">

```pascaligo test-ligo group=ex1
const testme_test = "./gitlab-pages/docs/advanced/src/testme.ligo"

const test = block {
  const init_storage = Test.compile_expression (Some(testme_test), [%pascaligo ({| (10 : int) |} : ligo_program) ]);
  const originated_contract = Test.originate_from_file(testme_test, "main", init_storage, 0tez);
  const addr = originated_contract.0;
  const param = Test.compile_expression (Some (testme_test), [%pascaligo ({| Increment(32) |} : ligo_program)]);
  const transfer_result = Test.transfer(addr, param, 0tez);
  const result = Test.get_storage_of_address(addr);
  const check = Test.compile_expression ((None : option(string)), [%pascaligo ({| (42: int) |} : ligo_program)]);
  Test.log(result);
} with (Test.michelson_equal(result, check))


```

</Syntax>
<Syntax syntax="cameligo">

```cameligo test-ligo group=ex1
let testme_test = "./gitlab-pages/docs/advanced/src/testme.mligo"

let test =
  let init_storage = Test.compile_expression (Some testme_test) [%cameligo ({| (10 : int) |} : ligo_program) ] in
  let (addr, _, _) = Test.originate_from_file testme_test "main" init_storage 0tez in
  let param = Test.compile_expression (Some testme_test) [%cameligo ({| Increment(32) |} : ligo_program)] in
  let transfer_result = Test.transfer addr param 0tez in
  let result = Test.get_storage_of_address addr in
  let check_ = Test.compile_expression (None : string option) [%cameligo ({| (42: int) |} : ligo_program)] in
  let _ = Test.log result in
  Test.michelson_equal result check_
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo test-ligo group=ex1
let testme_test = "./gitlab-pages/docs/advanced/src/testme.religo"

let test =
  let init_storage = Test.compile_expression(Some(testme_test), [%reasonligo ({| (10 : int) |} : ligo_program) ]);
  let (addr, _, _) = Test.originate_from_file(testme_test, "main", init_storage, 0tez);
  let param = Test.compile_expression((Some testme_test), [%reasonligo ({| Increment(32) |} : ligo_program)]);
  let transfer_result = Test.transfer(addr, param, 0tez);
  let result = Test.get_storage_of_address(addr);
  let check_ = Test.compile_expression((None : option(string)), [%reasonligo ({| (42: int) |} : ligo_program)]);
  let _ = Test.log(result);
  Test.michelson_equal(result, check_)
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo test-ligo group=ex1
let testme_test = "./gitlab-pages/docs/advanced/src/testme.jsligo"

let _test = (): bool => {
  let init_storage = Test.compile_expression(Some(testme_test), jsligo`10 as int` as ligo_program);
  let [addr, _, _] = Test.originate_from_file(testme_test, "main", init_storage, 0 as tez);
  let param = Test.compile_expression(Some(testme_test), jsligo`Increment(32)` as ligo_program);
  let transfer_result = Test.transfer(addr, param, 0 as tez);
  let result = Test.get_storage_of_address(addr);
  let check_ = Test.compile_expression((None() as option<string>), jsligo`42 as int` as ligo_program);
  Test.log("okay");
  return Test.michelson_equal(result, check_)
}

let test = _test();
```

</Syntax>

Notice that now we wrote the test property *inside* LIGO, but the contract being tested (written also in LIGO) is in an external file. We used the following functions:

* `Test.compile_expression` to compile an expression.

* `Test.originate_from_file` to deploy a contract from a file.

* `Test.transfer` to simulate an external call taking an address (instead of a contract).

* `Test.get_storage_of_address` to check the storage from a contract (instead of a `typed_address`).

* `Test.log` to log variables.

* `Test.michelson_equal` to check if the Michelson results are equal, as now the storage is returned in its Michelson representation from `Test.get_storage_of_address`.

[More info about the `Test` module available when using the sub-command `test`.](../reference/test.md)

#### Unit testing a function

Consider a map binding addresses to amounts and a function removing all entries in that map having an amount less to a given threshold.

<Syntax syntax="cameligo">

```cameligo group=rmv_bal
(*This is remove-balance.mligo*)
type balances = (address, tez) map

let balances_under (b:balances) (threshold:tez) : balances =
  Map.fold
    (fun ((acc, (k, v)) : balances * (address * tez)) -> if v < threshold then Map.remove k acc else acc)
    b b
```

</Syntax>

<Syntax syntax="pascaligo">

```pascaligo group=rmv_bal
(*This is remove-balance.ligo*)
type balances is map (address, tez)

function balances_under (const b : balances ; const threshold : tez) is
  block {
    const f =
      function (const x : balances * (address * tez)) is
        block {
          const (acc, (k,v)) = x ;
        } with if v < threshold then Map.remove (k, acc) else acc ;
  } with Map.fold (f, b, b)
```

</Syntax>

<Syntax syntax="reasonligo">

```reasonligo group=rmv_bal
// This is remove-balance.religo
type balances = map(address, tez);

let balances_under = ( (b, threshold) : (balances, tez) ) : balances => 
  let f = ( (acc,(k,v)) : (balances, (address, tez)) ) =>  if (v < threshold) { Map.remove (k,acc) } else {acc} ;
  Map.fold (f,b,b)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=rmv_bal
// This is remove-balance.jsligo
type balances = map <address, tez>

let balances_under = (b : balances, threshold:tez) : balances => {
  let f = (acc : balances, kv :[address , tez] ) : balances => {
    let [k,v] = kv ;
    if (v < threshold) { return Map.remove (k,acc) } else {return acc}
  };
  return Map.fold (f,b,b);
}
```

</Syntax>

Let us imagine that we want to test this function against a range of thresholds with the LIGO test framework.

<!-- I divided unit-remove-balance in multiple part of clarity -->
First, let's define a variable for the file under test and reset the state with 5 bootstrap accounts (we are going to use
the bootstrap addresses later)

<Syntax syntax="cameligo">

```cameligo test-ligo group=rmv_bal_test
let under_test : string option = Some "./gitlab-pages/docs/advanced/src/remove-balance.mligo"
let _u = Test.reset_state 5n ([] : nat list)
```

</Syntax>
<Syntax syntax="pascaligo">

```pascaligo test-ligo group=rmv_bal_test
const under_test : option (string) = Some ("./gitlab-pages/docs/advanced/src/remove-balance.ligo")
const _u = Test.reset_state (5n, (list [] : list (nat)))
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo test-ligo group=rmv_bal_test
let under_test: option(string) = (Some(("./gitlab-pages/docs/advanced/src/remove-balance.religo")));
let _u = Test.reset_state (5n, ([] : list(nat)));
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo test-ligo group=rmv_bal_test
let under_test : option <string> = Some(("./gitlab-pages/docs/advanced/src/remove-balance.jsligo")) ;
let x = Test.reset_state ( 5 as nat, list([]) as list <nat> );
```

</Syntax>

Now build the `balances` map that will serve as the input of our test.  
Because types/values living in the context of `balances_under` are not directly accessible from our unit-test code, you will need to compile bootstrap account
addresses to michelson using `Test.compile_value` and inject the resulting michelson value in the map using `Test.compile_expression_subst`.  
Note that within the code injection (e.g. `{| <code> |}`), you have access to all the types accesible from the tested file.

<Syntax syntax="cameligo">

```cameligo test-ligo group=rmv_bal_test
let bs_addr (i:int) : michelson_program =
  Test.compile_value (Test.nth_bootstrap_account i)

let balances : michelson_program =
  Test.compile_expression_subst under_test
    [%cameligo ({| Map.literal [ (( $a1 : address) , 10tz ) ; (( $a2 : address) , 100tz ) ; (( $a3 : address) , 1000tz ) ]|} : ligo_program)]
    [ ("a1", bs_addr 1) ; ("a2", bs_addr 2) ; ("a3", bs_addr 3) ]
```

</Syntax>
<Syntax syntax="pascaligo">

```pascaligo test-ligo group=rmv_bal_test
function bs_addr (const i : int) is Test.compile_value (Test.nth_bootstrap_account (i))

const balances : michelson_program =
  Test.compile_expression_subst
    ( under_test,
      [%pascaligo ({| map [ ( $a1 : address) -> 10tz ; ( $a2 : address) -> 100tz ; ( $a3 : address) -> 1000tz ] |} : ligo_program)],
      list [("a1", bs_addr (1)); ("a2", bs_addr (2)); ("a3", bs_addr (3))] )
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo test-ligo group=rmv_bal_test
let bs_addr = (i: int): michelson_program =>
  Test.compile_value (Test.nth_bootstrap_account (i));

let balances: michelson_program =
  Test.compile_expression_subst (
    under_test,
    [%reasonligo ({| Map.literal ([ (( $a1 : address) , 10tz ) , (( $a2 : address) , 100tz ) , (( $a3 : address) , 1000tz ) ])|} : ligo_program)],
    [ ("a1", bs_addr(1)), ("a2", bs_addr(2)), ("a3", bs_addr(3)) ] );
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo test-ligo group=rmv_bal_test
let bs_addr = (i : int) : michelson_program => {
  return (Test.compile_value (Test.nth_bootstrap_account (i)))
} ;

let balances : michelson_program =
  Test.compile_expression_subst (
    under_test,
    jsligo` Map.literal (list ([[ $a1 as address , 10 as mutez ] , [ $a2 as address , 100 as mutez ] , [ $a3 as address , 1000 as mutez ]])) ` as ligo_program,
    list ([ ["a1", bs_addr (1)] , ["a2", bs_addr (2)] , ["a3", bs_addr (3)] ])
  );
```

</Syntax>

In general, you can use `Test.compile_value` for simple types such as `int`; `string`; `nat`; `bytes`; `address` and `pair` which will directly compile its argument to michelson without the need of writing a LIGO code injection. Otherwise, use `Test.compile_expression_subst` or `Test.compile_expression` to compile an expression written in the same manner as in the tested file.  
  
> `Test.compile_expression_subst` will bind a new variable holding a michelson [injection](./code-injection.md) 
> for each hole (e.g. `$a1`) present in the substitution before compiling the expression.

Our simple test loop will call `balances_under` with the compiled map defined above, get the size of the resulting map and compare it to an expected value with `Test.michelson_equal`.   
The threshold - of type `nat` in the test file but of type `tez` in the tested file - also needs to be dynamically compiled from the test loop and 
injected in the function call using `Test.compile_expression_subst`.  

We also print the actual and expected sizes for good measure.

<Syntax syntax="cameligo">

```cameligo test-ligo group=rmv_bal_test
let to_tez (i:nat) : michelson_program =
  Test.compile_expression_subst (None: string option)
    [%cameligo ({| $i * 1tez |} : ligo_program)]
    [("i", Test.compile_value (i) )]

let test =
  List.iter
    (fun ((threshold , expected_size) : nat * nat) ->
      let expected_size = Test.compile_value expected_size in
      let size = 
        Test.compile_expression_subst under_test
          [%cameligo ({| Map.size (balances_under $b $threshold) |} : ligo_program)]
          [ ("b", balances) ; ("threshold", to_tez threshold)]
      in
      let () = Test.log ("expected", expected_size) in
      let () = Test.log ("actual",size) in
      assert (Test.michelson_equal size expected_size)
    )
    [(15n,2n);(130n,1n);(1200n,0n)]
```

</Syntax>
<Syntax syntax="pascaligo">

```pascaligo test-ligo group=rmv_bal_test
function to_tez (const i : nat) is
  Test.compile_expression_subst
    ( (None : option (string)),
      [%pascaligo ({| $i * 1tez |} : ligo_program)],
      list [("i", Test.compile_value (i))] )

const test =
  List.iter (
    (function (const threshold : nat ; const expected_size : nat) is
      block {
        const expected_size = Test.compile_value (expected_size);
        const size_ = Test.compile_expression_subst
          ( under_test,
            [%pascaligo ({| Map.size (balances_under ($b, $threshold)) |} : ligo_program)],
            list [("b", balances); ("threshold", to_tez (threshold))]
          );
        Test.log (("expected", expected_size));
        Test.log (("actual", size_));
      } with
        assert (Test.michelson_equal (size_, expected_size))),
    list [(15n, 2n); (130n, 1n); (1200n, 0n)])
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo test-ligo group=rmv_bal_test
let to_tez: nat => michelson_program = (i: nat): michelson_program =>
  Test.compile_expression_subst (
    None : option(string),
    [%reasonligo ({| ($i * 1tez) |} : ligo_program)],
    [ ("i", Test.compile_value(i)) ] );

let test =
  List.iter (
    (((threshold , expected_size) : (nat, nat)) => 
      let expected_size = Test.compile_value(expected_size) ;
      let size = Test.compile_expression_subst (
        under_test,
        [%reasonligo ({| Map.size (balances_under ( $b , $threshold )) |} : ligo_program) ],
        [("b", balances), ("threshold", to_tez(threshold))] );
      let _u = Test.log (("expected", expected_size)) ;
      let _u = Test.log (("actual", size)) ;
      assert ( Test.michelson_equal (size, expected_size) )),
    [ (15n, 2n), (130n, 1n), (1200n, 0n)] );
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo test-ligo group=rmv_bal_test
let to_tez = (i:nat) : michelson_program => {
  return (
    Test.compile_expression_subst (
      (None () as option<string>),
      jsligo` $i * (1 as mutez) ` as ligo_program,
      list ([ ["i", Test.compile_value (i)] ])
  )) } ;

let _test = () : unit =>
  List.iter
    ( ([threshold , expected_size] : [nat , nat]) : unit => {
      let expected_size = Test.compile_value (expected_size) ;
      let size = 
        Test.compile_expression_subst (
          under_test,
          jsligo` Map.size (balances_under ($b, $threshold)) ` as ligo_program,
          list ([ ["b", balances] , ["threshold", to_tez (threshold)]]) );
      let unit = Test.log (["expected", expected_size]) ;
      let unit = Test.log (["actual",size]) ;
      return (assert (Test.michelson_equal (size,expected_size)))
    },
    list ([ [15 as nat,2 as nat] , [130 as nat,1 as nat] , [1200 as nat,0 as nat]]) );

let test = _test();
```

</Syntax>

You can now execute the test:

<Syntax syntax="cameligo">

```shell
> ligo test gitlab-pages/docs/advanced/src/unit-remove-balance.mligo test
// Outputs:
// ("expected" , 2)
// ("actual" , 2)
// ("expected" , 1)
// ("actual" , 1)
// ("expected" , 0)
// ("actual" , 0)
// Test passed with ()
```

</Syntax>


### Testing with `interpret`

The sub-command `interpret` allows to interpret an expression in a
context initialised by a source file. The interpretation is done using
Michelson's interpreter.

Let's see how it works on an example. Suppose we write the following
contract which we want to test.

<Syntax syntax="pascaligo">

```pascaligo
// This is testme.ligo
type storage is int

type parameter is
  Increment of int
| Decrement of int
| Reset

type return is list (operation) * storage

// Two entrypoints
function add (const store : storage; const delta : int) : storage is
  store + delta
function sub (const store : storage; const delta : int) : storage is
  store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)
function main (const action : parameter; const store : storage) : return is
 ((nil : list (operation)),    // No operations
  case action of
    Increment (n) -> add (store, n)
  | Decrement (n) -> sub (store, n)
  | Reset         -> 0
  end)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
// This is testme.mligo
type storage = int

type parameter =
  Increment of int
| Decrement of int
| Reset

type return = operation list * storage

// Two entrypoints
let add (store, delta : storage * int) : storage = store + delta
let sub (store, delta : storage * int) : storage = store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)
let main (action, store : parameter * storage) : return =
 ([] : operation list),    // No operations
 (match action with
   Increment (n) -> add (store, n)
 | Decrement (n) -> sub (store, n)
 | Reset         -> 0)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
// This is testme.religo
type storage = int;

type parameter =
  Increment (int)
| Decrement (int)
| Reset;

type return = (list (operation), storage);

// Two entrypoints
let add = ((store, delta) : (storage, int)) : storage => store + delta;
let sub = ((store, delta) : (storage, int)) : storage => store - delta;

/* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. */
let main = ((action, store) : (parameter, storage)) : return => {
 (([] : list (operation)),    // No operations
 (switch (action) {
  | Increment (n) => add ((store, n))
  | Decrement (n) => sub ((store, n))
  | Reset         => 0}))
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo
// This is testme.jsligo
type storage = int;

type parameter =
  ["Increment", int]
| ["Decrement", int]
| ["Reset"];

type return_ = [list<operation>, storage];

// Two entrypoints
let add = ([store, delta]: [storage, int]): storage => store + delta;
let sub = ([store, delta]: [storage, int]): storage => store - delta;

/* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. */
let main = ([action, store]: [parameter, storage]) : return_ => {
  return [
    list([]) as list<operation>,    // No operations
    match(action, {
      Increment:(n: int) => add ([store, n]),
      Decrement:(n: int) => sub ([store, n]),
      Reset: ()          => 0})
  ]
};
```

</Syntax>

This contract keeps an integer as storage, and has three entry-points:
one for incrementing the storage, one for decrementing the storage,
and one for resetting the storage to `0`.

As a simple property, we check whether starting with an storage of
`10`, if we execute the entry-point for incrementing `32`, then we get
a resulting storage of `42`. For checking it, we can interpret the
`main` function:

<Syntax syntax="pascaligo">

```shell
ligo interpret --init-file gitlab-pages/docs/advanced/src/testing/testme.ligo "main (Increment (32), 10)"
// Outputs:
// ( LIST_EMPTY() , 42 )
```

</Syntax>
<Syntax syntax="cameligo">

```shell
ligo interpret --init-file testme.mligo "main (Increment (32), 10)"
// Outputs:
// ( LIST_EMPTY() , 42 )
```

</Syntax>
<Syntax syntax="reasonligo">

```shell
ligo interpret --init-file testme.religo "main (Increment (32), 10)"
// Outputs:
// ( LIST_EMPTY() , 42 )
```

</Syntax>
<Syntax syntax="jsligo">

```shell
ligo interpret --init-file testme.jsligo "main (Increment (32), 10)"
// Outputs:
// ( LIST_EMPTY() , 42 )
```

</Syntax>

With the argument `--init-file` we pass the contract we want to test,
and the sub-command requires also the expression to evaluate in that
context, in this case, a call to our contract (`main`) with parameter
`Increment (32)` and storage `10`. As a result, we can check that the
resulting storage is `42` (the second component of the pair), and
there are no further operations to execute (the first component).

We can tune certain parameters of the execution by passing them as
arguments:

```
--amount=AMOUNT (absent=0)
    AMOUNT is the amount the Michelson interpreter will use for the
    transaction.
--balance=BALANCE (absent=0)
    BALANCE is the balance the Michelson interpreter will use for the
    contract balance.
--now=NOW
    NOW is the NOW value the Michelson interpreter will use
    (e.g. '2000-01-01T10:10:10Z')
--sender=SENDER
    SENDER is the sender the Michelson interpreter transaction will use.
--source=SOURCE
    SOURCE is the source the Michelson interpreter transaction will use.
```
