---
id: test-next
title: Test.Next
description: next Test module.
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

> **Note:** This module/namespace exists inside the [Test](./test.md) module/namespace.  
> To use this experimental lib :
> <SyntaxTitle syntax="cameligo"> module Test = Test.Next</SyntaxTitle>
> <SyntaxTitle syntax="jsligo"> import Test = Test.Next</SyntaxTitle>

By default functions from module `Typed_address` are accessible at
this top-level. Also the function `Originate.contract` can be accessed
directly as `originate`.

## Module `Mutation`

<SyntaxTitle syntax="cameligo">
val func : 'a -> ('a -> 'b) -> ('b * mutation) option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const func&lt;a, b&gt; : (value: a, tester: ((_: a) => b)) => option &lt;[b, mutation]&gt;
</SyntaxTitle>

Given a value to mutate (first argument), it will try all the
mutations available of it, passing each one to the function (second
argument). On the first case of non failure when running the function
on a mutation, the value and mutation involved will be returned.

Former `Test.mutation_test`.


<SyntaxTitle syntax="cameligo">
val from_file : string -> string -> string list -> michelson_program -> tez -> (address * michelson_contract * int -> 'b) -> ('b * mutation) option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const from_file&lt;b&gt; : (filepath: string, entrypoint: string, views: list&lt;string&gt;, init: michelson_program, balance: tez, (tester: (originated_address: address, code: michelson_contract, size: int) => b)) => option&lt;[b, mutation]&gt;
</SyntaxTitle>

Given a contract from a file (passed by filepath, entrypoint and
views), an initial storage and balance, it will originate mutants of
the contract and pass the result to the function (last argument). On
the first case of non failure when running the function on a mutation,
the value and mutation involved will be returned.

Former `Test.originate_from_file_and_mutate`.


<SyntaxTitle syntax="cameligo">
val contract : (('param, 'storage) module_contract) -> 'storage -> tez -> (('param, 'storage) typed_address -> michelson_contract -> int -> b) -> ('b * mutation) option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const contract&lt;p, s, b&gt; : (contract: module_contract&lt;p, s&gt;, init: s, balance: tez, (tester: (originated_address: typed_address&lt;p, s&gt;, code: michelson_contract, size: int) => b)) => option&lt;[b, mutation]&gt;
</SyntaxTitle>

Given a contract as a module/namespace, an initial storage and
balance, it will originate mutants of the contract and pass the result
to the function (last argument). On the first case of non failure when
running the function on a mutation, the value and mutation involved
will be returned.

Former `Test.originate_module_and_mutate`.


<SyntaxTitle syntax="cameligo">
val value : nat -> 'a -> ('a * mutation) option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const value&lt;a&gt; : (index: nat, value: a) => option &lt;[a, mutation]&gt;
</SyntaxTitle>

Mutates a value using a natural number as an index for the available
mutations, returns an option for indicating whether mutation was
successful or not.

Former `Test.mutate_value`.


<SyntaxTitle syntax="cameligo">
val save : string -> mutation -> string option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let save : (path: string, mutation: mutation) => option &lt;string&gt;
</SyntaxTitle>

This function reconstructs a file from a mutation (second argument),
and saves it to a file in the directory path (first argument). It
returns an optional string indicating the filename where the mutation
was saved, or `None` if there was an error.

Former `Test.save_mutation`.

### Sub-module `All`

<SyntaxTitle syntax="cameligo">
val func : 'a -> ('a -> 'b) -> ('b * mutation) list
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let func&lt;a, b&gt; : (value: a, tester: ((_: a) => b)) => list &lt;[b, mutation]&gt;
</SyntaxTitle>

Given a value to mutate (first argument), it will try all the
mutations of it, passing each one to the function (second argument).
In case no failure arises when running the function on a mutation, the
failure and mutation involved will be added to the list to be
returned.

Former `Test.mutation_test_all`.


<SyntaxTitle syntax="cameligo">
val from_file : string -> string -> string list -> michelson_program -> tez -> (address * michelson_contract * int -> 'b) -> ('b * mutation) list
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let from_file&lt;b&gt; : (filepath: string, entrypoint: string, views: list&lt;string&gt;, init: michelson_program, balance: tez, (tester: (originated_address: address, code: michelson_contract, size: int) => b)) => list&lt;[b, mutation]&gt;
</SyntaxTitle>

Given a contract from a file (passed by filepath, entrypoint and
views), an initial storage and balance, it will originate mutants of
the contract and pass the result to the function (last argument). In
case no failure arises when running the function on a mutation, the
failure and mutation involved will be added to the list to be
returned.

Former `Test.originate_from_file_and_mutate_all`.


<SyntaxTitle syntax="cameligo">
val contract : (('param, 'storage) module_contract) -> 'storage -> tez -> (('param, 'storage) typed_address -> michelson_contract -> int -> b) -> ('b * mutation) list
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let contract&lt;s, p, b&gt; : (contract: module_contract&lt;p, s&gt;, init: s, balance: tez, (tester: (originated_address: typed_address&lt;p, s&gt;, code: michelson_contract, size: int) => b)) => list&lt;[b, mutation]&gt;
</SyntaxTitle>

Given a contract as a module/namespace, an initial storage and
balance, it will originate mutants of the contract and pass the result
to the function (last argument). In case no failure arises when
running the function on a mutation, the failure and mutation involved
will be added to the list to be returned.

Former `Test.originate_module_and_mutate_all`.


## Module `State`

<SyntaxTitle syntax="cameligo">
val save : unit -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let save: (u: unit) => unit
</SyntaxTitle>

Takes current testing framework context and saves it, pushing it into
a stack of contexts.

Former `Test.save_context`.


<SyntaxTitle syntax="cameligo">
val restore : unit -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let restore: (u: unit) => unit
</SyntaxTitle>

Pops a testing framework context from the stack of contexts, and sets
it up as the new current context. In case the stack was empty, the
current context is kept.

Former `Test.restore_context`.


<SyntaxTitle syntax="cameligo">
val drop : unit -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let drop: (u: unit) => unit
</SyntaxTitle>

Drops a testing framework context from the stack of contexts. In case
the stack was empty, nothing is done.

Former `Test.drop_context`.


<SyntaxTitle syntax="cameligo">
val reset : nat -> tez list -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let reset = (no_of_accounts: nat, amount: list&lt;tez&gt;) => unit
</SyntaxTitle>
Generate a number of random bootstrapped accounts with a default
amount of 4000000 tez. The passed list can be used to overwrite the
amount. By default, the state only has two bootstrapped accounts.

Notice that since Ithaca, a percentage of an account's balance is
frozen (5% in testing mode) in case the account can be taken to be a
validator ([see
here](https://tezos.gitlab.io/alpha/consensus.html#validator-selection-staking-balance-active-stake-and-frozen-deposits)),
and thus `Test.get_balance` can show a different amount to the one
being set with `Test.reset_state`.

Former `Test.reset_state`.


<SyntaxTitle syntax="cameligo">
val reset_at : timestamp -> nat -> tez list -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let reset_at = (initial_timestamp : timestamp, no_of_accounts: nat, amount: list&lt;tez&gt;) => unit
</SyntaxTitle>

Same as `reset` but accepts a timestamp which is set as the initial timestamp of the genesis block.

Former `Test.reset_state_at`.


<SyntaxTitle syntax="cameligo">
val register_delegate : key_hash -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let register_delegate = (account : key_hash) => unit
</SyntaxTitle>

Registers a `key_hash` corresponding to an account as a delegate.

Former `Test.register_delegate`.

<SyntaxTitle syntax="cameligo">
val register_constant : michelson_program -> string
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let register_constant = (constant : michelson_program) => string
</SyntaxTitle>

Registers a global constant `constant`, returns its hash as a string.

See the [documentation for global constants](../advanced/global-constants.md#global-constants-in-the-testing-framework)
for an example of usage.

Former `Test.register_constant`.


## Module `Account`

<SyntaxTitle syntax="cameligo">
val alice : unit -> address
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let alice = (_: unit) => address
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val bob : unit -> address
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let bob = (_: unit) => address
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val carol : unit -> address
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let carol = (_: unit) => address
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val dan : unit -> address
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let dan = (_: unit) => address
</SyntaxTitle>

Returns the address of the bootstrapped accounts: number 0 for
`alice`, 1 for `bob`, 2 for `carol` and 3 for `dan`.


<SyntaxTitle syntax="cameligo">
type info = &#x007b; addr : address ; pk : key ; sk : string &#x007d;
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type info = &#x007b; addr : address , pk : key , sk : string &#x007d;
</SyntaxTitle>

A type representing the data corresponding to an account.


<SyntaxTitle syntax="cameligo">
val info : nat -> info
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let info = (nth: nat) => info
</SyntaxTitle>

Returns the address, key and secret key of the nth bootstrapped account.


<SyntaxTitle syntax="cameligo">
val add : (string * key) -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let add = (sk: string, pk: key) => unit
</SyntaxTitle>

Adds an account specfied by secret key & public key to the test
context.

Former `Test.add_account`.


<SyntaxTitle syntax="cameligo">
val address : nat -> address
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let address = (nth: nat) => address
</SyntaxTitle>

Returns the address of the nth bootstrapped account.

Former `Test.nth_bootstrap_account` (but takes a `nat` instead of an `int`).


## Module `Compare`

This module is used for comparison of values. It is different from
regular comparison operators as these are expected to be more general:
things which are not comparable on Michelson can still be compared
with this primitive (in the context of the testing framework).

<SyntaxTitle syntax="cameligo">
val eq : 'a -> 'a -> bool
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const eq&lt;a&gt; : (l: a, r: a) => bool
</SyntaxTitle>

Former `Test.equal`.

<SyntaxTitle syntax="cameligo">
val neq : 'a -> 'a -> bool
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const neq&lt;a&gt; : (l: a, r: a) => bool
</SyntaxTitle>

Former `Test.not_equal`.

<SyntaxTitle syntax="cameligo">
val gt : 'a -> 'a -> bool
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const gt&lt;a&gt; : (l: a, r: a) => bool
</SyntaxTitle>

Former `Test.greater`.

<SyntaxTitle syntax="cameligo">
val lt : 'a -> 'a -> bool
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const lt&lt;a&gt; : (l: a, r: a) => bool
</SyntaxTitle>

Former `Test.less`.

<SyntaxTitle syntax="cameligo">
val ge : 'a -> 'a -> bool
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const ge&lt;a&gt; : (l: a, r: a) => bool
</SyntaxTitle>

Former `Test.greater_or_equal`.

<SyntaxTitle syntax="cameligo">
val le : 'a -> 'a -> bool
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const le&lt;a&gt; : (l: a, r: a) => bool
</SyntaxTitle>

Former `Test.less_or_equal`.


## Module `Michelson`

<SyntaxTitle syntax="cameligo">
val run : ('a -> 'b) -> 'a -> michelson_program
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const run&lt;a, b&gt; : (func: ((_: a) => b), value: a) => michelson_program
</SyntaxTitle>

Run a function on an input, all in Michelson. More concretely: a)
compiles the function argument to Michelson `f_mich`; b) compiles the
value argument (which was evaluated already) to Michelson `v_mich`; c)
runs the Michelson interpreter on the code `f_mich` with starting
stack `[v_mich]`.

Former `Test.run`.


<SyntaxTitle syntax="cameligo">
val eval : 'a -> michelson_program
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const eval&lt;a&gt; : (value: a) => michelson_program
</SyntaxTitle>
Compile a LIGO value to Michelson. Currently it is a renaming of
`compile_value`.

Former `Test.eval`.


<SyntaxTitle syntax="cameligo">
val decompile : michelson_program -> 'a
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const decompile&lt;a&gt; : (value: michelson_program) => a
</SyntaxTitle>

Decompile a Michelson value to LIGO, following the (mandatory) type
annotation. Note: This operation can fail at run-time, in case that
the `michelson_program` given cannot be decompiled to something
compatible with the annotated type.

Former `Test.decompile`.


<SyntaxTitle syntax="cameligo">
val parse : string -> michelson_program
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const parse : (constant : string) => michelson_program
</SyntaxTitle>

Parses Michelson (as string) into a `michelson_program`.


### Sub-module `Contract`

<SyntaxTitle syntax="cameligo">
val compile : ('param * 'storage -> operation list * 'storage) -> michelson_contract
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const compile&lt;p, s&gt; : (contract: (p, s) => (list &lt;operation&gt;, s)) => michelson_contract
</SyntaxTitle>

Compiles a contract from an entrypoint function.

Former `Test.compile_contract`.


<SyntaxTitle syntax="cameligo">
val size : michelson_contract -> int
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const size : (contract: michelson_contract) => int
</SyntaxTitle>

Measure the size of a contract.

Former `Test.size`.


<SyntaxTitle syntax="cameligo">
val from_file : string -> string -> string list -> michelson_contract
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const from_file : (filepath: string, entrypoint: string, views: list&lt;string&gt;) => michelson_contract
</SyntaxTitle>

Compiles a contract with a path to the contract file, an entrypoint, and a list of views.

Former `Test.compile_contract_from_file`.


## Module `IO`

<SyntaxTitle syntax="cameligo">
val print : string -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const print : (s: string) => unit
</SyntaxTitle>

Prints an string to stdout.

Former `Test.print`.


<SyntaxTitle syntax="cameligo">
val println : string -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const println : (s: string) => unit
</SyntaxTitle>

Prints an string to stdout, ended with a newline.

Former `Test.println`.


<SyntaxTitle syntax="cameligo">
val eprint : string -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const eprint : (s: string) => unit
</SyntaxTitle>

Prints an string to stderr.

Former `Test.eprint`.


<SyntaxTitle syntax="cameligo">
val log : 'a -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const log&lt;a&gt; : (a: a) => unit
</SyntaxTitle>

Log a value.

Former `Test.log`.


<SyntaxTitle syntax="cameligo">
val set_test_print : unit -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const set_test_print : (u: unit) => unit
</SyntaxTitle>

Turns on the printing of `test` prefixed values at the end of tests. This is the default behaviour.

Former `Test.set_print_values`.


<SyntaxTitle syntax="cameligo">
val unset_test_print : unit -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const unset_test_print : (u: unit) => unit
</SyntaxTitle>

Turns off the printing of `test` prefixed values at the end of tests.

Former `Test.unset_print_values`.


## Module `Assert`

<SyntaxTitle syntax="cameligo">
val assert : bool -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const assert: (condition: bool) => unit
</SyntaxTitle>

Check if a certain condition has been met. If not the testing
framework will fail.

Former `Test.assert`.


<SyntaxTitle syntax="cameligo">
val some : 'a option -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const some&lt;a&gt;: (v: option&lt;a&gt;) => unit
</SyntaxTitle>

Check if an option value is `Some`. If not the testing
framework will fail.

Former `Test.some`.


<SyntaxTitle syntax="cameligo">
val none : 'a option -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const none&lt;a&gt;: (v: option&lt;a&gt;) => unit
</SyntaxTitle>

Check if an option value is `None`. If not the testing
framework will fail.

Former `Test.none`.


### Sub-module `Error`

<SyntaxTitle syntax="cameligo">
val assert : bool -> string -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const assert: (condition: bool, message: string) => unit
</SyntaxTitle>

Check if a certain condition has been met. If not the testing
framework will fail with the string passed as message.

Former `Test.assert_with_error`.


<SyntaxTitle syntax="cameligo">
val some : 'a option -> string -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const some&lt;a&gt;: (v: option&lt;a&gt;, message: string) => unit
</SyntaxTitle>

Check if an option value is `Some`. If not the testing
framework will fail with the string passed as message.

Former `Test.some_with_error`.


<SyntaxTitle syntax="cameligo">
val none : 'a option -> string -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const none&lt;a&gt;: (v: option&lt;a&gt;, message: string) => unit
</SyntaxTitle>

Check if an option value is `None`. If not the testing
framework will fail with the string passed as message.

Former `Test.none_with_error`.


## Module `String`

<SyntaxTitle syntax="cameligo">
val chr : nat -> string option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const chr : (c: nat) => option&lt;string&gt;
</SyntaxTitle>

String consisting of the character represented by a `nat` in the
interval [0, 255].

Former `Test.chr`.


<SyntaxTitle syntax="cameligo">
val nl : string
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const nl : string
</SyntaxTitle>
String consisting of only a newline.

Former `Test.nl`.


<SyntaxTitle syntax="cameligo">
val show : 'a -> string
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const show&lt;a&gt; = (a: a) => string
</SyntaxTitle>

Convert a value to a string (same conversion as used by `log`).

Former `Test.to_string`.


<SyntaxTitle syntax="cameligo">
val json : 'a -> string
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const json&lt;a&gt; = (a: a) => string
</SyntaxTitle>

Convert a value to its JSON representation (as a string). A JSON
schema is available [here](values.schema.json).

Former `Test.to_json`.


## Module `Ticket`

This module only contains the following sub-module.

### Sub-module `Proxy`

This module is the same as the former `Test.Proxy_ticket` module.


## Module `Originate`

<SyntaxTitle syntax="cameligo">
type ('p, 's) origination_result = &#x007b; taddr : ('p, 's) typed_address ; code : ('p, 's) michelson_contract ; size : int &#x007d;
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type origination_result&lt;p, s&gt; = &#x007b; taddr : typed_address&lt;p, s&gt; , code : michelson_contract&lt;p, s&gt; , size : int &#x007d;
</SyntaxTitle>

A type representing the data corresponding to the origination of a contract.


<SyntaxTitle syntax="cameligo">
val contract : (('param, 'storage) module_contract) -> 'storage -> tez -> ('param, 'storage) origination_result
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const contract&lt;p, s&gt; : (contract: module_contract&lt;p, s&gt;, init: s, balance: tez) => origination_result&lt;p, s&gt;
</SyntaxTitle>

Originate a contract from a module/namespace. To obtain a `module_contract` from a module, use the `contract_of` keyword.

Former `Test.originate_module`.


<SyntaxTitle syntax="cameligo">
val from_file : string -> 'storage -> tez -> ('param, 'storage) origination_result
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const from_file&lt;p, s&gt; : (filepath: string, init: s, balance: tez) => origination_result&lt;p, s&gt;
</SyntaxTitle>

Originate a contract with a path to the contract file, together with an initial storage and an initial balance.

Former `Test.originate_from_file`.


## Module `Contract`

<SyntaxTitle syntax="cameligo">
val transfer : 'param contract -> 'param -> tez -> test_exec_result
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const transfer&lt;p&gt; : (addr: contract&lt;p&gt;, param: p, amount: tez) => test_exec_result
</SyntaxTitle>

Bake a transaction by sending an amount of tez with a parameter from
the current source to a contract.  Returns the amount of gas consumed
by the execution of the contract.

Former `Test.transfer_to_contract`.


<SyntaxTitle syntax="cameligo">
val transfer_exn : 'p contract -> 'p -> tez -> nat
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const transfer_exn&lt;p&gt; : (addr: contract&lt;p&gt;, parameter: p, amount: tez) => nat
</SyntaxTitle>

Similar as `transfer`, but fails when anything goes wrong.

Former `Test.transfer_to_contract_exn`.


## Module `Typed_address`

<SyntaxTitle syntax="cameligo">
val transfer : ('param, 'storage) typed_address -> 'param -> tez -> test_exec_result
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const transfer&lt;p, s&gt; : (addr: typed_address&lt;p, s&gt;, param: p, amount: tez) => test_exec_result
</SyntaxTitle>

Bake a transaction by sending an amount of tez with a parameter from
the current source to a contract given as `typed_address`.  Returns the amount of gas consumed
by the execution of the contract.

Former `Test.transfer`.


<SyntaxTitle syntax="cameligo">
val transfer_exn : ('p, 's) typed_address -> 'p -> tez -> nat
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const transfer_exn&lt;p, s&gt; : (addr: typed_address&lt;p, s&gt;, parameter: p, amount: tez) => nat
</SyntaxTitle>

Similar as `transfer`, but fails when anything goes wrong.

Former `Test.transfer_exn`.


<SyntaxTitle syntax="cameligo">
val get_storage : ('param, 'storage) typed_address -> 'storage
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const get_storage&lt;p, s&gt; : (account: typed_address&lt;p, s&gt;) => s
</SyntaxTitle>

Get the storage of a typed address.

Former `Test.get_storage`.


## Module `Address`

<SyntaxTitle syntax="cameligo">
val get_balance : address -> tez
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const get_balance : (account: address) => tez
</SyntaxTitle>

Get the balance of an account in tez.

Former `Test.get_balance`.


<SyntaxTitle syntax="cameligo">
val to_typed_address : address -> ('param,'storage) typed_address
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const to_typed_address&lt;p, s&gt; : (addr: adress) => typed_address&lt;p, s&gt;
</SyntaxTitle>

This function casts an address to a typed address. You will need to annotate the result with the type you expect.

Former `Test.cast_address`.
