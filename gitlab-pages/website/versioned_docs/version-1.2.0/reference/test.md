---
id: test
title: Test
description: Test operations
hide_table_of_contents: true
---


import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

> Important: The `Test` module is only available inside the `ligo run test` command. See also [Testing LIGO](../advanced/testing.md).

<SyntaxTitle syntax="cameligo">
type michelson_program
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type michelson_program
</SyntaxTitle>

A type for code that is compiled to Michelson.

<SyntaxTitle syntax="cameligo">
type michelson_contract
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type michelson_contract
</SyntaxTitle>

A type for Michelson compiled contracts.

<SyntaxTitle syntax="cameligo">
type test_exec_error_balance_too_low = &#x007b; contract_balance : tez ; contract_too_low : address ; spend_request : tez &#x007d;
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type test_exec_error_balance_too_low = &#x007b; contract_balance : tez , contract_too_low : address , spend_request : tez &#x007d;
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
type test_exec_error =
  Rejected of michelson_program * address
| Balance_too_low of test_exec_error_balance_too_low
| Other of string
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type test_exec_error =
  ["Rejected", michelson_program, address]
| ["Balance_too_low", test_exec_error_balance_too_low]
| ["Other", string]
</SyntaxTitle>

A test error:
  - The `Rejected` case means the called contract or its transitive callees (identified by the address in the second constructor argument) failed with some data (first constructor argument)
  - The `Balance_too_low` case means a contract tried to push an operation but did not have enough balance.
    `contract_too_low` is the address of the contract, `contract_balance` is the actual balance of the contract and `spend_request` is the amount of tez that was required for the operation
  - The `Other` case wraps all the other possible reasons. Its argument is a string representation of the tezos_client error

<SyntaxTitle syntax="cameligo">
type test_exec_result =
  Success of nat
| Fail of test_exec_error
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type test_exec_result =
  ["Success", nat]
| ["Fail", test_exec_error]
</SyntaxTitle>

A test execution result:
 - The `Success` case means the transaction went through without an issue. Its argument represent the total amount of gas consumed by the transaction
 - The "Fail reason" case means something went wrong. Its argument encode the causes of the failure (see type `test_exec_error`)

<SyntaxTitle syntax="cameligo">
type test_baker_policy =
  | By_round of int
  | By_account of address
  | Excluding of address list
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type test_baker_policy =
    ["By_round", int]
  | ["By_account", address]
  | ["Excluding", list&lt;address&gt;]
</SyntaxTitle>

A test baking policy as used by the underlying testing helpers. The
case `By_account` is the standard one, as used by `Test.set_baker`.
Policies to select the next baker (taken from test helpers documentation):

- `By_round r` selects the baker at round `r`
- `By_account pkh` selects the first slot for baker `pkh`
- `Excluding pkhs` selects the first baker that doesn't belong to `pkhs`

<SyntaxTitle syntax="cameligo">
type ('param, 'storage) typed_address
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type typed_address &lt;&apos;param, &apos;s&gt;
</SyntaxTitle>

A type for an address of a contract with parameter `'param` and storage
`'storage`.

<SyntaxTitle syntax="cameligo">
type 's unforged_ticket = &#x007b; ticketer : address; value : 's; amount : nat &#x007d;
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
type unforged_ticket &lt;s&gt; = &#x007b; ticketer : address, value : s, amount : nat &#x007d;
</SyntaxTitle>

A type for decompiling tickets.

<SyntaxTitle syntax="cameligo">
val to_contract : ('param, 'storage) typed_address -> 'param contract
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let to_contract = (account: typed_address &lt;&apos;param, &apos;storage&gt;) => contract &lt;&apos;param&gt;
</SyntaxTitle>

Get the contract corresponding to the default entrypoint of a typed
address: the contract parameter in the result will be the type of the
default entrypoint (generally `'param`, but this might differ if
`'param` includes a "default" entrypoint).

<SyntaxTitle syntax="cameligo">
val to_entrypoint : string -> ('param, 'storage) typed_address -> 'e contract
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let to_entrypoint = (entrypoint: string, account: typed_address &lt;&apos;param, &apos;storage&gt;) => contract &lt;&apos;e&gt;
</SyntaxTitle>

Get the contract corresponding to an entrypoint of a typed address:
the contract parameter in the result will be the type of the
entrypoint, it needs to be annotated, entrypoint string should omit
the prefix "%", but if passed a string starting with "%", it will be
removed (and a warning emitted).

<SyntaxTitle syntax="cameligo">
val originate_from_file : string -> string -> string list -> michelson_program -> tez -> address * michelson_contract * int
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let originate_from_file = (filepath: string, entrypoint: string, views: list&lt;string&gt;, init: michelson_program, balance: tez) => [address, michelson_contract, int]
</SyntaxTitle>

Originate a contract with a path to the contract file, an entrypoint, and a list of views, together with an initial storage and an initial balance.

<Syntax syntax="cameligo">

```cameligo skip
let addr, contract, size =
  Test.originate_from_file testme_test "main" [] init_storage 0tez
...
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
let [addr, contract, size] = Test.originate_from_file(testme_test, "main", list([]), init_storage, 0tez);
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val compile_contract_from_file : string -> string -> string list -> michelson_contract
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let compile_contract_from_file = (filepath: string, entrypoint: string, views: list&lt;string&gt;) => michelson_contract
</SyntaxTitle>

Compiles a contract with a path to the contract file, an entrypoint, and a list of views.

<SyntaxTitle syntax="cameligo">
val originate : ('param -> 'storage -> operation list * 'storage) -> 'storage -> tez -> (('param, 'storage) typed_address * michelson_contract * int)
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let originate = (contract: (p: 'param, s: 'storage) => [list &lt;operation&gt;, &apos;storage], init: 'storage, balance: tez) => [typed_address &lt;&apos;param, &apos;storage&gt;, michelson_contract, int]
</SyntaxTitle>

Originate a contract with an entrypoint function in curried form, initial storage and initial balance.

<SyntaxTitle syntax="cameligo">
val originate_module : (('param, 'storage) module_contract) -> 'storage -> tez -> (('param, 'storage) typed_address * michelson_contract * int)
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let originate_module = (contract: module_contract&lt;&apos;param, &apos;storage&gt;, init: &apos;storage, balance: tez) => [typed_address &lt;&apos;param, &apos;storage&gt;, michelson_contract, int]
</SyntaxTitle>

Originate a contract from a module/namespace. To obtain a `module_contract` from a module, use the `contract_of` keyword.

<Syntax syntax="cameligo">

```cameligo skip
let taddr, contract, size =
  Test.originate (contract_of C) init_storage 0tez
...
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
let [taddr, contract, size] = Test.originate(contract_of(C), init_storage, 0tez);
```

</Syntax>


<SyntaxTitle syntax="cameligo">
val compile_contract : ('param * 'storage -> operation list * 'storage) -> michelson_contract
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let compile_contract = (contract: ('param, 'storage) => (list &lt;operation&gt;, &apos;storage)) => michelson_contract
</SyntaxTitle>

Compiles a contract from an entrypoint function.

<SyntaxTitle syntax="cameligo">
val read_contract_from_file : string -> michelson_contract
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let read_contract_from_file = (filepath: string) => michelson_contract
</SyntaxTitle>

Reads a contract from a `.tz` file.

<SyntaxTitle syntax="cameligo">
val originate_contract : michelson_contract -> michelson_program -> tez -> address
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let originate_contract = (contract: michelson_contract, init: michelson_program, balance: tez) => address
</SyntaxTitle>

Originate a contract with initial storage and initial balance.

<SyntaxTitle syntax="cameligo">
val size : michelson_contract -> int
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let size = (contract: michelson_contract) => int
</SyntaxTitle>

Measure the size of a contract.

<SyntaxTitle syntax="cameligo">
val set_source : address -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let set_source = (source: address) => unit
</SyntaxTitle>
Set the source for `Test.transfer` and `Test.originate`.

<SyntaxTitle syntax="cameligo">
val set_baker_policy : test_baker_policy -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let set_baker_policy = (policy: test_baker_policy) => unit
</SyntaxTitle>

Force the baking policy for `Test.transfer` and `Test.originate`. By
default, the first bootstrapped account.

<SyntaxTitle syntax="cameligo">
val set_baker : address -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let set_baker = (source: address) => unit
</SyntaxTitle>

Force the baker for `Test.transfer` and `Test.originate`, implemented
using `Test.set_baker_policy` with `By_account`. By default, the first
bootstrapped account.

<SyntaxTitle syntax="cameligo">
val transfer : address -> michelson_program -> tez -> test_exec_result
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let transfer = (addr: address, param: michelson_program, amount: tez) => test_exec_result
</SyntaxTitle>

Bake a transaction by sending an amount of tez with a parameter from
the current source to another account.  Returns the amount of gas
consumed by the execution of the contract.

<SyntaxTitle syntax="cameligo">
val transfer_exn : address -> michelson_program -> tez -> nat
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let transfer_exn = (addr: address, parameter: michelson_program, amount: tez) => nat
</SyntaxTitle>

Similar as `Test.transfer`, but fails when anything goes wrong.

<SyntaxTitle syntax="cameligo">
val transfer_to_contract : 'param contract -> 'param -> tez -> test_exec_result
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let transfer_to_contract = (addr: contract&lt;&apos;p&gt;, param: &apos;p, amount: tez) => test_exec_result
</SyntaxTitle>

Bake a transaction by sending an amount of tez with a parameter from
the current source to a contract.  Returns the amount of gas consumed
by the execution of the contract.

<SyntaxTitle syntax="cameligo">
val transfer_to_contract_exn : 'p contract -> 'p -> tez -> nat
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let transfer_to_contract_exn = (addr: contract&lt;&apos;p&gt;, parameter: &apos;p, amount: tez) => nat
</SyntaxTitle>

Similar as `Test.transfer_to_contract`, but fails when anything goes wrong.

<Syntax syntax="cameligo">

```cameligo skip
val storage_with_dynamic_entrypoints : 'contract ->
  'storage ->
  {
   dynamic_entrypoints : dynamic_entrypoints;
   storage : 'storage
  }
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
let storage_with_dynamic_entrypoints = (contract: module_contract<'param, 'storage>, 'storage) => 
  {
   dynamic_entrypoints : dynamic_entrypoints;
   storage : 'storage
  }
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val get_storage_of_address : address -> michelson_program
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let get_storage_of_address = (account: address) => michelson_program
</SyntaxTitle>

Get the storage of an account in `michelson_program`.

<SyntaxTitle syntax="cameligo">
val get_storage : ('param, 'storage) typed_address -> 'storage
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let get_storage = (account: typed_address &lt;&apos;p, &apos;s&gt;) => &apos;s
</SyntaxTitle>

Get the storage of a typed account.

<SyntaxTitle syntax="cameligo">
val get_balance : address -> tez
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let get_balance = (account: address) => tez
</SyntaxTitle>

Get the balance of an account in tez.

<SyntaxTitle syntax="cameligo">
val get_voting_power : key_hash -> nat
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let get_voting_power = (kh: key_hash) => nat
</SyntaxTitle>

Return the voting power of a given contract. This voting power
coincides with the weight of the contract in the voting listings
(i.e., the rolls count) which is calculated at the beginning of every
voting period.

<SyntaxTitle syntax="cameligo">
val get_total_voting_power : nat
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let get_total_voting_power = nat
</SyntaxTitle>

Return the total voting power of all contracts. The total voting power coincides with the sum of the rolls count of every contract in the voting listings. The voting listings is calculated at the beginning of every voting period.

<SyntaxTitle syntax="cameligo">
val michelson_equal : michelson_program -> michelson_program -> bool
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let michelson_equal = (a: michelson_program, b: michelson_program) => bool
</SyntaxTitle>

Compare two Michelson values.

<SyntaxTitle syntax="cameligo">
val log : 'a -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let log = (a: 'a) => unit
</SyntaxTitle>

Log a value.

<SyntaxTitle syntax="cameligo">
val to_string : 'a -> string
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let to_string = (a: 'a) => string
</SyntaxTitle>

Convert a value to a string (same conversion as used by `log`).

<SyntaxTitle syntax="cameligo">
val to_json : 'a -> string
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let to_json = (a: 'a) => string
</SyntaxTitle>

Convert a value to its JSON representation (as a string). A JSON
schema is available [here](values.schema.json).

<SyntaxTitle syntax="cameligo">
val print : string -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let print = (s: string) => unit
</SyntaxTitle>

Prints an string to stdout.

<SyntaxTitle syntax="cameligo">
val println : string -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let println = (s: string) => unit
</SyntaxTitle>

Prints an string to stdout, ended with a newline.

<SyntaxTitle syntax="cameligo">
val eprint : string -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let eprint = (s: string) => unit
</SyntaxTitle>

Prints an string to stderr.

<SyntaxTitle syntax="cameligo">
val nl : string
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let nl : string
</SyntaxTitle>
String consisting of only a newline.

<SyntaxTitle syntax="cameligo">
val chr : nat -> string option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let chr = (c: nat) => option&lt;string&gt;
</SyntaxTitle>

String consisting of the character represented by a `nat` in the
interval [0, 255].

<SyntaxTitle syntax="cameligo">
val reset_state : nat -> tez list -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let reset_state = (no_of_accounts: nat, amount: list&lt;tez&gt;) => unit
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

<SyntaxTitle syntax="cameligo">
val reset_state_at : timestamp -> nat -> tez list -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let reset_state_at = (initial_timestamp : timestamp, no_of_accounts: nat, amount: list&lt;tez&gt;) => unit
</SyntaxTitle>

Same as `reset_state` but accepts a timestamp which is set as the initial timestamp of the genesis block.

<SyntaxTitle syntax="cameligo">
val get_time : unit -> timestamp
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let get_time = (_u: unit) => timestamp
</SyntaxTitle>

Gets the current time (to be used in test mode).

<SyntaxTitle syntax="cameligo">
val baker_account : (string * key) -> tez option -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let baker_account = ([string, key], amount : option&lt;tez&gt;) => unit
</SyntaxTitle>

Adds an account `(sk, pk)` as a baker. The change is only effective
after `Test.reset_state`.

<SyntaxTitle syntax="cameligo">
val register_delegate : key_hash -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let register_delegate = (account : key_hash) => unit
</SyntaxTitle>

Registers a `key_hash` corresponding to an account as a delegate.

<SyntaxTitle syntax="cameligo">
val register_constant : michelson_program -> string
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let register_constant = (constant : michelson_program) => string
</SyntaxTitle>

Registers a global constant `constant`, returns its hash as a string.

See the [documentation for global constants](../advanced/global-constants.md#global-constants-in-the-testing-framework)
for an example of usage.

<SyntaxTitle syntax="cameligo">
val constant_to_michelson_program : string -> michelson_program
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let constant_to_michelson_program = (constant : string) => michelson_program
</SyntaxTitle>

Turn a constant (as a string) into a `michelson_program`. To be used
together with `Test.register_constant`.

<SyntaxTitle syntax="cameligo">
val parse_michelson : string -> michelson_program
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let parse_michelson = (constant : string) => michelson_program
</SyntaxTitle>
Parses Michelson (as string) into a `michelson_program`.

<SyntaxTitle syntax="cameligo">
val register_file_constants : string -> string list
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let register_file_constants = (filepath : string) => list&lt;string&gt;
</SyntaxTitle>
Registers the global constants listed in a JSON file. It takes a
string (file path) and returns a list of strings corresponding to the
hashes of the registered constants.

<SyntaxTitle syntax="cameligo">
val bake_until_n_cycle_end : nat -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let bake_until_n_cycle_end = (cycles : nat) => unit
</SyntaxTitle>

It bakes until a number of cycles pass, so that an account registered
as delegate can effectively act as a baker.

*Note :* It can be used in tests to [manually advance time](../faq/tezos-now-advance-time.md)

<SyntaxTitle syntax="cameligo">
val new_account : unit -> (string * key)
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let new_account = (_: unit) => (string, key)
</SyntaxTitle>

Creates and returns secret key & public key of a new account.

<SyntaxTitle syntax="cameligo">
val add_account : (string * key) -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let add_account = (sk: string, pk: key) => unit
</SyntaxTitle>

Adds an account specfied by secret key & public key to the test
context.

<SyntaxTitle syntax="cameligo">
val nth_bootstrap_account : int -> address
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let nth_bootstrap_account = (nth: int) => address
</SyntaxTitle>

Returns the address of the nth bootstrapped account.

<SyntaxTitle syntax="cameligo">
val nth_bootstrap_contract : nat -> address
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let nth_bootstrap_contract = (nth: nat) => address
</SyntaxTitle>

Returns the address corresponding to the nth bootstrapped contract.

<SyntaxTitle syntax="cameligo">
val bootstrap_contract : tez -> ('param * 'storage -> operation list * 'storage) -> 'storage -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let bootstrap_contract = (balance: tez, contract: ('param, 'storage) => (list &lt;operation&gt;, &apos;storage), init: 'storage) => unit
</SyntaxTitle>

Setup a bootstrap contract with an entrypoint function, initial
storage and initial balance. Bootstrap contracts will be loaded in
order, and they will be available only after reset.

<SyntaxTitle syntax="cameligo">
val nth_bootstrap_typed_address : int -> ('param, 'storage) typed_address
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let nth_bootstrap_typed_address = (nth: int) => typed_address &lt;&apos;p, &apos;s&gt;
</SyntaxTitle>

Returns the typed address corresponding to the nth bootstrapped
contract currently loaded. The types are inferred from those contracts
loaded with `Test.bootstrap_contract` (before reset).

<SyntaxTitle syntax="cameligo">
val get_bootstrap_account : nat -> address * key * string
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let get_bootstrap_account = (nth: nat) => [address, key, string]
</SyntaxTitle>
Returns the address, key and secret key of the nth bootstrapped account.

<SyntaxTitle syntax="cameligo">
val last_originations : unit -> (address * address list) map
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let last_originations = (_: unit) => map&lt;address , address list&gt;
</SyntaxTitle>
Returns addresses of orginated accounts in the last transfer.
It is given in the form of a map binding the address of the source of the origination operation to the addresses of newly originated accounts.

<SyntaxTitle syntax="cameligo">
val compile_value : 'a -> michelson_program
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let compile_value = (value: 'a) => michelson_program
</SyntaxTitle>
Compile a LIGO value to Michelson.

<SyntaxTitle syntax="cameligo">
val eval : 'a -> michelson_program
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let eval = (value: 'a) => michelson_program
</SyntaxTitle>
Compile a LIGO value to Michelson. Currently it is a renaming of
`compile_value`.

<SyntaxTitle syntax="cameligo">
val run : ('a -> 'b) -> 'a -> michelson_program
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let run = (func: ('a => 'b), value: 'a) => michelson_program
</SyntaxTitle>

Run a function on an input, all in Michelson. More concretely: a)
compiles the function argument to Michelson `f_mich`; b) compiles the
value argument (which was evaluated already) to Michelson `v_mich`; c)
runs the Michelson interpreter on the code `f_mich` with starting
stack `[v_mich]`.


<Syntax syntax="cameligo">

```cameligo test-ligo group=test_run
type some_r = [@layout comb] {
  one : int;
  two : nat;
  three : string;
  four : bytes;
  five : unit
}

let f = fun (x : some_r) -> x.one

let test_example =
  Test.run (fun (x : int * nat * string * bytes * unit) -> f ({ one = x.0 ; two = x.1 ; three = x.2 ; four = x.3 ; five = x.4 }))
           (1 + 3 + 2, 1n + 2n, "a" ^ "b", 0xFF00, ())
```

</Syntax>

<Syntax syntax="cameligo">

```cameligo test-ligo group=test_run
type some_r = [@layout comb] {
  one   : int;
  two   : nat;
  three : string;
  four  : bytes;
  five  : unit
}

let f = fun (x : some_r) -> x.one

let test_example =
  Test.run (fun (x : int * nat * string * bytes * unit) -> f ({ one = x.0 ; two = x.1 ; three = x.2 ; four = x.3 ; five = x.4 }))
           (1 + 3 + 2, 1n + 2n, "a" ^ "b", 0xFF00, ())
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo test-ligo group=test_run
type some_r =
  @layout("comb")
  { one : int , two : nat , three : string , four : bytes , five : unit };

let f = (x: some_r) : int => x.one;

let test_example =
  Test.run (((x : [int, nat, string, bytes, unit]) => f ({ one : x[0] , two : x[1] , three : x[2] , four : x[3] , five : x[4] })),
           [1 + 3 + 2, 1n + 2n, ("a" + "b"), 0xFF00, unit]);
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val decompile : michelson_program -> 'a
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let decompile = (value: michelson_program) => 'a
</SyntaxTitle>

Decompile a Michelson value to LIGO, following the (mandatory) type
annotation. Note: This operation can fail at run-time, in case that
the `michelson_program` given cannot be decompiled to something
compatible with the annotated type.

<SyntaxTitle syntax="cameligo">
val mutate_value : nat -> 'a -> ('a * mutation) option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let mutate_value : (index: nat, value: 'a) => option &lt;[&apos;a, mutation]&gt;
</SyntaxTitle>

Mutates a value using a natural number as an index for the available
mutations, returns an option for indicating whether mutation was
successful or not.

<SyntaxTitle syntax="cameligo">
val mutation_test : 'a -> ('a -> 'b) -> ('b * mutation) option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let mutation_test : (value: 'a, tester: ('a -> 'b)) => option &lt;[&apos;b, mutation]&gt;
</SyntaxTitle>

Given a value to mutate (first argument), it will try all the
mutations available of it, passing each one to the function (second
argument). On the first case of non failure when running the function
on a mutation, the value and mutation involved will be returned.

<SyntaxTitle syntax="cameligo">
val mutation_test_all : 'a -> ('a -> 'b) -> ('b * mutation) list
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let mutation_test_all : (value: 'a, tester: ('a -> 'b)) => list &lt;[&apos;b, mutation]&gt;
</SyntaxTitle>

Given a value to mutate (first argument), it will try all the
mutations of it, passing each one to the function (second argument).
In case no failure arises when running the function on a mutation, the
failure and mutation involved will be added to the list to be
returned.

<SyntaxTitle syntax="cameligo">
val originate_from_file_and_mutate : string -> string -> string list -> michelson_program -> tez -> (address * michelson_contract * int -> 'b) -> ('b * mutation) option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let originate_from_file_and_mutate : (filepath: string, entrypoint: string, views: list&lt;string&gt;, init: michelson_program, balance: tez, (tester: (originated_address: address, code: michelson_contract, size: int) => 'b)) => option&lt;[&apos;b, mutation]&gt;
</SyntaxTitle>

Given a contract from a file (passed by filepath, entrypoint and
views), an initial storage and balance, it will originate mutants of
the contract and pass the result to the function (last argument). On
the first case of non failure when running the function on a mutation,
the value and mutation involved will be returned.

<SyntaxTitle syntax="cameligo">
val originate_from_file_and_mutate_all : string -> string -> string list -> michelson_program -> tez -> (address * michelson_contract * int -> 'b) -> ('b * mutation) list
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let originate_from_file_and_mutate_all : (filepath: string, entrypoint: string, views: list&lt;string&gt;, init: michelson_program, balance: tez, (tester: (originated_address: address, code: michelson_contract, size: int) => 'b)) => list&lt;[&apos;b, mutation]&gt;
</SyntaxTitle>

Given a contract from a file (passed by filepath, entrypoint and
views), an initial storage and balance, it will originate mutants of
the contract and pass the result to the function (last argument). In
case no failure arises when running the function on a mutation, the
failure and mutation involved will be added to the list to be
returned.

<SyntaxTitle syntax="cameligo">
val originate_module_and_mutate : (('param, 'storage) module_contract) -> 'storage -> tez -> (('param, 'storage) typed_address -> michelson_contract -> int -> b) -> ('b * mutation) option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let originate_module_and_mutate : (contract: module_contract&lt;&apos;p, &apos;s&gt;, init: &apos;s, balance: tez, (tester: (originated_address: typed_address&lt;&apos;p, &apos;s&gt;, code: michelson_contract, size: int) => &apos;b)) => option&lt;[&apos;b, mutation]&gt;
</SyntaxTitle>

Given a contract as a module/namespace, an initial storage and
balance, it will originate mutants of the contract and pass the result
to the function (last argument). On the first case of non failure when
running the function on a mutation, the value and mutation involved
will be returned.

<SyntaxTitle syntax="cameligo">
val originate_module_and_mutate_all : (('param, 'storage) module_contract) -> 'storage -> tez -> (('param, 'storage) typed_address -> michelson_contract -> int -> b) -> ('b * mutation) list
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let originate_module_and_mutate_all : (contract: module_contract&lt;&apos;p, &apos;s&gt;, init: &apos;s, balance: tez, (tester: (originated_address: typed_address&lt;&apos;p, &apos;s&gt;, code: michelson_contract, size: int) => &apos;b)) => list&lt;[&apos;b, mutation]&gt;
</SyntaxTitle>

Given a contract as a module/namespace, an initial storage and
balance, it will originate mutants of the contract and pass the result
to the function (last argument). In case no failure arises when
running the function on a mutation, the failure and mutation involved
will be added to the list to be returned.

<SyntaxTitle syntax="cameligo">
val save_mutation : string -> mutation -> string option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let save_mutation : (path: string, mutation: mutation) => option &lt;string&gt;
</SyntaxTitle>

This function reconstructs a file from a mutation (second argument),
and saves it to a file in the directory path (first argument). It
returns an optional string indicating the filename where the mutation
was saved, or `None` if there was an error.

<SyntaxTitle syntax="cameligo">
val random : unit -> 'a
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let random : (u: unit) => 'a
</SyntaxTitle>

This function creates a random value for a chosen type.

<SyntaxTitle syntax="cameligo">
val cast_address : address -> ('param,'storage) typed_address
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let cast_address : (addr: adress) => typed_address &lt;&apos;param, &apos;storage&gt;
</SyntaxTitle>

This function casts an address to a typed address. You will need to annotate the result with the type you expect.

<SyntaxTitle syntax="cameligo">
val set_big_map : int -> ('key, 'value) big_map  -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let set_big_map: (id: &apos;int, big_map: big_map&lt;&apos;key, &apos;value&gt;) => unit
</SyntaxTitle>

The testing framework keeps an internal reference to the values
corresponding to big map identifiers. This function allows to override
the value of a particular big map identifier. It should not be
normally needed, except in particular circumstances such as using
custom bootstrap contracts that initialize big maps.

<SyntaxTitle syntax="cameligo">
val save_context : unit -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let save_context: (u: unit) => unit
</SyntaxTitle>

Takes current testing framework context and saves it, pushing it into
a stack of contexts.

<SyntaxTitle syntax="cameligo">
val restore_context : unit -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let restore_context: (u: unit) => unit
</SyntaxTitle>

Pops a testing framework context from the stack of contexts, and sets
it up as the new current context. In case the stack was empty, the
current context is kept.

<SyntaxTitle syntax="cameligo">
val drop_context : unit -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let drop_context: (u: unit) => unit
</SyntaxTitle>

Drops a testing framework context from the stack of contexts. In case
the stack was empty, nothing is done.

<SyntaxTitle syntax="cameligo">
val sign : string -> bytes -> signature
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let sign: (secret_key: string, data: bytes) => signature
</SyntaxTitle>

Creates a signature of `bytes` from a `string` representing a secret
key, it can be checked with `Crypto.check`.

<SyntaxTitle syntax="cameligo">
val set_print_values : unit -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let set_print_values = (u: unit) => unit
</SyntaxTitle>

Turns on the printing of `test` prefixed values at the end of tests. This is the default behaviour.

<SyntaxTitle syntax="cameligo">
val unset_print_values : unit -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let unset_print_values = (u: unit) => unit
</SyntaxTitle>

Turns off the printing of `test` prefixed values at the end of tests.

<SyntaxTitle syntax="cameligo">
val get_last_events_from : ('p,'s) typed_address -> string -> 'a list
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let get_last_events_from: typed_address &lt;&apos;p,&apos;s&gt; => string => list &lt;&apos;a&gt;
</SyntaxTitle>

Returns the list of all the event payloads emited with a given tag by
a given address. Any call to this function must be annotated with the
expected payload type.

### Failwith and asserts

<SyntaxTitle syntax="cameligo">
val failwith : 'a -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let failwith: (message: &apos;a) => unit
</SyntaxTitle>

Cause the testing framework to fail.

<SyntaxTitle syntax="cameligo">
val assert : bool -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let assert: (condition: bool) => unit
</SyntaxTitle>

Check if a certain condition has been met. If not the testing
framework will fail.

<SyntaxTitle syntax="cameligo">
val assert_with_error : bool -> string -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let assert_with_error: (condition: bool, message: string) => unit
</SyntaxTitle>

Check if a certain condition has been met. If not the testing
framework will fail with the string passed as message.

### Proxy_ticket

Helper functions for working with tickets in the LIGO Testing framework.

Find the complete API reference [here](./proxy_ticket.md)
