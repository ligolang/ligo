---
id: test-reference
title: Test
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';


The testing framework


[module PBT](Test.PBT.md)


[module Proxy_ticket](Test.Proxy_ticket.md)


[module Next](Test.Next.md)


<SyntaxTitle syntax="cameligo">
val run : &#39;a &#39;b.(&#39;a -&gt; &#39;b) -&gt; &#39;a -&gt; michelson&#95;program
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let run: &lt;a, b&gt;(&#95;: (&#95;: a) =&gt; b) =&gt; (&#95;: a) =&gt; michelson&#95;program
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.run` from `Test.Next` is encouraged for a smoother migration.

Run a function on an input, all in Michelson. More concretely:
    a) compiles the function argument to Michelson `f_mich`; b)
    compiles the value argument (which was evaluated already) to
    Michelson `v_mich`; c) runs the Michelson interpreter on the code
    `f_mich` with starting stack `[v_mich]`.


<SyntaxTitle syntax="cameligo">
val eval : &#39;a.&#39;a -&gt; michelson&#95;program
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let eval: &lt;a&gt;(&#95;: a) =&gt; michelson&#95;program
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

Compile a LIGO value to Michelson. Currently it is a renaming of
    `compile_value`.


<SyntaxTitle syntax="cameligo">
val decompile : &#39;a.michelson&#95;program -&gt; &#39;a
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let decompile: &lt;a&gt;(&#95;: michelson&#95;program) =&gt; a
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.decompile` from `Test.Next` is encouraged for a smoother migration.

Decompile a Michelson value to LIGO, following the (mandatory)
  type annotation. Note: This operation can fail at run-time, in case
  that the `michelson_program` given cannot be decompiled to something
  compatible with the annotated type.


<SyntaxTitle syntax="cameligo">
val compile&#95;value : &#39;a.&#39;a -&gt; michelson&#95;program
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let compile&#95;value: &lt;a&gt;(&#95;: a) =&gt; michelson&#95;program
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.eval` from `Test.Next` is encouraged for a smoother migration.

Compile a LIGO value to Michelson.


<SyntaxTitle syntax="cameligo">
val get&#95;total&#95;voting&#95;power : unit -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;total&#95;voting&#95;power: (&#95;: unit) =&gt; nat
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `State.get_total_voting_power` from `Test.Next` is encouraged for a smoother migration.

Returns the total voting power of all contracts. The total
    voting power coincides with the sum of the rolls count of every
    contract in the voting listings. The voting listings is calculated
    at the beginning of every voting period.


<SyntaxTitle syntax="cameligo">
val failwith : &#39;a &#39;b.&#39;a -&gt; &#39;b
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let failwith: &lt;a, b&gt;(&#95;: a) =&gt; b
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.failwith` from `Test.Next` is encouraged for a smoother migration.

Cause the testing framework to fail.


<SyntaxTitle syntax="cameligo">
val to&#95;contract : &#39;p &#39;s.(&#39;p, &#39;s) typed&#95;address -&gt; &#39;p contract
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let to&#95;contract: &lt;p, s&gt;(&#95;: typed&#95;address&lt;p, s&gt;) =&gt; contract&lt;p&gt;
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_contract` from `Test.Next` is encouraged for a smoother migration.

Gets the contract corresponding to the default entrypoint of a
    typed address: the contract parameter in the result will be the
    type of the default entrypoint (generally `'param`, but this might
    differ if `'param` includes a "default" entrypoint).


<SyntaxTitle syntax="cameligo">
val set&#95;source : address -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let set&#95;source: (&#95;: address) =&gt; unit
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_source` from `Test.Next` is encouraged for a smoother migration.

Sets the source for `Test.transfer` and `Test.originate`.


<SyntaxTitle syntax="cameligo">
val cast&#95;address : &#39;a &#39;b.address -&gt; (&#39;a, &#39;b) typed&#95;address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let cast&#95;address: &lt;a, b&gt;(&#95;: address) =&gt; typed&#95;address&lt;a, b&gt;
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Address.to_typed_address` from `Test.Next` is encouraged for a smoother migration.

This function casts an address to a typed address. You will need
    to annotate the result with the type you expect.


<SyntaxTitle syntax="cameligo">
val to&#95;address : &#39;a &#39;b.(&#39;a, &#39;b) typed&#95;address -&gt; address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let to&#95;address: &lt;a, b&gt;(&#95;: typed&#95;address&lt;a, b&gt;) =&gt; address
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.to_address` from `Test.Next` is encouraged for a smoother migration.


<SyntaxTitle syntax="cameligo">
val get&#95;storage : &#39;p &#39;s.(&#39;p, &#39;s) typed&#95;address -&gt; &#39;s
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;storage: &lt;p, s&gt;(&#95;: typed&#95;address&lt;p, s&gt;) =&gt; s
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_storage` from `Test.Next` is encouraged for a smoother migration.

Gets the storage of a typed account.


<SyntaxTitle syntax="cameligo">
val get&#95;storage&#95;of&#95;address : &#39;b.address -&gt; &#39;b
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;storage&#95;of&#95;address: &lt;b&gt;(&#95;: address) =&gt; b
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_storage` from `Test.Next` is encouraged for a smoother migration.

Gets the storage of an account in `michelson_program`.


<SyntaxTitle syntax="cameligo">
val get&#95;balance&#95;of&#95;address : address -&gt; tez
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;balance&#95;of&#95;address: (&#95;: address) =&gt; tez
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Address.get_balance` from `Test.Next` is encouraged for a smoother migration.

Gets the balance of an account (given as an address) in tez.


<SyntaxTitle syntax="cameligo">
val get&#95;balance : &#39;p &#39;s.(&#39;p, &#39;s) typed&#95;address -&gt; tez
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;balance: &lt;p, s&gt;(&#95;: typed&#95;address&lt;p, s&gt;) =&gt; tez
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_balance` from `Test.Next` is encouraged for a smoother migration.

Gets the balance of an account in tez.


<SyntaxTitle syntax="cameligo">
val print : string -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let print: (&#95;: string) =&gt; unit
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `IO.print` from `Test.Next` is encouraged for a smoother migration.

Prints an string to stdout.


<SyntaxTitle syntax="cameligo">
val eprint : string -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let eprint: (&#95;: string) =&gt; unit
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `IO.eprint` from `Test.Next` is encouraged for a smoother migration.

Prints an string to stderr.


<SyntaxTitle syntax="cameligo">
val get&#95;voting&#95;power : key&#95;hash -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;voting&#95;power: (&#95;: key&#95;hash) =&gt; nat
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `State.get_voting_power` from `Test.Next` is encouraged for a smoother migration.

Return the voting power of a given contract. This voting power
    coincides with the weight of the contract in the voting listings
    (i.e., the rolls count) which is calculated at the beginning of
    every voting period.


<SyntaxTitle syntax="cameligo">
val nth&#95;bootstrap&#95;contract : nat -&gt; address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let nth&#95;bootstrap&#95;contract: (&#95;: nat) =&gt; address
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Account.Contract.bootstrap` from `Test.Next` is encouraged for a smoother migration.

Returns the address corresponding to the nth bootstrapped
    contract.


<SyntaxTitle syntax="cameligo">
val nth&#95;bootstrap&#95;account : int -&gt; address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let nth&#95;bootstrap&#95;account: (&#95;: int) =&gt; address
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Account.address` from `Test.Next` is encouraged for a smoother migration.

Returns the address of the nth bootstrapped account.


<SyntaxTitle syntax="cameligo">
val get&#95;bootstrap&#95;account : nat -&gt; (address * key * string)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;bootstrap&#95;account: (&#95;: nat) =&gt; [address, key, string]
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Account.info` from `Test.Next` is encouraged for a smoother migration.

Returns the address, key and secret key of the nth bootstrapped
    account.


<SyntaxTitle syntax="cameligo">
val nth&#95;bootstrap&#95;typed&#95;address : &#39;a &#39;b.nat -&gt; (&#39;a, &#39;b) typed&#95;address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let nth&#95;bootstrap&#95;typed&#95;address: &lt;a, b&gt;(&#95;: nat) =&gt; typed&#95;address&lt;a, b&gt;
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Account.Contract.bootstrap_typed_address` from `Test.Next` is encouraged for a smoother migration.

Returns the typed address corresponding to the nth bootstrapped
    contract currently loaded. The types are inferred from those
    contracts loaded with `Test.bootstrap_contract` (before reset).


<SyntaxTitle syntax="cameligo">
val last&#95;originations : unit -&gt; (address, address list) map
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let last&#95;originations: (&#95;: unit) =&gt; map&lt;address, list&lt;address&gt;&gt;
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `State.last_originations` from `Test.Next` is encouraged for a smoother migration.

Returns addresses of orginated accounts in the last transfer. It
    is given in the form of a map binding the address of the source of
    the origination operation to the addresses of newly originated
    accounts.


<SyntaxTitle syntax="cameligo">
val random : &#39;a.unit -&gt; &#39;a
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let random: &lt;a&gt;(&#95;: unit) =&gt; a
</SyntaxTitle>
This function creates a random value for a chosen type.


<SyntaxTitle syntax="cameligo">
val new&#95;account : unit -&gt; (string * key)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let new&#95;account: (&#95;: unit) =&gt; [string, key]
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Account.new` from `Test.Next` is encouraged for a smoother migration.

Creates and returns secret key & public key of a new account.


<SyntaxTitle syntax="cameligo">
val bake&#95;until&#95;n&#95;cycle&#95;end : nat -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let bake&#95;until&#95;n&#95;cycle&#95;end: (&#95;: nat) =&gt; unit
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `State.bake_until` from `Test.Next` is encouraged for a smoother migration.

It bakes until a number of cycles pass, so that an account
    registered as delegate can effectively act as a baker. Note: It
    can be used in tests to manually advance time.


<SyntaxTitle syntax="cameligo">
val get&#95;time : unit -&gt; timestamp
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;time: (&#95;: unit) =&gt; timestamp
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val register&#95;delegate : key&#95;hash -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let register&#95;delegate: (&#95;: key&#95;hash) =&gt; unit
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `State.register_delegate` from `Test.Next` is encouraged for a smoother migration.

Registers a `key_hash` corresponding to an account as a delegate.


<SyntaxTitle syntax="cameligo">
val stake : key&#95;hash -&gt; tez -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let stake: (&#95;: key&#95;hash) =&gt; (&#95;: tez) =&gt; unit
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `State.stake` from `Test.Next` is encouraged for a smoother migration.


<SyntaxTitle syntax="cameligo">
val register&#95;constant : michelson&#95;program -&gt; string
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let register&#95;constant: (&#95;: michelson&#95;program) =&gt; string
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `State.register_constant` from `Test.Next` is encouraged for a smoother migration.

Registers a global constant, returns its hash as a string. See
    the documentation for global constants for an example of usage.


<SyntaxTitle syntax="cameligo">
val to&#95;typed&#95;address : &#39;a &#39;b.&#39;a contract -&gt; (&#39;a, &#39;b) typed&#95;address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let to&#95;typed&#95;address: &lt;a, b&gt;(&#95;: contract&lt;a&gt;) =&gt; typed&#95;address&lt;a, b&gt;
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.to_typed_address` from `Test.Next` is encouraged for a smoother migration.


<SyntaxTitle syntax="cameligo">
val constant&#95;to&#95;michelson&#95;program : string -&gt; michelson&#95;program
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let constant&#95;to&#95;michelson&#95;program: (&#95;: string) =&gt; michelson&#95;program
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.parse` from `Test.Next` is encouraged for a smoother migration.

Turn a constant (as a string) into a `michelson_program`. To be
    used together with `Test.register_constant`.


<SyntaxTitle syntax="cameligo">
val parse&#95;michelson : string -&gt; michelson&#95;program
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let parse&#95;michelson: (&#95;: string) =&gt; michelson&#95;program
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.parse` from `Test.Next` is encouraged for a smoother migration.

Parses Michelson (as string) into a `michelson_program`.


<SyntaxTitle syntax="cameligo">
val restore&#95;context : unit -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let restore&#95;context: (&#95;: unit) =&gt; unit
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `State.restore` from `Test.Next` is encouraged for a smoother migration.

Pops a testing framework context from the stack of contexts, and
    sets it up as the new current context. In case the stack was
    empty, the current context is kept.


<SyntaxTitle syntax="cameligo">
val save&#95;context : unit -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let save&#95;context: (&#95;: unit) =&gt; unit
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `State.save` from `Test.Next` is encouraged for a smoother migration.

Takes current testing framework context and saves it, pushing it
    into a stack of contexts.


<SyntaxTitle syntax="cameligo">
val drop&#95;context : unit -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let drop&#95;context: (&#95;: unit) =&gt; unit
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `State.drop` from `Test.Next` is encouraged for a smoother migration.

Drops a testing framework context from the stack of contexts. In
    case the stack was empty, nothing is done.


<SyntaxTitle syntax="cameligo">
val to&#95;string : &#39;a.&#39;a -&gt; string
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let to&#95;string: &lt;a&gt;(&#95;: a) =&gt; string
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `String.show` from `Test.Next` is encouraged for a smoother migration.

Converts a value to a string (same conversion as used by
    `Test.log`).


<SyntaxTitle syntax="cameligo">
val to&#95;json : &#39;a.&#39;a -&gt; string
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let to&#95;json: &lt;a&gt;(&#95;: a) =&gt; string
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `String.json` from `Test.Next` is encouraged for a smoother migration.

Converts a value to its JSON representation (as a string).


<SyntaxTitle syntax="cameligo">
val to&#95;debugger&#95;json : &#39;a.&#39;a -&gt; string
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let to&#95;debugger&#95;json: &lt;a&gt;(&#95;: a) =&gt; string
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `String.debugger_json` from `Test.Next` is encouraged for a smoother migration.


<SyntaxTitle syntax="cameligo">
val set&#95;baker&#95;policy : test&#95;baker&#95;policy -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let set&#95;baker&#95;policy: (&#95;: test&#95;baker&#95;policy) =&gt; unit
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_baker_policy` from `Test.Next` is encouraged for a smoother migration.

Forces the baking policy for `Test.transfer` and
    `Test.originate`. By default, the first bootstrapped account.


<SyntaxTitle syntax="cameligo">
val set&#95;baker : address -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let set&#95;baker: (&#95;: address) =&gt; unit
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_baker` from `Test.Next` is encouraged for a smoother migration.

Forces the baker for `Test.transfer` and `Test.originate`,
    implemented using `Test.set_baker_policy` with `By_account`. By
    default, the first bootstrapped account.


<SyntaxTitle syntax="cameligo">
val size : &#39;p &#39;s.(&#39;p, &#39;s) michelson&#95;contract -&gt; int
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let size: &lt;p, s&gt;(&#95;: michelson&#95;contract&lt;p, s&gt;) =&gt; int
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.Contract.size` from `Test.Next` is encouraged for a smoother migration.

Measures the size of a contract.


<SyntaxTitle syntax="cameligo">
val compile&#95;contract : &#39;p &#39;s.((&#39;p * &#39;s) -&gt; (operation list * &#39;s)) -&gt; (&#39;p, &#39;s) michelson&#95;contract
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let compile&#95;contract: &lt;p, s&gt;(&#95;: (&#95;: [p, s]) =&gt; [list&lt;operation&gt;, s]) =&gt; michelson&#95;contract&lt;p, s&gt;
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.Contract.compile` from `Test.Next` is encouraged for a smoother migration.

Compiles a contract from an entrypoint function.


<SyntaxTitle syntax="cameligo">
val read&#95;contract&#95;from&#95;file : &#39;p &#39;s.string -&gt; (&#39;p, &#39;s) michelson&#95;contract
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let read&#95;contract&#95;from&#95;file: &lt;p, s&gt;(&#95;: string) =&gt; michelson&#95;contract&lt;p, s&gt;
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.Contract.from_file` from `Test.Next` is encouraged for a smoother migration.

Reads a contract from a `.tz` file.


<SyntaxTitle syntax="cameligo">
val chr : nat -&gt; string option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let chr: (&#95;: nat) =&gt; option&lt;string&gt;
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `String.chr` from `Test.Next` is encouraged for a smoother migration.

String consisting of the character represented by a `nat` in the
    interval `[0, 255]`.


<SyntaxTitle syntax="cameligo">
val nl : string
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let nl: string
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `String.nl` from `Test.Next` is encouraged for a smoother migration.

String consisting of only a newline.


<SyntaxTitle syntax="cameligo">
val println : string -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let println: (&#95;: string) =&gt; unit
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `IO.println` from `Test.Next` is encouraged for a smoother migration.

Prints an string to stdout, ended with a newline.


<SyntaxTitle syntax="cameligo">
val set&#95;print&#95;values : unit -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let set&#95;print&#95;values: (&#95;: unit) =&gt; unit
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `IO.set_test_print` from `Test.Next` is encouraged for a smoother migration.

Turns on the printing of `test` prefixed values at the end of
    tests. This is the default behaviour.


<SyntaxTitle syntax="cameligo">
val unset&#95;print&#95;values : unit -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let unset&#95;print&#95;values: (&#95;: unit) =&gt; unit
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `IO.unset_test_print` from `Test.Next` is encouraged for a smoother migration.

Turns off the printing of `test` prefixed values at the end of
    tests.


<SyntaxTitle syntax="cameligo">
val get&#95;last&#95;events&#95;from : &#39;a &#39;p &#39;s.(&#39;p, &#39;s) typed&#95;address -&gt; string -&gt; &#39;a list
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;last&#95;events&#95;from: &lt;a, p, s&gt;(&#95;: typed&#95;address&lt;p, s&gt;) =&gt; (&#95;: string) =&gt; list&lt;a&gt;
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `State.last_events` from `Test.Next` is encouraged for a smoother migration.

Returns the list of all the event payloads emited with a given
    tag by a given address. Any call to this function must be
    annotated with the expected payload type.


<SyntaxTitle syntax="cameligo">
val transfer : &#39;p &#39;s.(&#39;p, &#39;s) typed&#95;address -&gt; &#39;p -&gt; tez -&gt; test&#95;exec&#95;result
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let transfer: &lt;p, s&gt;(&#95;: typed&#95;address&lt;p, s&gt;) =&gt; (&#95;: p) =&gt; (&#95;: tez) =&gt; test&#95;exec&#95;result
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer` from `Test.Next` is encouraged for a smoother migration.

Bakes a transaction by sending an amount of tez with a parameter
    from the current source to another account. Returns the amount of
    gas consumed by the execution of the contract.


<SyntaxTitle syntax="cameligo">
val transfer&#95;exn : &#39;p &#39;s.(&#39;p, &#39;s) typed&#95;address -&gt; &#39;p -&gt; tez -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let transfer&#95;exn: &lt;p, s&gt;(&#95;: typed&#95;address&lt;p, s&gt;) =&gt; (&#95;: p) =&gt; (&#95;: tez) =&gt; nat
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

Bakes a transaction by sending an amount of tez with a parameter
    from the current source to another account. Returns the amount of
    gas consumed by the execution of the contract. Similar as
    `Test.transfer`, but fails when anything goes wrong.


<SyntaxTitle syntax="cameligo">
val log : &#39;a.&#39;a -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let log: &lt;a&gt;(&#95;: a) =&gt; unit
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `IO.log` from `Test.Next` is encouraged for a smoother migration.

Logs a value.


<SyntaxTitle syntax="cameligo">
val reset&#95;state : nat -&gt; tez list -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let reset&#95;state: (&#95;: nat) =&gt; (&#95;: list&lt;tez&gt;) =&gt; unit
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset` from `Test.Next` is encouraged for a smoother migration.

Generates a number of random bootstrapped accounts with a
    default amount of `4000000` tez. The passed list can be used to
    overwrite the amount. By default, the state only has two
    bootstrapped accounts. Notice that since Ithaca, a percentage of
    an account's balance is frozen (5% in testing mode) in case the
    account can be taken to be a validator, and thus
    `Test.get_balance` can show a different amount to the one being
    set with `Test.reset_state`.


<SyntaxTitle syntax="cameligo">
val reset&#95;state&#95;at : timestamp -&gt; nat -&gt; tez list -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let reset&#95;state&#95;at: (&#95;: timestamp) =&gt; (&#95;: nat) =&gt; (&#95;: list&lt;tez&gt;) =&gt; unit
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `State.reset_at` from `Test.Next` is encouraged for a smoother migration.

Generates a number of random bootstrapped accounts with a
    default amount of `4000000` tez. The passed list can be used to
    overwrite the amount. By default, the state only has two
    bootstrapped accounts. Notice that since Ithaca, a percentage of
    an account's balance is frozen (5% in testing mode) in case the
    account can be taken to be a validator, and thus
    `Test.get_balance` can show a different amount to the one being
    set with `Test.reset_state`. It also takes a starting timestamp
    for the genesis block.


<SyntaxTitle syntax="cameligo">
val bootstrap&#95;contract : &#39;p &#39;s.((&#39;p * &#39;s) -&gt; (operation list * &#39;s)) -&gt; &#39;s -&gt; tez -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let bootstrap&#95;contract: &lt;p, s&gt;(&#95;: (&#95;: [p, s]) =&gt; [list&lt;operation&gt;, s]) =&gt; (&#95;: s) =&gt; (&#95;: tez) =&gt; unit
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `State.Reset.add_func_contract` from `Test.Next` is encouraged for a smoother migration.

Setup a bootstrap contract with an entrypoint function, initial
    storage and initial balance. Bootstrap contracts will be loaded in
    order, and they will be available only after reset.


<SyntaxTitle syntax="cameligo">
val mutate&#95;value : &#39;a.nat -&gt; &#39;a -&gt; (&#39;a * mutation) option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let mutate&#95;value: &lt;a&gt;(&#95;: nat) =&gt; (&#95;: a) =&gt; option&lt;[a, mutation]&gt;
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Mutation.value` from `Test.Next` is encouraged for a smoother migration.

Mutates a value using a natural number as an index for the
    available mutations, returns an option for indicating whether
    mutation was successful or not.


<SyntaxTitle syntax="cameligo">
val save&#95;mutation : string -&gt; mutation -&gt; string option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let save&#95;mutation: (&#95;: string) =&gt; (&#95;: mutation) =&gt; option&lt;string&gt;
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Mutation.save` from `Test.Next` is encouraged for a smoother migration.

This function reconstructs a file from a mutation (second
    argument), and saves it to a file in the directory path (first
    argument). It returns an optional string indicating the filename
    where the mutation was saved, or `None` if there was an error.


<SyntaxTitle syntax="cameligo">
val sign : string -&gt; bytes -&gt; signature
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let sign: (&#95;: string) =&gt; (&#95;: bytes) =&gt; signature
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Crypto.sign` from `Test.Next` is encouraged for a smoother migration.

Creates a signature of bytes from a string representing a secret
    key, it can be checked with `Crypto.check`.


<SyntaxTitle syntax="cameligo">
val add&#95;account : string -&gt; key -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let add&#95;account: (&#95;: string) =&gt; (&#95;: key) =&gt; unit
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Account.add` from `Test.Next` is encouraged for a smoother migration.

Adds an account specfied by secret key & public key to the test
    context.


<SyntaxTitle syntax="cameligo">
val baker&#95;account : (string * key) -&gt; tez option -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let baker&#95;account: (&#95;: [string, key]) =&gt; (&#95;: option&lt;tez&gt;) =&gt; unit
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `State.Reset.add_baker` from `Test.Next` is encouraged for a smoother migration.

Adds an account `(sk, pk)` as a baker. The change is only
    effective after `Test.reset_state`.


<SyntaxTitle syntax="cameligo">
val set&#95;big&#95;map : &#39;a &#39;b.int -&gt; (&#39;a, &#39;b) big&#95;map -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let set&#95;big&#95;map: &lt;a, b&gt;(&#95;: int) =&gt; (&#95;: big&#95;map&lt;a, b&gt;) =&gt; unit
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `State.set_big_map` from `Test.Next` is encouraged for a smoother migration.

The testing framework keeps an internal reference to the values
    corresponding to big map identifiers. This function allows to
    override the value of a particular big map identifier. It should
    not be normally needed, except in particular circumstances such as
    using custom bootstrap contracts that initialize big maps.


<SyntaxTitle syntax="cameligo">
val transfer&#95;to&#95;contract : &#39;p.&#39;p contract -&gt; &#39;p -&gt; tez -&gt; test&#95;exec&#95;result
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let transfer&#95;to&#95;contract: &lt;p&gt;(&#95;: contract&lt;p&gt;) =&gt; (&#95;: p) =&gt; (&#95;: tez) =&gt; test&#95;exec&#95;result
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer` from `Test.Next` is encouraged for a smoother migration.

Bake a transaction by sending an amount of tez with a parameter
    from the current source to a contract. Returns the amount of gas
    consumed by the execution of the contract.


<SyntaxTitle syntax="cameligo">
val transfer&#95;to&#95;contract&#95;exn : &#39;p.&#39;p contract -&gt; &#39;p -&gt; tez -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let transfer&#95;to&#95;contract&#95;exn: &lt;p&gt;(&#95;: contract&lt;p&gt;) =&gt; (&#95;: p) =&gt; (&#95;: tez) =&gt; nat
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Contract.transfer_exn` from `Test.Next` is encouraged for a smoother migration.

Bakes a transaction by sending an amount of tez with a parameter
    from the current source to a contract. Returns the amount of gas
    consumed by the execution of the contract. Similar as
    `Test.transfer_to_contract`, but fails when anything goes
    wrong.


<SyntaxTitle syntax="cameligo">
val michelson&#95;equal : michelson&#95;program -&gt; michelson&#95;program -&gt; bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let michelson&#95;equal: (&#95;: michelson&#95;program) =&gt; (&#95;: michelson&#95;program) =&gt; bool
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.eq` from `Test.Next` is encouraged for a smoother migration.

Compares two Michelson values.


<SyntaxTitle syntax="cameligo">
val to&#95;entrypoint : &#39;a &#39;b &#39;c.string -&gt; (&#39;a, &#39;b) typed&#95;address -&gt; &#39;c contract
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let to&#95;entrypoint: &lt;a, b, c&gt;(&#95;: string) =&gt; (&#95;: typed&#95;address&lt;a, b&gt;) =&gt; contract&lt;c&gt;
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Typed_address.get_entrypoint` from `Test.Next` is encouraged for a smoother migration.

Gets the contract corresponding to an entrypoint of a typed
    address: the contract parameter in the result will be the type of
    the entrypoint, it needs to be annotated, entrypoint string should
    omit the prefix "%", but if passed a string starting with "%", it
    will be removed (and a warning emitted).


<SyntaxTitle syntax="cameligo">
val storage&#95;with&#95;dynamic&#95;entrypoints :
  &#39;p
  &#39;s
  &#39;s2.(&#39;p, &#39;s) module&#95;contract -&gt;
  &#39;s2 -&gt;
  &#123;
   dynamic&#95;entrypoints : dynamic&#95;entrypoints;
   storage : &#39;s2
  &#125;
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let storage&#95;with&#95;dynamic&#95;entrypoints:
  &lt;p, s, s2&gt;(&#95;: module&#95;contract&lt;p, s&gt;, s: s2) =&gt; &#123; dynamic&#95;entrypoints: dynamic&#95;entrypoints; storage: s2 &#125;
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Dynamic_entrypoints.storage` from `Test.Next` is encouraged for a smoother migration.


<SyntaxTitle syntax="cameligo">
val originate&#95;contract : &#39;p &#39;s.(&#39;p, &#39;s) michelson&#95;contract -&gt; &#39;s -&gt; tez -&gt; (&#39;p, &#39;s) typed&#95;address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let originate&#95;contract: &lt;p, s&gt;(&#95;: michelson&#95;contract&lt;p, s&gt;) =&gt; (&#95;: s) =&gt; (&#95;: tez) =&gt; typed&#95;address&lt;p, s&gt;
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.michelson` from `Test.Next` is encouraged for a smoother migration.

Originate a contract with initial storage and initial
    balance.


<SyntaxTitle syntax="cameligo">
val compile&#95;contract&#95;with&#95;views : &#39;p &#39;s.((&#39;p * &#39;s) -&gt; (operation list * &#39;s)) -&gt; &#39;s views -&gt; (&#39;p, &#39;s) michelson&#95;contract
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let compile&#95;contract&#95;with&#95;views:
  &lt;p, s&gt;(&#95;: (&#95;: [p, s]) =&gt; [list&lt;operation&gt;, s]) =&gt; (&#95;: views&lt;s&gt;) =&gt; michelson&#95;contract&lt;p, s&gt;
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.Contract.compile_with_views` from `Test.Next` is encouraged for a smoother migration.


<SyntaxTitle syntax="cameligo">
val originate : &#39;p &#39;s.(&#39;p, &#39;s) module&#95;contract -&gt; &#39;s -&gt; tez -&gt; (&#39;p, &#39;s) origination&#95;result
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let originate: &lt;p, s&gt;(&#95;: module&#95;contract&lt;p, s&gt;) =&gt; (&#95;: s) =&gt; (&#95;: tez) =&gt; origination&#95;result&lt;p, s&gt;
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.contract` from `Test.Next` is encouraged for a smoother migration.

Originate a contract with an entrypoint function in curried
    form, initial storage and initial balance.


<SyntaxTitle syntax="cameligo">
val compile&#95;contract&#95;from&#95;file : &#39;p &#39;s.string -&gt; (&#39;p, &#39;s) michelson&#95;contract
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let compile&#95;contract&#95;from&#95;file: &lt;p, s&gt;(&#95;: string) =&gt; michelson&#95;contract&lt;p, s&gt;
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Michelson.Contract.from_file` from `Test.Next` is encouraged for a smoother migration.

Compiles a contract with a path to the contract file, an
    entrypoint, and a list of views.


<SyntaxTitle syntax="cameligo">
val originate&#95;from&#95;file : &#39;p &#39;s.string -&gt; &#39;s -&gt; tez -&gt; (&#39;p, &#39;s) origination&#95;result
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let originate&#95;from&#95;file: &lt;p, s&gt;(&#95;: string) =&gt; (&#95;: s) =&gt; (&#95;: tez) =&gt; origination&#95;result&lt;p, s&gt;
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Originate.from_file` from `Test.Next` is encouraged for a smoother migration.

Originate a contract with a path to the contract file, an
    entrypoint, and a list of views, together with an initial storage
    and an initial balance.


<SyntaxTitle syntax="cameligo">
val mutation&#95;test : &#39;a &#39;b.&#39;a -&gt; (&#39;a -&gt; &#39;b) -&gt; (&#39;b * mutation) option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let mutation&#95;test: &lt;a, b&gt;(&#95;: a) =&gt; (&#95;: (&#95;: a) =&gt; b) =&gt; option&lt;[b, mutation]&gt;
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Mutation.func` from `Test.Next` is encouraged for a smoother migration.

Given a value to mutate (first argument), it will try all the
    mutations available of it, passing each one to the function
    (second argument). On the first case of non failure when running
    the function on a mutation, the value and mutation involved will
    be returned.


<SyntaxTitle syntax="cameligo">
val mutation&#95;test&#95;all : &#39;a &#39;b.&#39;a -&gt; (&#39;a -&gt; &#39;b) -&gt; (&#39;b * mutation) list
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let mutation&#95;test&#95;all: &lt;a, b&gt;(&#95;: a) =&gt; (&#95;: (&#95;: a) =&gt; b) =&gt; list&lt;[b, mutation]&gt;
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Mutation.All.func` from `Test.Next` is encouraged for a smoother migration.

Given a value to mutate (first argument), it will try all the
    mutations of it, passing each one to the function (second
    argument). In case no failure arises when running the function on
    a mutation, the failure and mutation involved will be added to the
    list to be returned.


<SyntaxTitle syntax="cameligo">
val originate&#95;from&#95;file&#95;and&#95;mutate :
  &#39;b
  &#39;p
  &#39;s.string -&gt;
  &#39;s -&gt; tez -&gt; (((&#39;p, &#39;s) typed&#95;address * (&#39;p, &#39;s) michelson&#95;contract * int) -&gt; &#39;b) -&gt; (&#39;b * mutation) option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let originate&#95;from&#95;file&#95;and&#95;mutate:
  &lt;b, p, s&gt;(&#95;: string) =&gt; (&#95;: s) =&gt; (&#95;: tez) =&gt; (&#95;: (&#95;: [typed&#95;address&lt;p, s&gt;, michelson&#95;contract&lt;p, s&gt;, int]) =&gt; b) =&gt; option&lt;
    [b, mutation]
  &gt;
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Mutation.from_file` from `Test.Next` is encouraged for a smoother migration.

Given a contract from a file (passed by filepath, entrypoint and
    views), an initial storage and balance, it will originate mutants
    of the contract and pass the result to the function (last
    argument). On the first case of non failure when running the
    function on a mutation, the value and mutation involved will be
    returned.


<SyntaxTitle syntax="cameligo">
val originate&#95;from&#95;file&#95;and&#95;mutate&#95;all :
  &#39;b
  &#39;p
  &#39;s.string -&gt; &#39;s -&gt; tez -&gt; (((&#39;p, &#39;s) typed&#95;address * (&#39;p, &#39;s) michelson&#95;contract * int) -&gt; &#39;b) -&gt; (&#39;b * mutation) list
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let originate&#95;from&#95;file&#95;and&#95;mutate&#95;all:
  &lt;b, p, s&gt;(&#95;: string) =&gt; (&#95;: s) =&gt; (&#95;: tez) =&gt; (&#95;: (&#95;: [typed&#95;address&lt;p, s&gt;, michelson&#95;contract&lt;p, s&gt;, int]) =&gt; b) =&gt; list&lt;
    [b, mutation]
  &gt;
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Mutation.All.from_file` from `Test.Next` is encouraged for a smoother migration.

Given a contract from a file (passed by filepath, entrypoint and
    views), an initial storage and balance, it will originate mutants
    of the contract and pass the result to the function (last
    argument). In case no failure arises when running the function on
    a mutation, the failure and mutation involved will be added to the
    list to be returned.


<SyntaxTitle syntax="cameligo">
val originate&#95;module&#95;and&#95;mutate :
  &#39;p
  &#39;s
  &#39;b.(&#39;p, &#39;s) module&#95;contract -&gt;
  &#39;s -&gt; tez -&gt; ((&#39;p, &#39;s) typed&#95;address -&gt; (&#39;p, &#39;s) michelson&#95;contract -&gt; int -&gt; &#39;b) -&gt; (&#39;b * mutation) option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let originate&#95;module&#95;and&#95;mutate:
  &lt;p, s, b&gt;(&#95;: module&#95;contract&lt;p, s&gt;) =&gt; (&#95;: s) =&gt; (&#95;: tez) =&gt; (
    &#95;: (&#95;: typed&#95;address&lt;p, s&gt;) =&gt; (&#95;: michelson&#95;contract&lt;p, s&gt;) =&gt; (&#95;: int) =&gt; b
  ) =&gt; option&lt;[b, mutation]&gt;
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Mutation.contract` from `Test.Next` is encouraged for a smoother migration.

Given a contract as a module/namespace, an initial storage and
    balance, it will originate mutants of the contract and pass the
    result to the function (last argument). On the first case of non
    failure when running the function on a mutation, the value and
    mutation involved will be returned.


<SyntaxTitle syntax="cameligo">
val originate&#95;and&#95;mutate&#95;all :
  &#39;p
  &#39;s
  &#39;b.(&#39;p, &#39;s) module&#95;contract -&gt;
  &#39;s -&gt; tez -&gt; ((&#39;p, &#39;s) typed&#95;address -&gt; (&#39;p, &#39;s) michelson&#95;contract -&gt; int -&gt; &#39;b) -&gt; (&#39;b * mutation) list
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let originate&#95;and&#95;mutate&#95;all:
  &lt;p, s, b&gt;(&#95;: module&#95;contract&lt;p, s&gt;) =&gt; (&#95;: s) =&gt; (&#95;: tez) =&gt; (
    &#95;: (&#95;: typed&#95;address&lt;p, s&gt;) =&gt; (&#95;: michelson&#95;contract&lt;p, s&gt;) =&gt; (&#95;: int) =&gt; b
  ) =&gt; list&lt;[b, mutation]&gt;
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Mutation.All.contract` from `Test.Next` is encouraged for a smoother migration.

Given a contract as a module/namespace, an initial storage and
    balance, it will originate mutants of the contract and pass the
    result to the function (last argument). In case no failure arises
    when running the function on a mutation, the failure and mutation
    involved will be added to the list to be returned.


<SyntaxTitle syntax="cameligo">
val assert : bool -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let assert: (&#95;: bool) =&gt; unit
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.assert` from `Test.Next` is encouraged for a smoother migration.

<Syntax syntax="cameligo">

The call `assert cond` terminates the execution with the string
    `"failed assertion"` if, and only if, the boolean condition `cond`
    is false. The failure is handled by LIGO's testing framework and
    not by Michelson's interpreter.

</Syntax>

<Syntax syntax="jsligo">

The call `assert(cond)` terminates the execution with the string
    `"failed assertion"` if, and only if, the boolean condition `cond`
    is false. The failure is handled by LIGO's testing framework and
    not by Michelson's interpreter.

</Syntax>


<SyntaxTitle syntax="cameligo">
val assert&#95;some : &#39;a.&#39;a option -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let assert&#95;some: &lt;a&gt;(&#95;: option&lt;a&gt;) =&gt; unit
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.some` from `Test.Next` is encouraged for a smoother migration.

<Syntax syntax="cameligo">

The call `assert_some opt` terminates the execution with the
    string `"failed assert some"` if, and only if, `opt` is `None`.
    The failure is handled by LIGO's testing framework and
    not by Michelson's interpreter.

</Syntax>

<Syntax syntax="jsligo">

The call `assert_some(opt)` terminates the execution with the
    string `"failed assert some"` if, and only if, `opt` is `None()`.
    The failure is handled by LIGO's testing framework and
    not by Michelson's interpreter.

</Syntax>


<SyntaxTitle syntax="cameligo">
val assert&#95;none : &#39;a.&#39;a option -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let assert&#95;none: &lt;a&gt;(&#95;: option&lt;a&gt;) =&gt; unit
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.none` from `Test.Next` is encouraged for a smoother migration.

<Syntax syntax="cameligo">

The call `assert_none opt` terminates the execution with the string
    `"failed assert none"` if, and only if, `opt` is not `None`.
    The failure is handled by LIGO's testing framework and
    not by Michelson's interpreter.

</Syntax>

<Syntax syntax="jsligo">

The call `assert_none(opt)` terminates the execution with the string
    `"failed assert none"` if, and only if, `opt` is not `None()`.
    The failure is handled by LIGO's testing framework and
    not by Michelson's interpreter.

</Syntax>


<SyntaxTitle syntax="cameligo">
val assert&#95;with&#95;error : bool -&gt; string -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let assert&#95;with&#95;error: (b: bool, s: string) =&gt; unit
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.Error.assert` from `Test.Next` is encouraged for a smoother migration.

<Syntax syntax="cameligo">

The call `assert_with_error cond error` terminates the execution
    with the string `error` (that is, an error message) if, and only
    if, the boolean condition `cond` is false. The failure is handled
    by LIGO's testing framework and not by Michelson's interpreter.

</Syntax>

<Syntax syntax="jsligo">

The call `assert_with_error(cond, error)` terminates the execution
    with the string `error` (that is, an error message) if, and only
    if, the boolean condition `cond` is false. The failure is handled
    by LIGO's testing framework and not by Michelson's interpreter.

</Syntax>


<SyntaxTitle syntax="cameligo">
val assert&#95;some&#95;with&#95;error : &#39;a.&#39;a option -&gt; string -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let assert&#95;some&#95;with&#95;error: &lt;a&gt;(&#95;: option&lt;a&gt;) =&gt; (&#95;: string) =&gt; unit
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.Error.some` from `Test.Next` is encouraged for a smoother migration.

<Syntax syntax="cameligo">

The call `assert_some_with_error opt err` terminates the execution
    with the string `err` (that is, an error message) if, and only if,
    `opt` is `None`. The failure is handled by LIGO's testing
    framework and not by Michelson's interpreter.

</Syntax>

<Syntax syntax="jsligo">

The call `assert_some_with_error(opt, err)` terminates the
    execution with the string `err` (that is, an error message) if,
    and only if, `opt` is `None()`. The failure is handled by LIGO's
    testing framework and not by Michelson's interpreter.

</Syntax>


<SyntaxTitle syntax="cameligo">
val assert&#95;none&#95;with&#95;error : &#39;a.&#39;a option -&gt; string -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let assert&#95;none&#95;with&#95;error: &lt;a&gt;(&#95;: option&lt;a&gt;) =&gt; (&#95;: string) =&gt; unit
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Assert.Error.none` from `Test.Next` is encouraged for a smoother migration.

<Syntax syntax="cameligo">

The call `assert_none_with_error opt err` terminates the execution
    with the string `err` (that is, an error message) if, and only if,
    `opt` is an optional value different from `None`. The failure is
    handled by LIGO's testing framework and not by Michelson's
    interpreter.

</Syntax>

<Syntax syntax="jsligo">

The call `assert_none_with_error(opt, err)` terminates the
    execution with the string `err` (that is, an error message) if,
    and only if, `opt` is an optional value different from
    `None()`. The failure is handled by LIGO's testing framework and
    not by Michelson's interpreter.

</Syntax>


<SyntaxTitle syntax="cameligo">
val equal : &#39;a.&#39;a -&gt; &#39;a -&gt; bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let equal: &lt;a&gt;(&#95;: a) =&gt; (&#95;: a) =&gt; bool
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.eq` from `Test.Next` is encouraged for a smoother migration.

<Syntax syntax="cameligo">

The call `equal x y` returns `true` if, and only if, `x` and `y`
    are considered to be equal w.r.t. the order on the underlying
    type.

</Syntax>

<Syntax syntax="jsligo">

The call `equal(x, y)` returns `true` if, and only if, `x` and `y`
    are considered to be equal w.r.t. the order on the underlying
    type.

</Syntax>


<SyntaxTitle syntax="cameligo">
val not&#95;equal : &#39;a.&#39;a -&gt; &#39;a -&gt; bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let not&#95;equal: &lt;a&gt;(&#95;: a) =&gt; (&#95;: a) =&gt; bool
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.neq` from `Test.Next` is encouraged for a smoother migration.

<Syntax syntax="cameligo">

The call `not_equal x y` returns `true` if, and only if, `x` and
    `y` are not considered to be equal w.r.t. the order on the
    underlying type.

</Syntax>

<Syntax syntax="jsligo">

The call `not_equal(x, y)` returns `true` if, and only if, `x` and
    `y` are not considered to be equal w.r.t. the order on the
    underlying type.

</Syntax>


<SyntaxTitle syntax="cameligo">
val greater : &#39;a.&#39;a -&gt; &#39;a -&gt; bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let greater: &lt;a&gt;(&#95;: a) =&gt; (&#95;: a) =&gt; bool
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.gt` from `Test.Next` is encouraged for a smoother migration.

<Syntax syntax="cameligo">

The call `greater x y` returns `true` if, and only if, `x` is
    considered to be greater than `y` w.r.t. the order on the
    underlying type.

</Syntax>

<Syntax syntax="jsligo">

The call `greater(x, y)` returns `true` if, and only if, `x` is
    considered to be greater than `y` w.r.t. the order on the
    underlying type.

</Syntax>


<SyntaxTitle syntax="cameligo">
val less : &#39;a.&#39;a -&gt; &#39;a -&gt; bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let less: &lt;a&gt;(&#95;: a) =&gt; (&#95;: a) =&gt; bool
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.lt` from `Test.Next` is encouraged for a smoother migration.

<Syntax syntax="cameligo">

The call `less x y` returns `true` if, and only if, `x` is
    considered to be less than `y` w.r.t. the order on the underlying
    type.

</Syntax>

<Syntax syntax="jsligo">

The call `less(x, y)` returns `true` if, and only if, `x` is
    considered to be less than `y` w.r.t. the order on the underlying
    type.

</Syntax>


<SyntaxTitle syntax="cameligo">
val greater&#95;or&#95;equal : &#39;a.&#39;a -&gt; &#39;a -&gt; bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let greater&#95;or&#95;equal: &lt;a&gt;(&#95;: a) =&gt; (&#95;: a) =&gt; bool
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.ge` from `Test.Next` is encouraged for a smoother migration.

<Syntax syntax="cameligo">

The call `greater_or_equal x y` returns `true` if, and only if,
    `x` is considered to be greater or equal than `y` w.r.t. the order
    on the underlying type.

</Syntax>

<Syntax syntax="jsligo">

The call `greater_or_equal(x, y)` returns `true` if, and only if,
    `x` is considered to be greater or equal than `y` w.r.t. the order
    on the underlying type.

</Syntax>


<SyntaxTitle syntax="cameligo">
val less&#95;or&#95;equal : &#39;a.&#39;a -&gt; &#39;a -&gt; bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let less&#95;or&#95;equal: &lt;a&gt;(&#95;: a) =&gt; (&#95;: a) =&gt; bool
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Compare.le` from `Test.Next` is encouraged for a smoother migration.

<Syntax syntax="cameligo">

The call `less_or_equal x y` returns `true` if, and only if, `x`
    is considered to be less or equal than `y` w.r.t. the order on the
    underlying type.

</Syntax>

<Syntax syntax="jsligo">

The call `less_or_equal(x, y)` returns `true` if, and only if, `x`
    is considered to be less or equal than `y` w.r.t. the order on the
    underlying type.

</Syntax>


<SyntaxTitle syntax="cameligo">
val create&#95;chest : bytes -&gt; nat -&gt; (chest * chest&#95;key)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let create&#95;chest: (&#95;: bytes) =&gt; (&#95;: nat) =&gt; [chest, chest&#95;key]
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Timelock.create` from `Test.Next` is encouraged for a smoother migration.


<SyntaxTitle syntax="cameligo">
val create&#95;chest&#95;key : chest -&gt; nat -&gt; chest&#95;key
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let create&#95;chest&#95;key: (&#95;: chest) =&gt; (&#95;: nat) =&gt; chest&#95;key
</SyntaxTitle>
**Deprecated:** In a future version, `Test` will be replaced by `Test.Next`, and using `Timelock.create_key` from `Test.Next` is encouraged for a smoother migration.
