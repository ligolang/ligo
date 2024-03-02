---
id: test.next.state-reference
title: State
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';



[module Reset](Test.Next.State.Reset.md)


<SyntaxTitle syntax="cameligo">
val restore : unit -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let restore: (&#95;: unit) =&gt; unit
</SyntaxTitle>
Pops a testing framework context from the stack of contexts, and
        sets it up as the new current context. In case the stack was
        empty, the current context is kept.


<SyntaxTitle syntax="cameligo">
val save : unit -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let save: (&#95;: unit) =&gt; unit
</SyntaxTitle>
Takes current testing framework context and saves it, pushing it
        into a stack of contexts.


<SyntaxTitle syntax="cameligo">
val drop : unit -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let drop: (&#95;: unit) =&gt; unit
</SyntaxTitle>
Drops a testing framework context from the stack of contexts. In
        case the stack was empty, nothing is done.


<SyntaxTitle syntax="cameligo">
val reset : nat -&gt; tez list -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let reset: (&#95;: nat) =&gt; (&#95;: list&lt;tez&gt;) =&gt; unit
</SyntaxTitle>
Generates a number of random bootstrapped accounts with a
        default amount of `4000000` tez. The passed list can be used to
        overwrite the amount. By default, the state only has two
        bootstrapped accounts. Notice that since Ithaca, a percentage of
        an account's balance is frozen (5% in testing mode) in case the
        account can be taken to be a validator, and thus getting
        balance can show a different amount to the one being set with
        `Test.State.reset`.


<SyntaxTitle syntax="cameligo">
val reset&#95;at : timestamp -&gt; nat -&gt; tez list -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let reset&#95;at: (&#95;: timestamp) =&gt; (&#95;: nat) =&gt; (&#95;: list&lt;tez&gt;) =&gt; unit
</SyntaxTitle>
Generates a number of random bootstrapped accounts with a
        default amount of `4000000` tez. The passed list can be used to
        overwrite the amount. By default, the state only has two
        bootstrapped accounts. Notice that since Ithaca, a percentage of
        an account's balance is frozen (5% in testing mode) in case the
        account can be taken to be a validator, and thus getting
        balance can show a different amount to the one being set with
        `Test.State.reset`. It also takes a starting timestamp
        for the genesis block.


<SyntaxTitle syntax="cameligo">
val register&#95;delegate : key&#95;hash -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let register&#95;delegate: (&#95;: key&#95;hash) =&gt; unit
</SyntaxTitle>
Registers a `key_hash` corresponding to an account as a delegate.


<SyntaxTitle syntax="cameligo">
val register&#95;constant : michelson&#95;program -&gt; string
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let register&#95;constant: (&#95;: michelson&#95;program) =&gt; string
</SyntaxTitle>
Registers a global constant, returns its hash as a string. See
        the documentation for global constants for an example of usage.


<SyntaxTitle syntax="cameligo">
val set&#95;source : address -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let set&#95;source: (&#95;: address) =&gt; unit
</SyntaxTitle>
Sets the source for `Test.transfer` and `Test.originate`.


<SyntaxTitle syntax="cameligo">
val set&#95;baker&#95;policy : test&#95;baker&#95;policy -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let set&#95;baker&#95;policy: (&#95;: test&#95;baker&#95;policy) =&gt; unit
</SyntaxTitle>
Forces the baking policy for `Test.transfer` and
        `Test.originate`. By default, the first bootstrapped account.


<SyntaxTitle syntax="cameligo">
val set&#95;baker : address -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let set&#95;baker: (&#95;: address) =&gt; unit
</SyntaxTitle>
Forces the baker for `Test.transfer` and `Test.originate`,
        implemented using `Test.set_baker_policy` with `By_account`. By
        default, the first bootstrapped account.


<SyntaxTitle syntax="cameligo">
val bake&#95;until : nat -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let bake&#95;until: (&#95;: nat) =&gt; unit
</SyntaxTitle>
It bakes until a number of cycles pass, so that an account
       registered as delegate can effectively act as a baker. Note: It
       can be used in tests to manually advance time.


<SyntaxTitle syntax="cameligo">
val set&#95;big&#95;map : &#39;k &#39;v.int -&gt; (&#39;k, &#39;v) big&#95;map -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let set&#95;big&#95;map: &lt;k, v&gt;(&#95;: int) =&gt; (&#95;: big&#95;map&lt;k, v&gt;) =&gt; unit
</SyntaxTitle>
The testing framework keeps an internal reference to the values
        corresponding to big map identifiers. This function allows to
        override the value of a particular big map identifier. It should
        not be normally needed, except in particular circumstances such as
        using custom bootstrap contracts that initialize big maps.


<SyntaxTitle syntax="cameligo">
val get&#95;voting&#95;power : key&#95;hash -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;voting&#95;power: (&#95;: key&#95;hash) =&gt; nat
</SyntaxTitle>
Return the voting power of a given contract. This voting power
        coincides with the weight of the contract in the voting listings
        (i.e., the rolls count) which is calculated at the beginning of
        every voting period.


<SyntaxTitle syntax="cameligo">
val get&#95;total&#95;voting&#95;power : unit -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;total&#95;voting&#95;power: (&#95;: unit) =&gt; nat
</SyntaxTitle>
Returns the total voting power of all contracts. The total
        voting power coincides with the sum of the rolls count of every
        contract in the voting listings. The voting listings is calculated
        at the beginning of every voting period.


<SyntaxTitle syntax="cameligo">
val last&#95;originations : unit -&gt; (address, address list) map
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let last&#95;originations: (&#95;: unit) =&gt; map&lt;address, list&lt;address&gt;&gt;
</SyntaxTitle>
Returns addresses of orginated accounts in the last transfer. It
        is given in the form of a map binding the address of the source of
        the origination operation to the addresses of newly originated
        accounts.


<SyntaxTitle syntax="cameligo">
val last&#95;events : &#39;a &#39;p &#39;s.(&#39;p, &#39;s) typed&#95;address -&gt; string -&gt; &#39;a list
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let last&#95;events: &lt;a, p, s&gt;(&#95;: typed&#95;address&lt;p, s&gt;) =&gt; (&#95;: string) =&gt; list&lt;a&gt;
</SyntaxTitle>
Returns the list of all the event payloads emited with a given
        tag by a given address. Any call to this function must be
        annotated with the expected payload type.


<SyntaxTitle syntax="cameligo">
val stake : key&#95;hash -&gt; tez -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let stake: (&#95;: key&#95;hash) =&gt; (&#95;: tez) =&gt; unit
</SyntaxTitle>
