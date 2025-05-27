---
id: test.account-reference
title: account
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';


[module contract](test.account.contract.md)


<SyntaxTitle syntax="cameligo">
val address : nat -&gt; address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
address: (n: nat) =&gt; address
</SyntaxTitle>

Returns the address of the nth bootstrapped account.


<SyntaxTitle syntax="cameligo">
val alice : unit -&gt; address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
alice: (&#95;: unit) =&gt; address
</SyntaxTitle>

Returns the address of the 0th bootstrapped account.


<SyntaxTitle syntax="cameligo">
val bob : unit -&gt; address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
bob: (&#95;: unit) =&gt; address
</SyntaxTitle>

Returns the address of the 1st bootstrapped account.


<SyntaxTitle syntax="cameligo">
val carol : unit -&gt; address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
carol: (&#95;: unit) =&gt; address
</SyntaxTitle>

Returns the address of the 2nd bootstrapped account.


<SyntaxTitle syntax="cameligo">
val dan : unit -&gt; address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
dan: (&#95;: unit) =&gt; address
</SyntaxTitle>

Returns the address of the 3rd bootstrapped account.


<SyntaxTitle syntax="cameligo">
val add : string -&gt; key -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
add: (&#95;: string) =&gt; (&#95;: key) =&gt; unit
</SyntaxTitle>

Adds an account specfied by secret key & public key to the test
context.


<SyntaxTitle syntax="cameligo">
type info = &#123;
 addr : address;
 pk : key;
 sk : string
&#125;
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
type info = &#123; addr: address; pk: key; sk: string &#125;
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val info : nat -&gt; Test.Account.info
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
info: (&#95;: nat) =&gt; Test.Account.info
</SyntaxTitle>

Returns the address information of the nth bootstrapped account.


<SyntaxTitle syntax="cameligo">
val new : unit -&gt; Test.Account.info
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
new: (&#95;: unit) =&gt; Test.Account.info
</SyntaxTitle>

Creates and returns information of a new account.
