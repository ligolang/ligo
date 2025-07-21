---
id: test.mutation.all-reference
title: all
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';


<SyntaxTitle syntax="cameligo">
val func : &#39;a &#39;b.&#39;a -&gt; (&#39;a -&gt; &#39;b) -&gt; (&#39;b * mutation) list
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
func: &lt;a, b&gt;(_: a) =&gt; (_: (_: a) =&gt; b) =&gt; list&lt;[b, mutation]&gt;
</SyntaxTitle>

Given a value to mutate (first argument), it will try all the
mutations of it, passing each one to the function (second
argument). In case no failure arises when running the function on a
mutation, the failure and mutation involved will be added to the list
to be returned.


<SyntaxTitle syntax="cameligo">
val from_file :
  &#39;b
  &#39;p
  &#39;s.string -&gt; &#39;s -&gt; tez -&gt; (((&#39;p, &#39;s) typed_address * (&#39;p, &#39;s) michelson_contract * int) -&gt; &#39;b) -&gt; (&#39;b * mutation) list
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
from_file:
  &lt;b, p, s&gt;(_: string) =&gt; (_: s) =&gt; (_: tez) =&gt; (_: (_: [typed_address&lt;p, s&gt;, michelson_contract&lt;p, s&gt;, int]) =&gt; b) =&gt; list&lt;
    [b, mutation]
  &gt;
</SyntaxTitle>

Given a contract from a file (passed by filepath, entrypoint and
views), an initial storage and balance, it will originate mutants of
the contract and pass the result to the function (last argument). In
case no failure arises when running the function on a mutation, the
failure and mutation involved will be added to the list to be
returned.


<SyntaxTitle syntax="cameligo">
val contract :
  &#39;p
  &#39;s
  &#39;b.(&#39;p, &#39;s) module_contract -&gt;
  &#39;s -&gt; tez -&gt; ((&#39;p, &#39;s) typed_address -&gt; (&#39;p, &#39;s) michelson_contract -&gt; int -&gt; &#39;b) -&gt; (&#39;b * mutation) list
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
contract:
  &lt;p, s, b&gt;(_: module_contract&lt;p, s&gt;, storage: s, amount:
  tez, _: (_: typed_address&lt;p, s&gt;, _: michelson_contract&lt;p,
  s&gt;, _: int) =&gt; b) =&gt; list&lt;[b, mutation]&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

Given a contract as a module, an initial storage and balance, it will
originate mutants of the contract and pass the result to the function
(last argument). In case no failure arises when running the function
on a mutation, the failure and mutation involved will be added to the
list to be returned.

</Syntax>

<Syntax syntax="jsligo">

Given a contract as a namespace, an initial storage and balance, it
will originate mutants of the contract and pass the result to the
function (last argument). In case no failure arises when running the
function on a mutation, the failure and mutation involved will be
added to the list to be returned.

</Syntax>

<SyntaxTitle syntax="cameligo">
val from_file :
  &#39;p
  &#39;b
  &#39;s.string
  -> &#39;s
  -> tez
  -> ((&#39;p,&#39;s) typed_address * (&#39;p,&#39;s) michelson_contract * int -> &#39;b)
  -> (&#39;b * mutation) list
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
from_file :
  &lt;p,b,s&gt;(fn: string, s: s, t: tez,
  tester: (_ : [typed_address&lt;p,s&gt;, michelson_contract&lt;p,s&gt;, int]) =&gt; b)
  =&gt; list&lt;[b, mutation]&gt;
</SyntaxTitle>

Given a contract from a file (passed by filepath, entrypoint and
views), an initial storage and balance, it will originate mutants of
the contract and pass the result to the function (last argument). In
case no failure arises when running the function on a mutation, the
failure and mutation involved will be added to the list to be
returned.
