---
id: test.next.mutation-reference
title: Mutation
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';



[module All](Test.Next.Mutation.All.md)


<SyntaxTitle syntax="cameligo">
val func : &#39;a &#39;b.&#39;a -&gt; (&#39;a -&gt; &#39;b) -&gt; (&#39;b * mutation) option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let func: &lt;a, b&gt;(&#95;: a) =&gt; (&#95;: (&#95;: a) =&gt; b) =&gt; option&lt;[b, mutation]&gt;
</SyntaxTitle>
Given a value to mutate (first argument), it will try all the
        mutations available of it, passing each one to the function
        (second argument). On the first case of non failure when running
        the function on a mutation, the value and mutation involved will
        be returned.


<SyntaxTitle syntax="cameligo">
val from&#95;file :
  &#39;b
  &#39;p
  &#39;s.string -&gt;
  &#39;s -&gt; tez -&gt; (((&#39;p, &#39;s) typed&#95;address * (&#39;p, &#39;s) michelson&#95;contract * int) -&gt; &#39;b) -&gt; (&#39;b * mutation) option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let from&#95;file:
  &lt;b, p, s&gt;(&#95;: string) =&gt; (&#95;: s) =&gt; (&#95;: tez) =&gt; (&#95;: (&#95;: [typed&#95;address&lt;p, s&gt;, michelson&#95;contract&lt;p, s&gt;, int]) =&gt; b) =&gt; option&lt;
    [b, mutation]
  &gt;
</SyntaxTitle>
Given a contract from a file (passed by filepath, entrypoint and
        views), an initial storage and balance, it will originate mutants
        of the contract and pass the result to the function (last
        argument). On the first case of non failure when running the
        function on a mutation, the value and mutation involved will be
        returned.


<SyntaxTitle syntax="cameligo">
val contract :
  &#39;p
  &#39;s
  &#39;b.(&#39;p, &#39;s) module&#95;contract -&gt;
  &#39;s -&gt; tez -&gt; ((&#39;p, &#39;s) typed&#95;address -&gt; (&#39;p, &#39;s) michelson&#95;contract -&gt; int -&gt; &#39;b) -&gt; (&#39;b * mutation) option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let contract:
  &lt;p, s, b&gt;(&#95;: module&#95;contract&lt;p, s&gt;) =&gt; (&#95;: s) =&gt; (&#95;: tez) =&gt; (
    &#95;: (&#95;: typed&#95;address&lt;p, s&gt;) =&gt; (&#95;: michelson&#95;contract&lt;p, s&gt;) =&gt; (&#95;: int) =&gt; b
  ) =&gt; option&lt;[b, mutation]&gt;
</SyntaxTitle>
Given a contract as a module/namespace, an initial storage and
        balance, it will originate mutants of the contract and pass the
        result to the function (last argument). On the first case of non
        failure when running the function on a mutation, the value and
        mutation involved will be returned.


<SyntaxTitle syntax="cameligo">
val value : &#39;a.nat -&gt; &#39;a -&gt; (&#39;a * mutation) option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let value: &lt;a&gt;(&#95;: nat) =&gt; (&#95;: a) =&gt; option&lt;[a, mutation]&gt;
</SyntaxTitle>
Mutates a value using a natural number as an index for the
        available mutations, returns an option for indicating whether
        mutation was successful or not.


<SyntaxTitle syntax="cameligo">
val save : string -&gt; mutation -&gt; string option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let save: (&#95;: string) =&gt; (&#95;: mutation) =&gt; option&lt;string&gt;
</SyntaxTitle>
This function reconstructs a file from a mutation (second
        argument), and saves it to a file in the directory path (first
        argument). It returns an optional string indicating the filename
        where the mutation was saved, or `None` if there was an error.
