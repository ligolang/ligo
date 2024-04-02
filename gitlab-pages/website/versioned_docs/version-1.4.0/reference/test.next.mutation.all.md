---
id: test.next.mutation.all-reference
title: All
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';



<SyntaxTitle syntax="cameligo">
val func : &#39;a &#39;b.&#39;a -&gt; (&#39;a -&gt; &#39;b) -&gt; (&#39;b * mutation) list
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let func: &lt;a, b&gt;(&#95;: a) =&gt; (&#95;: (&#95;: a) =&gt; b) =&gt; list&lt;[b, mutation]&gt;
</SyntaxTitle>
Given a value to mutate (first argument), it will try all the
             mutations of it, passing each one to the function (second
             argument). In case no failure arises when running the function on
             a mutation, the failure and mutation involved will be added to the
             list to be returned.


<SyntaxTitle syntax="cameligo">
val from&#95;file :
  &#39;b
  &#39;p
  &#39;s.string -&gt; &#39;s -&gt; tez -&gt; (((&#39;p, &#39;s) typed&#95;address * (&#39;p, &#39;s) michelson&#95;contract * int) -&gt; &#39;b) -&gt; (&#39;b * mutation) list
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let from&#95;file:
  &lt;b, p, s&gt;(&#95;: string) =&gt; (&#95;: s) =&gt; (&#95;: tez) =&gt; (&#95;: (&#95;: [typed&#95;address&lt;p, s&gt;, michelson&#95;contract&lt;p, s&gt;, int]) =&gt; b) =&gt; list&lt;
    [b, mutation]
  &gt;
</SyntaxTitle>
Given a contract from a file (passed by filepath, entrypoint and
            views), an initial storage and balance, it will originate mutants
            of the contract and pass the result to the function (last
            argument). In case no failure arises when running the function on
            a mutation, the failure and mutation involved will be added to the
            list to be returned.


<SyntaxTitle syntax="cameligo">
val contract :
  &#39;p
  &#39;s
  &#39;b.(&#39;p, &#39;s) module&#95;contract -&gt;
  &#39;s -&gt; tez -&gt; ((&#39;p, &#39;s) typed&#95;address -&gt; (&#39;p, &#39;s) michelson&#95;contract -&gt; int -&gt; &#39;b) -&gt; (&#39;b * mutation) list
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let contract:
  &lt;p, s, b&gt;(&#95;: module&#95;contract&lt;p, s&gt;) =&gt; (&#95;: s) =&gt; (&#95;: tez) =&gt; (
    &#95;: (&#95;: typed&#95;address&lt;p, s&gt;) =&gt; (&#95;: michelson&#95;contract&lt;p, s&gt;) =&gt; (&#95;: int) =&gt; b
  ) =&gt; list&lt;[b, mutation]&gt;
</SyntaxTitle>
Given a contract as a module/namespace, an initial storage and
            balance, it will originate mutants of the contract and pass the
            result to the function (last argument). In case no failure arises
            when running the function on a mutation, the failure and mutation
            involved will be added to the list to be returned.
