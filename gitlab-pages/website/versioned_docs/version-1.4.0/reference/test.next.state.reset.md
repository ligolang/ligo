---
id: test.next.state.reset-reference
title: Reset
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';



<SyntaxTitle syntax="cameligo">
val add&#95;baker : (string * key) -&gt; tez option -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let add&#95;baker: (&#95;: [string, key]) =&gt; (&#95;: option&lt;tez&gt;) =&gt; unit
</SyntaxTitle>
Adds an account `(sk, pk)` as a baker. The change is only
          effective after `Test.reset_state`.


<SyntaxTitle syntax="cameligo">
val add&#95;func&#95;contract : &#39;p &#39;s.((&#39;p * &#39;s) -&gt; (operation list * &#39;s)) -&gt; &#39;s -&gt; tez -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let add&#95;func&#95;contract: &lt;p, s&gt;(&#95;: (&#95;: [p, s]) =&gt; [list&lt;operation&gt;, s]) =&gt; (&#95;: s) =&gt; (&#95;: tez) =&gt; unit
</SyntaxTitle>
Setup a bootstrap contract with an entrypoint function, initial
          storage and initial balance. Bootstrap contracts will be loaded in
          order, and they will be available only after reset.
