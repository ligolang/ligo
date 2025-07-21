---
id: test.state.reset-reference
title: reset
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';



<SyntaxTitle syntax="cameligo">
val add_baker : (string * key) -&gt; tez option -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
add_baker: (keys: [string, key], amount: option&lt;tez&gt;) =&gt; unit
</SyntaxTitle>

<Syntax syntax="cameligo">

Adds an account `(sk, pk)` as a baker. The change is only effective
after `Test.reset_state`.

</Syntax>

<Syntax syntax="jsligo">

Adds an account `[sk, pk]` as a baker. The change is only effective
after `Test.reset_state`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val add_func_contract : &#39;p &#39;s.((&#39;p * &#39;s) -&gt; (operation list * &#39;s)) -&gt; &#39;s -&gt; tez -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
add_func_contract: &lt;p, s&gt;(_: (_: [p, s]) =&gt;
[list&lt;operation&gt;, s], storage: s, amount: tez) =&gt; unit
</SyntaxTitle>

Setup a bootstrap contract with an entrypoint function, initial
storage and initial balance. Bootstrap contracts will be loaded in
order, and they will be available only after reset.
