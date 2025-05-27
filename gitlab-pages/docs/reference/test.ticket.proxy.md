---
id: test.ticket.proxy-reference
title: proxy
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

<SyntaxTitle syntax="cameligo">
val transfer : &#39;vt.&#39;vt proxy_address -&gt; (&#39;vt * nat) *
address -&gt; test_exec_result
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
transfer: &lt;vt&gt;(&#95;: proxy_address&lt;vt;&gt;, info:
[[vt, nat], address]) =&gt; test_exec_result
</SyntaxTitle>


<SyntaxTitle syntax="cameligo">

val originate: &#39;vt &#39;whole_s &#39;vp.&#39;vt * nat -&gt; (&#39;vt ticket -&gt; &#39;whole_s)
-&gt; (&#39;vp -&gt; &#39;whole_s -&gt; operation list * &#39;whole_s) -&gt;
(&#39;vp, &#39; whole_s) typed_address

</SyntaxTitle>

<SyntaxTitle syntax="jsligo">

originate: &lt;vt, whole_s, vp&gt;(ticket_info: [vt, nat], mk_storage:
ticket&lt;vt&gt; =&gt; whole_s), contract: ([vp, whole_s] =&gt;
[list&lt;operation&gt;, whole_s])) =&gt; typed_address&lt;vp, whole_s&gt;
[[vt, nat], address]) =&gt; test_exec_result

</SyntaxTitle>


<SyntaxTitle syntax="cameligo">

val get_storage : &#39;p &#39;s &#39;s2.(&#39;p, &#39;s) typed_address -&gt; &#39;s2

</SyntaxTitle>

<SyntaxTitle syntax="jsligo">

get_storage: &lt;p,s,s2&gt;(t: typed_address&lt;p,s&gt;) =&gt; s2

</SyntaxTitle>
