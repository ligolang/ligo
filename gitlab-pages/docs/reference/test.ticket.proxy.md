---
id: test.ticket.proxy-reference
title: proxy
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

<SyntaxTitle syntax="cameligo">
val transfer : &#39;vt.&#39;vt proxy&#95;address -&gt; (&#39;vt * nat) *
address -&gt; test&#95;exec&#95;result
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
transfer: &lt;vt&gt;(&#95;: proxy&#95;address&lt;vt;&gt;, info:
[[vt, nat], address]) =&gt; test&#95;exec&#95;result
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val originate: &#39;vt &#39;whole&#95;s &#39;vp.&#39;vt * nat -&gt; (&#39;vt ticket -&gt; &#39;whole&#95;s)
-&gt; (&#39;vp -&gt; &#39;whole&#95;s -&gt; operation list * &#39;whole&#95;s) -&gt;
(&#39;vp, &#39; whole&#95;s) typed&#95;address
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
originate: &lt;vt, whole&#95;s, vp&gt;(ticket&#95;info: [vt, nat], mk&#95;storage:
ticket&lt;vt&gt; =&gt; whole&#95;s), contract: ([vp, whole&#95;s] =&gt;
[list&lt;operation&gt;, whole&#95;s])) =&gt; typed&#95;address&lt;vp, whole&#95;s&gt;
[[vt, nat], address]) =&gt; test&#95;exec&#95;result
</SyntaxTitle>


<SyntaxTitle syntax="cameligo">
val get&#95;storage : &#39;p &#39;s &#39;s2.(&#39;p, &#39;s) typed&#95;address -&gt; &#39;s2
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
get&#95;storage: &lt;p,s,s2&gt;(t: typed&#95;address&lt;p,s&gt;) =&gt; s2
</SyntaxTitle>
