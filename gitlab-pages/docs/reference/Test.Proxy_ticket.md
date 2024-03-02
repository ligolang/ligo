---
id: test.proxy-ticket-reference
title: Proxy_ticket
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';



<SyntaxTitle syntax="cameligo">
type &#39;v proxy&#95;address = (&#39;v * nat * address, unit) typed&#95;address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
type proxy&#95;address&lt;v&gt; = typed&#95;address&lt;[[v, nat], address], unit&gt;
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val init&#95;transfer : &#39;vt &#39;whole&#95;p.(&#39;vt ticket -&gt; &#39;whole&#95;p) -&gt; &#39;vt proxy&#95;address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let init&#95;transfer: &lt;vt, whole&#95;p&gt;(&#95;: (&#95;: ticket&lt;vt&gt;) =&gt; whole&#95;p) =&gt; proxy&#95;address&lt;vt&gt;
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val transfer : &#39;vt.&#39;vt proxy&#95;address -&gt; (&#39;vt * nat * address) -&gt; test&#95;exec&#95;result
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let transfer: &lt;vt&gt;(&#95;: proxy&#95;address&lt;vt&gt;) =&gt; (&#95;: [[vt, nat], address]) =&gt; test&#95;exec&#95;result
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val originate :
  &#39;vt
  &#39;whole&#95;s
  &#39;vp.(&#39;vt * nat) -&gt;
  (&#39;vt ticket -&gt; &#39;whole&#95;s) -&gt; (&#39;vp -&gt; &#39;whole&#95;s -&gt; (operation list * &#39;whole&#95;s)) -&gt; (&#39;vp, &#39;whole&#95;s) typed&#95;address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let originate:
  &lt;vt, whole&#95;s, vp&gt;(&#95;: [vt, nat]) =&gt; (&#95;: (&#95;: ticket&lt;vt&gt;) =&gt; whole&#95;s) =&gt; (
    &#95;: (&#95;: vp) =&gt; (&#95;: whole&#95;s) =&gt; [list&lt;operation&gt;, whole&#95;s]
  ) =&gt; typed&#95;address&lt;vp, whole&#95;s&gt;
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val get&#95;storage : &#39;p &#39;s &#39;s2.(&#39;p, &#39;s) typed&#95;address -&gt; &#39;s2
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;storage: &lt;p, s, s2&gt;(&#95;: typed&#95;address&lt;p, s&gt;) =&gt; s2
</SyntaxTitle>
