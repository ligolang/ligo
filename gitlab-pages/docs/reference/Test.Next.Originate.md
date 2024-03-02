---
id: test.next.originate-reference
title: Originate
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';



<SyntaxTitle syntax="cameligo">
type (&#39;p, &#39;s) origination&#95;result = &#123;
 code : (&#39;p, &#39;s) michelson&#95;contract;
 size : int;
 taddr : (&#39;p, &#39;s) typed&#95;address
&#125;
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
type origination&#95;result&lt;p, s&gt; = &#123; code: michelson&#95;contract&lt;p, s&gt;; size: int; taddr: typed&#95;address&lt;p, s&gt; &#125;
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val contract : &#39;p &#39;s.(&#39;p, &#39;s) module&#95;contract -&gt; &#39;s -&gt; tez -&gt; (&#39;p, &#39;s) origination&#95;result
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let contract: &lt;p, s&gt;(&#95;: module&#95;contract&lt;p, s&gt;) =&gt; (&#95;: s) =&gt; (&#95;: tez) =&gt; origination&#95;result&lt;p, s&gt;
</SyntaxTitle>
Originate a contract with an entrypoint function in curried
        form, initial storage and initial balance.


<SyntaxTitle syntax="cameligo">
val from&#95;file : &#39;p &#39;s.string -&gt; &#39;s -&gt; tez -&gt; (&#39;p, &#39;s) origination&#95;result
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let from&#95;file: &lt;p, s&gt;(&#95;: string) =&gt; (&#95;: s) =&gt; (&#95;: tez) =&gt; origination&#95;result&lt;p, s&gt;
</SyntaxTitle>
Originate a contract with a path to the contract file, an
        entrypoint, and a list of views, together with an initial storage
        and an initial balance.


<SyntaxTitle syntax="cameligo">
val michelson : &#39;p &#39;s.(&#39;p, &#39;s) michelson&#95;contract -&gt; &#39;s -&gt; tez -&gt; (&#39;p, &#39;s) typed&#95;address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let michelson: &lt;p, s&gt;(&#95;: michelson&#95;contract&lt;p, s&gt;) =&gt; (&#95;: s) =&gt; (&#95;: tez) =&gt; typed&#95;address&lt;p, s&gt;
</SyntaxTitle>
Originate a contract with initial storage and initial
        balance.
