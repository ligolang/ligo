---
id: test.next.dynamic-entrypoints-reference
title: Dynamic_entrypoints
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';



<SyntaxTitle syntax="cameligo">
val storage :
  &#39;p
  &#39;s
  &#39;s2.(&#39;p, &#39;s) module&#95;contract -&gt;
  &#39;s2 -&gt;
  &#123;
   dynamic&#95;entrypoints : dynamic&#95;entrypoints;
   storage : &#39;s2
  &#125;
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let storage: &lt;p, s, s2&gt;(&#95;: module&#95;contract&lt;p, s&gt;, s: s2) =&gt; &#123; dynamic&#95;entrypoints: dynamic&#95;entrypoints; storage: s2 &#125;
</SyntaxTitle>
