---
id: test.dynamic-entrypoints-reference
title: dynamic_entrypoints
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';


<SyntaxTitle syntax="cameligo">
val storage :
  &#39;p
  &#39;s
  &#39;s2.(&#39;p, &#39;s) module_contract -&gt;
  &#39;s2 -&gt;
  &#123;
   dynamic_entrypoints : dynamic_entrypoints;
   storage : &#39;s2
  &#125;
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
storage: &lt;p, s, s2&gt;(_: module_contract&lt;p, s&gt;, storage: s2) =&gt; &#123; dynamic_entrypoints: dynamic_entrypoints; storage: s2 &#125;
</SyntaxTitle>
