---
id: test.next.timelock-reference
title: Timelock
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';



<SyntaxTitle syntax="cameligo">
val create : bytes -&gt; nat -&gt; (chest * chest&#95;key)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let create: (&#95;: bytes) =&gt; (&#95;: nat) =&gt; [chest, chest&#95;key]
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val create&#95;key : chest -&gt; nat -&gt; chest&#95;key
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let create&#95;key: (&#95;: chest) =&gt; (&#95;: nat) =&gt; chest&#95;key
</SyntaxTitle>
