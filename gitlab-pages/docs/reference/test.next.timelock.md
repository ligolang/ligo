---
id: test.next.timelock-reference
title: timelock
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

<SyntaxTitle syntax="cameligo">
val verify : chest -&gt; chest&#95;key -&gt; nat -&gt; bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let verify: (&#95;: chest) =&gt; (&#95;: chest&#95;key) =&gt; (&#95;: nat) =&gt; bool
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `verify chest chest_key n` verifies a matching
        between `chest` and `chest_key` (taking into account `n`).

</Syntax>

<Syntax syntax="jsligo">

The call `verify(chest, chest_key, n)` verifies a matching
        between `chest` and `chest_key` (taking into account `n`).

</Syntax>
