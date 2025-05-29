---
id: test.timelock-reference
title: timelock
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';



<SyntaxTitle syntax="cameligo">
val create : bytes -&gt; nat -&gt; (chest * chest_key)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
create: (_: bytes, _: nat) =&gt; [chest, chest_key]
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val create_key : chest -&gt; nat -&gt; chest_key
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
create_key: (_: chest, _: nat) =&gt; chest_key
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val verify : chest -&gt; chest_key -&gt; nat -&gt; bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
verify: (_: chest, _: chest_key, _: nat) =&gt; bool
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `verify chest chest_key n` verifies a matching between
`chest` and `chest_key` (taking into account `n`).

</Syntax>

<Syntax syntax="jsligo">

The call `verify(chest, chest_key, n)` verifies a matching between
`chest` and `chest_key` (taking into account `n`).

</Syntax>
