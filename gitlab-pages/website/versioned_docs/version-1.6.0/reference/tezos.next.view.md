---
id: tezos.next.view-reference
title: view
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';



<SyntaxTitle syntax="cameligo">
val call : &#39;param &#39;return.string -&gt; &#39;param -&gt; address -&gt; &#39;return option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let call: &lt;param, return&gt;(&#95;: string) =&gt; (&#95;: param) =&gt; (&#95;: address) =&gt; option&lt;return&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `call v p a` calls the view `v` with parameter
        `param` at the contract whose address is `a`. The value returned
        is `None` if the view does not exist, or has a different type of
        parameter, or if the contract does not exist at that
        address. Otherwise, it is `Some v`, where `v` is the return value
        of the view. Note: the storage of the view is the same as when the
        execution of the contract calling the view started.

</Syntax>

<Syntax syntax="jsligo">

The call `call(v, p, a)` calls the view `v` with parameter
        `param` at the contract whose address is `a`. The value returned
        is `None()` if the view does not exist, or has a different type of
        parameter, or if the contract does not exist at that
        address. Otherwise, it is `Some(v)`, where `v` is the return value
        of the view. Note: the storage of the view is the same as when the
        execution of the contract calling the view started.

</Syntax>
