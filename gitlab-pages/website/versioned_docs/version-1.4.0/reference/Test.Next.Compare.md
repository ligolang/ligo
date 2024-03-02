---
id: test.next.compare-reference
title: Compare
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';



<SyntaxTitle syntax="cameligo">
val eq : &#39;a.&#39;a -&gt; &#39;a -&gt; bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let eq: &lt;a&gt;(&#95;: a) =&gt; (&#95;: a) =&gt; bool
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `eq x y` returns `true` if, and only if, `x` and `y`
        are considered to be equal w.r.t. the order on the underlying
        type.

</Syntax>

<Syntax syntax="jsligo">

The call `eq(x, y)` returns `true` if, and only if, `x` and `y`
        are considered to be equal w.r.t. the order on the underlying
        type.

</Syntax>


<SyntaxTitle syntax="cameligo">
val neq : &#39;a.&#39;a -&gt; &#39;a -&gt; bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let neq: &lt;a&gt;(&#95;: a) =&gt; (&#95;: a) =&gt; bool
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `neq x y` returns `true` if, and only if, `x` and
        `y` are not considered to be equal w.r.t. the order on the
        underlying type.

</Syntax>

<Syntax syntax="jsligo">

The call `neq(x, y)` returns `true` if, and only if, `x` and
        `y` are not considered to be equal w.r.t. the order on the
        underlying type.

</Syntax>


<SyntaxTitle syntax="cameligo">
val gt : &#39;a.&#39;a -&gt; &#39;a -&gt; bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let gt: &lt;a&gt;(&#95;: a) =&gt; (&#95;: a) =&gt; bool
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `gt x y` returns `true` if, and only if, `x` is
        considered to be greater than `y` w.r.t. the order on the
        underlying type.

</Syntax>

<Syntax syntax="jsligo">

The call `gt(x, y)` returns `true` if, and only if, `x` is
        considered to be greater than `y` w.r.t. the order on the
        underlying type.

</Syntax>


<SyntaxTitle syntax="cameligo">
val lt : &#39;a.&#39;a -&gt; &#39;a -&gt; bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let lt: &lt;a&gt;(&#95;: a) =&gt; (&#95;: a) =&gt; bool
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `lt` returns `true` if, and only if, `x` is
        considered to be less than `y` w.r.t. the order on the underlying
        type.

</Syntax>

<Syntax syntax="jsligo">

The call `lt(x, y)` returns `true` if, and only if, `x` is
        considered to be less than `y` w.r.t. the order on the underlying
        type.

</Syntax>


<SyntaxTitle syntax="cameligo">
val ge : &#39;a.&#39;a -&gt; &#39;a -&gt; bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let ge: &lt;a&gt;(&#95;: a) =&gt; (&#95;: a) =&gt; bool
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `ge x y` returns `true` if, and only if,
        `x` is considered to be greater or equal than `y` w.r.t. the order
        on the underlying type.

</Syntax>

<Syntax syntax="jsligo">

The call `ge(x, y)` returns `true` if, and only if,
        `x` is considered to be greater or equal than `y` w.r.t. the order
        on the underlying type.

</Syntax>


<SyntaxTitle syntax="cameligo">
val le : &#39;a.&#39;a -&gt; &#39;a -&gt; bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let le: &lt;a&gt;(&#95;: a) =&gt; (&#95;: a) =&gt; bool
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `le x y` returns `true` if, and only if, `x`
        is considered to be less or equal than `y` w.r.t. the order on the
        underlying type.

</Syntax>

<Syntax syntax="jsligo">

The call `le(x, y)` returns `true` if, and only if, `x`
        is considered to be less or equal than `y` w.r.t. the order on the
        underlying type.

</Syntax>
