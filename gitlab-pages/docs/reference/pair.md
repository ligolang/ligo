---
id: pair-reference
title: pair
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';


<SyntaxTitle syntax="cameligo">
val curry : &#39;a &#39;b &#39;c.((&#39;a * &#39;b) -&gt; &#39;c) -&gt; &#39;a -&gt; &#39;b -&gt; &#39;c
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
curry: &lt;a, b, c&gt;(f: (pair: [a, b]) =&gt; c, x: a, y: b) =&gt; c
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `curry f x y` has the same value as `f (x,y)`.

</Syntax>

<Syntax syntax="jsligo">

The call `curry(f,x,y)` has the same value as `f(x,y)`.

</Syntax>

<SyntaxTitle syntax="cameligo">
val uncurry : &#39;a &#39;b &#39;c.(&#39;a -&gt; &#39;b -&gt; &#39;c) -&gt; (&#39;a * &#39;b) -&gt; &#39;c
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
uncurry: &lt;a, b, c&gt;(f: (&#95;: a) =&gt; (&#95;: b) =&gt; c) =&gt; (&#95;: [a, b]) =&gt; c
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `uncurry f (x,y)` has the same value as `f x y`.

</Syntax>

<Syntax syntax="jsligo">

The call `uncurry(f,[x,y])` has the same value as `f(x)(y)`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val fst : &#39;a &#39;b.(&#39;a * &#39;b) -&gt; &#39;a
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
fst: &lt;a, b&gt;(&#95;: [a, b]) =&gt; a
</SyntaxTitle>

Projecting the first component of a pair.

<SyntaxTitle syntax="cameligo">
val snd : &#39;a &#39;b.(&#39;a * &#39;b) -&gt; &#39;b
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
snd: &lt;a, b&gt;(&#95;: [a, b]) =&gt; b
</SyntaxTitle>

Projecting the second component of a pair.
