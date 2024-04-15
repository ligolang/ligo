---
id: tuple2-reference
title: tuple2
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';


Binary tuples


<SyntaxTitle syntax="cameligo">
val curry : &#39;a &#39;b &#39;c.((&#39;a * &#39;b) -&gt; &#39;c) -&gt; &#39;a -&gt; &#39;b -&gt; &#39;c
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let curry: &lt;a, b, c&gt;(&#95;: (&#95;: [a, b]) =&gt; c) =&gt; (&#95;: a) =&gt; (&#95;: b) =&gt; c
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
let uncurry: &lt;a, b, c&gt;(&#95;: (&#95;: a) =&gt; (&#95;: b) =&gt; c) =&gt; (&#95;: [a, b]) =&gt; c
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `uncurry f (x,y)` has the same value as `f x y`.

</Syntax>

<Syntax syntax="jsligo">

The call `uncurry(f,[x,y])` has the same value as `f(x)(y)`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val get1 : &#39;a &#39;b.(&#39;a * &#39;b) -&gt; &#39;a
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get1: &lt;a, b&gt;(&#95;: [a, b]) =&gt; a
</SyntaxTitle>
Projecting the first component of a pair


<SyntaxTitle syntax="cameligo">
val get2 : &#39;a &#39;b.(&#39;a * &#39;b) -&gt; &#39;b
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get2: &lt;a, b&gt;(&#95;: [a, b]) =&gt; b
</SyntaxTitle>
Projecting the second component of a pair.


<SyntaxTitle syntax="cameligo">
val swap : &#39;a &#39;b.(&#39;a * &#39;b) -&gt; (&#39;b * &#39;a)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let swap: &lt;a, b&gt;(&#95;: [a, b]) =&gt; [b, a]
</SyntaxTitle>
Swap the components of a pair.
