---
id: option-reference
title: option
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';


The module of optional values

<SyntaxTitle syntax="cameligo">
val value : &#39;a.&#39;a -&gt; &#39;a option -&gt; &#39;a
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
value: &lt;a&gt;(default: a, opt: option&lt;a&gt;) =&gt; a
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `Option.value d opt` is `v` if `opt` is `Some v`, and `d`
otherwise.

</Syntax>

<Syntax syntax="jsligo">

The call `Option.value(d, opt)` is `v` if `opt` is
`["Some" as "Some", v]`, and `d` otherwise.

</Syntax>


<SyntaxTitle syntax="cameligo">
val value&#95;with&#95;error : &#39;err &#39;a.&#39;err -&gt; &#39;a option -&gt; &#39;a
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
value&#95;with&#95;error: &lt;err, a&gt;(error: err, opt: option&lt;a&gt;) =&gt; a
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `Option.value_with_error err opt` terminates with the error
`err` if, and only if, `opt` is `None`; otherwise it is `Some v`
and `v` is returned.

</Syntax>

<Syntax syntax="jsligo">

The call `Option.value_with_error(err, opt)` terminates with the
error `err` if, and only if, `opt` is `["None" as "None"]`;
otherwise it is `["Some" as "Some", v]` and `v` is returned.

</Syntax>


<SyntaxTitle syntax="cameligo">
val map : &#39;a &#39;b.(&#39;a -&gt; &#39;b) -&gt; &#39;a option -&gt; &#39;b option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
map: &lt;a, b&gt;(f: (&#95;: a) =&gt; b, opt: option&lt;a&gt;) =&gt; option&lt;b&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `Option.map f opt` is `None` if `opt` is `None`, and
`Some (f v)` if `opt` is `Some v`.

</Syntax>

<Syntax syntax="jsligo">

The call `Option.map(f, opt)` is `["None" as "None"]` if `opt` is
`["None" as "None"]`, and `["Some" as "Some", f(v)]` if `opt` is
`["Some" as "Some", v]`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val is&#95;none : &#39;a.&#39;a option -&gt; bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
is_none: &lt;a&gt;(&#95;: option&lt;a&gt;) =&gt; bool
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `Option.is_none opt` is `true` if, and only if, `opt` is `None`.
</Syntax>

<Syntax syntax="jsligo">

The call `Option.is_none(opt)` is `true` if, and only if, `opt` is
`["None" as "None"]`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val is&#95;some : &#39;a.&#39;a option -&gt; bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
is&#95;some: &lt;a&gt;(&#95;: option&lt;a&gt;) =&gt; bool
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `Option.is_some opt` is `false` if, and only if, `opt` is `None`.

</Syntax>

<Syntax syntax="jsligo">

The call `Option.is_some(opt)` is `false` if, and only if, `opt` is
`["None" as "None"]`.

</Syntax>
