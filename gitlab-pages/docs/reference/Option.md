---
id: option-reference
title: Option
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';


The module of optional values


<SyntaxTitle syntax="cameligo">
val value : &#39;a.&#39;a -&gt; &#39;a option -&gt; &#39;a
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let value: &lt;a&gt;(&#95;: a) =&gt; (&#95;: option&lt;a&gt;) =&gt; a
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `value d opt` is `v` if `opt` is `Some v`, and `d`
    otherwise.

</Syntax>

<Syntax syntax="jsligo">

The call `value(d, opt)` is `v` if `opt` is `Some(v)`, and `d`
    otherwise.

</Syntax>


<SyntaxTitle syntax="cameligo">
val value&#95;with&#95;error : &#39;err &#39;a.&#39;err -&gt; &#39;a option -&gt; &#39;a
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let value&#95;with&#95;error: &lt;err, a&gt;(&#95;: err) =&gt; (&#95;: option&lt;a&gt;) =&gt; a
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `value_with_error err opt` terminates with the error
    `err` if, and only if, `opt` is `None`; otherwise it is `Some v`
    and `v` is returned.

</Syntax>

<Syntax syntax="jsligo">

The call `value_with_error(err, opt)` terminates with the error
    `err` if, and only if, `opt` is `None()`; otherwise it is `Some(v)`
    and `v` is returned.

</Syntax>


<SyntaxTitle syntax="cameligo">
val value&#95;exn : &#39;err &#39;a.&#39;err -&gt; &#39;a option -&gt; &#39;a
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let value&#95;exn: &lt;err, a&gt;(&#95;: err) =&gt; (&#95;: option&lt;a&gt;) =&gt; a
</SyntaxTitle>
**Deprecated:** Use `Option.value_with_error` instead.

<Syntax syntax="cameligo">

The call `value_exn err opt` terminates with the error `err` if,
    and only if, `opt` is `None`; otherwise it is `Some v` and `v` is
    returned.

</Syntax>

<Syntax syntax="jsligo">

The call `value_exn(err, opt)` terminates with the error `err` if,
    and only if, `opt` is `None()`; otherwise it is `Some(v)` and `v` is
    returned.

</Syntax>


<SyntaxTitle syntax="cameligo">
val unopt&#95;with&#95;error : &#39;a.&#39;a option -&gt; string -&gt; &#39;a
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let unopt&#95;with&#95;error: &lt;a&gt;(&#95;: option&lt;a&gt;) =&gt; (&#95;: string) =&gt; a
</SyntaxTitle>
**Deprecated:** Use `Option.value_with_error` instead.

<Syntax syntax="cameligo">

The call `unopt_with_error opt err` terminates with the error
    `err` if, and only if, `opt` is `None`; otherwise it is `Some v`
    and `v` is returned.

</Syntax>

<Syntax syntax="jsligo">

The call `unopt_with_error(opt, err)` terminates with the error
    `err` if, and only if, `opt` is `None()`; otherwise it is
    `Some(v)` and `v` is returned.

</Syntax>


<SyntaxTitle syntax="cameligo">
val unopt : &#39;a.&#39;a option -&gt; &#39;a
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let unopt: &lt;a&gt;(&#95;: option&lt;a&gt;) =&gt; a
</SyntaxTitle>
**Deprecated:** Use `Option.value_with_error` instead.

<Syntax syntax="cameligo">

The call `unopt opt ` terminates with the string
    `"option is None"` if, and only if, `opt` is `None`; otherwise it is
    `Some v` and `v` is returned.

</Syntax>

<Syntax syntax="jsligo">

The call `unopt(opt)` terminates with the string
    `"option is None"` if, and only if, `opt` is `None()`; otherwise it is
    `Some(v)` and `v` is returned.

</Syntax>


<SyntaxTitle syntax="cameligo">
val map : &#39;a &#39;b.(&#39;a -&gt; &#39;b) -&gt; &#39;a option -&gt; &#39;b option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let map: &lt;a, b&gt;(&#95;: (&#95;: a) =&gt; b) =&gt; (&#95;: option&lt;a&gt;) =&gt; option&lt;b&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `map f opt` is `None` if `opt` is `None`, and
    `Some (f v)` if `opt` is `Some v`.

</Syntax>

<Syntax syntax="jsligo">

The call `map(f, opt)` is `None()` if `opt` is `None()`, and
    `Some(f(v))` if `opt` is `Some(v)`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val is&#95;none : &#39;a.&#39;a option -&gt; bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let is&#95;none: &lt;a&gt;(&#95;: option&lt;a&gt;) =&gt; bool
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `is_none opt` is `true` if, and only if, `opt` is
    `None`.

</Syntax>

<Syntax syntax="jsligo">

The call `is_none(opt)` is `true` if, and only if, `opt` is
    `None()`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val is&#95;some : &#39;a.&#39;a option -&gt; bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let is&#95;some: &lt;a&gt;(&#95;: option&lt;a&gt;) =&gt; bool
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `is_some opt` is `false` if, and only if, `opt` is
    `None`.

</Syntax>

<Syntax syntax="jsligo">

The call `is_some(opt)` is `false` if, and only if, `opt` is
    `None()`.

</Syntax>
