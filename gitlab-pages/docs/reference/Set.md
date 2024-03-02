---
id: set-reference
title: Set
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';


Totally ordered sets


<SyntaxTitle syntax="cameligo">
type &#39;elt t = &#39;elt set
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
type t&lt;elt&gt; = set&lt;elt&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The type `'elt t` is an alias for `'elt set`.

</Syntax>

<Syntax syntax="jsligo">

The type `t<elt>` is an alias for `set<elt>`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val empty : &#39;elt.&#39;elt t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let empty: &lt;elt&gt;t&lt;elt&gt;
</SyntaxTitle>
<Syntax syntax="jsligo">

The value `empty` denotes the empty set. In some contexts, it is
    useful to annotate it with its type, for example:
    `(empty as set<int>)`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val update : &#39;elt.&#39;elt -&gt; bool -&gt; &#39;elt t -&gt; &#39;elt t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let update: &lt;elt&gt;(&#95;: elt) =&gt; (&#95;: bool) =&gt; (&#95;: t&lt;elt&gt;) =&gt; t&lt;elt&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `update elt true set` is a copy of the set `set`
    containing the element `elt`. The call `update elt false set` is a
    copy of the set `set` where the element `elt` is absent.

</Syntax>

<Syntax syntax="jsligo">

The call `update(elt, true, set)` is a copy of the set `set`
    containing the element `elt`. The call `update(elt, false, set)` is a
    copy of the set `set` where the element `elt` is absent.

</Syntax>


<SyntaxTitle syntax="cameligo">
val add : &#39;elt.&#39;elt -&gt; &#39;elt t -&gt; &#39;elt t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let add: &lt;elt&gt;(&#95;: elt) =&gt; (&#95;: t&lt;elt&gt;) =&gt; t&lt;elt&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `add elt set` is a set containing all the elements of
    the set `set`, plus the element `elt`.

</Syntax>

<Syntax syntax="jsligo">

The call `add(elt, set)` is a set containing all the elements of
    the set `set`, plus the element `elt`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val remove : &#39;elt.&#39;elt -&gt; &#39;elt t -&gt; &#39;elt t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let remove: &lt;elt&gt;(&#95;: elt) =&gt; (&#95;: t&lt;elt&gt;) =&gt; t&lt;elt&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `remove elt set` is a copy of the set `set` without the
    element `elt`.

</Syntax>

<Syntax syntax="jsligo">

The call `remove(elt, set)` is a copy of the set `set` without the
    element `elt`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val literal : &#39;elt.&#39;elt list -&gt; &#39;elt t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let literal: &lt;elt&gt;(&#95;: list&lt;elt&gt;) =&gt; t&lt;elt&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `literal [e1; ...; en]` is a set containing exactly the
    elements in the list. Note: The list must be literal, not an
    expression (compile-time list of values).

</Syntax>

<Syntax syntax="jsligo">

The call `literal(list([e1, ..., en]))` is a set containing
    exactly the elements in the list. Note: The list must be literal,
    not an expression (compile-time list of values).

</Syntax>


<SyntaxTitle syntax="cameligo">
val of&#95;list : &#39;elt.&#39;elt list -&gt; &#39;elt t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let of&#95;list: &lt;elt&gt;(&#95;: list&lt;elt&gt;) =&gt; t&lt;elt&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `of_list elements` is a set containing exactly the
    elements in the list `elements`. Note: Use `literal` instead if
    using a literal list. Note: Use `literal` instead if using a
    literal list.

</Syntax>

<Syntax syntax="jsligo">

The call `of_list(elements)` is a set containing exactly the
    elements in the list `elements`. Note: Use `literal` instead if
    using a literal list. Note: Use `literal` instead if using a
    literal list.

</Syntax>


<SyntaxTitle syntax="cameligo">
val size : &#39;elt.&#39;elt t -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let size: &lt;elt&gt;(&#95;: t&lt;elt&gt;) =&gt; nat
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `size set` is the number of elements of the set `set`.

</Syntax>

<Syntax syntax="jsligo">

The call `size(set)` is the number of elements of the set `set`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val cardinal : &#39;elt.&#39;elt t -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let cardinal: &lt;elt&gt;(&#95;: t&lt;elt&gt;) =&gt; nat
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `cardinal set` is the number of elements of the set `set`.

</Syntax>

<Syntax syntax="jsligo">

The call `cardinal(set)` is the number of elements of the set `set`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val mem : &#39;elt.&#39;elt -&gt; &#39;elt t -&gt; bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let mem: &lt;elt&gt;(&#95;: elt) =&gt; (&#95;: t&lt;elt&gt;) =&gt; bool
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `mem elt set` is `true` if, and only if, the element
    `elt` belongs to the set `set`.

</Syntax>

<Syntax syntax="jsligo">

The call `mem(elt, set)` is `true` if, and only if, the element
    `elt` belongs to the set `set`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val fold : &#39;elt &#39;acc.((&#39;acc * &#39;elt) -&gt; &#39;acc) -&gt; &#39;elt t -&gt; &#39;acc -&gt; &#39;acc
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let fold: &lt;elt, acc&gt;(&#95;: (&#95;: [acc, elt]) =&gt; acc) =&gt; (&#95;: t&lt;elt&gt;) =&gt; (&#95;: acc) =&gt; acc
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `fold f set init` is
    `f(... (f (f (init, e1), e2), ...), en)`,
    where `e1`, `e2`, ..., `en` are the elements of the set `set` in
    increasing order.

</Syntax>

<Syntax syntax="jsligo">

The call `fold(f, set, init)` is
    `f(... (f (f (init, e1), e2), ...), en)`,
    where `e1`, `e2`, ..., `en` are the elements of the set `set` in
    increasing order.

</Syntax>


<SyntaxTitle syntax="cameligo">
val fold&#95;desc : &#39;elt &#39;acc.((&#39;elt * &#39;acc) -&gt; &#39;acc) -&gt; &#39;elt t -&gt; &#39;acc -&gt; &#39;acc
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let fold&#95;desc: &lt;elt, acc&gt;(&#95;: (&#95;: [elt, acc]) =&gt; acc) =&gt; (&#95;: t&lt;elt&gt;) =&gt; (&#95;: acc) =&gt; acc
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `fold f set init` is `f(... (f (init, en), ...), e1)`,
    where `e1`, `e2`, ..., `en` are the elements of the set `set` in
    increasing order.

</Syntax>

<Syntax syntax="jsligo">

The call `fold(f, set, init)` is `f(... (f (init, en), ...), e1)`,
    where `e1`, `e2`, ..., `en` are the elements of the set `set` in
    increasing order.

</Syntax>


<SyntaxTitle syntax="cameligo">
val filter&#95;map : &#39;old &#39;new.(&#39;old -&gt; &#39;new option) -&gt; &#39;old t -&gt; &#39;new t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let filter&#95;map: &lt;old, new&gt;(&#95;: (&#95;: old) =&gt; option&lt;new&gt;) =&gt; (&#95;: t&lt;old&gt;) =&gt; t&lt;new&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `filter_map f set` is a set made by calling `f` (the
    filter) on each element of the set `set`: if `f` returns `None`,
    the element is skipped in the result, otherwise, if it is
    `Some e`, then `e` is kept.

</Syntax>

<Syntax syntax="jsligo">

The call `filter_map(f, set)` is a set made by calling `f` (the
    filter) on each element of the set `set`: if `f` returns `None()`,
    the element is skipped in the result, otherwise, if it is
    `Some(e)`, then `e` is kept.

</Syntax>


<SyntaxTitle syntax="cameligo">
val iter : &#39;elt.(&#39;elt -&gt; unit) -&gt; &#39;elt t -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let iter: &lt;elt&gt;(&#95;: (&#95;: elt) =&gt; unit) =&gt; (&#95;: t&lt;elt&gt;) =&gt; unit
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `iter f set` applies `f` to all the elements of the set
    `set` in increasing order.

</Syntax>

<Syntax syntax="jsligo">

The call `iter(f, set)` applies `f` to all the elements of the set
    `set` in increasing order.

</Syntax>


<SyntaxTitle syntax="cameligo">
val map : &#39;old &#39;new.(&#39;old -&gt; &#39;new) -&gt; &#39;old t -&gt; &#39;new t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let map: &lt;old, new&gt;(&#95;: (&#95;: old) =&gt; new) =&gt; (&#95;: t&lt;old&gt;) =&gt; t&lt;new&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `map f set` evaluates in a set whose elements have been
    obtained by applying `f` to the elements of the set `set`.

</Syntax>

<Syntax syntax="jsligo">

The call `map(f, set)` evaluates in a set whose elements have been
    obtained by applying `f` to the elements of the set `set`.

</Syntax>
