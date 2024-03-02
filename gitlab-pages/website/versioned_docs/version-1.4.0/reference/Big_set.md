---
id: big-set-reference
title: Big_set
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';


Lazily accessed sets


<SyntaxTitle syntax="cameligo">
type &#39;elt t = (&#39;elt, unit) big&#95;map
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
type t&lt;elt&gt; = big&#95;map&lt;elt, unit&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The type of the big sets is based on `big_map`.

</Syntax>

<Syntax syntax="jsligo">

The type of the big sets is based on `big_map`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val empty : &#39;elt.&#39;elt t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let empty: &lt;elt&gt;t&lt;elt&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The value `empty` denotes the empty big set. In some contexts,
    it is useful to annotate it with its type, for example:
    `(empty as Big_set.t<int>)`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val update : &#39;elt.&#39;elt -&gt; bool -&gt; &#39;elt t -&gt; &#39;elt t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let update: &lt;elt&gt;(&#95;: elt) =&gt; (&#95;: bool) =&gt; (&#95;: t&lt;elt&gt;) =&gt; t&lt;elt&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `update elt true set` is a copy of the big set `set`
    containing the element `elt`. The call `update elt false set` is a
    copy of the big set `set` where the element `elt` is absent.

</Syntax>

<Syntax syntax="jsligo">

The call `update(elt, true, set)` is a copy of the big set `set`
    containing the element `elt`. The call `update(elt, false, set)`
    is a copy of the big set `set` where the element `elt` is
    absent.

</Syntax>


<SyntaxTitle syntax="cameligo">
val add : &#39;elt.&#39;elt -&gt; &#39;elt t -&gt; &#39;elt t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let add: &lt;elt&gt;(&#95;: elt) =&gt; (&#95;: t&lt;elt&gt;) =&gt; t&lt;elt&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `add elt set` is a big set containing all the elements
    of the big set `set`, plus the element `elt`.

</Syntax>

<Syntax syntax="jsligo">

The call `add(elt, set)` is a big set containing all the elements
    of the big set `set`, plus the element `elt`.

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

The call `literal [e1; ...; en]` is a big set containing exactly
    the elements in the list. Note: The list must be literal, not an
    expression (compile-time list of values).

</Syntax>

<Syntax syntax="jsligo">

The call `literal(list([e1, ..., en]))` is a big set containing
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

The call `of_list elements` is a big set containing exactly the
    elements in the list `elements`. Note: Use `literal` instead if
    using a literal list. Note: Use `literal` instead if using a
    literal list.

</Syntax>

<Syntax syntax="jsligo">

The call `of_list(elements)` is a big set containing exactly the
    elements in the list `elements`. Note: Use `literal` instead if
    using a literal list. Note: Use `literal` instead if using a
    literal list.

</Syntax>


<SyntaxTitle syntax="cameligo">
val mem : &#39;elt.&#39;elt -&gt; &#39;elt t -&gt; bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let mem: &lt;elt&gt;(&#95;: elt) =&gt; (&#95;: t&lt;elt&gt;) =&gt; bool
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `mem elt set` is `true` if, and only if, the element
    `elt` belongs to the big set `set`.

</Syntax>

<Syntax syntax="jsligo">

The call `mem(elt, set)` is `true` if, and only if, the element
    `elt` belongs to the big set `set`.

</Syntax>
