---
id: list-reference
title: List
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';


Lists


<SyntaxTitle syntax="cameligo">
type &#39;elt t = &#39;elt list
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
type t&lt;elt&gt; = list&lt;elt&gt;
</SyntaxTitle>
The type `t` is an alias for the predefined type `list`.


<SyntaxTitle syntax="cameligo">
val empty : &#39;elt.&#39;elt t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let empty: &lt;elt&gt;t&lt;elt&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The value `empty` is the empty list. It is a synonym for
    `[]`. In some contexts, it is useful to annotate it with its type,
    for example: `(empty : int list)`.

</Syntax>

<Syntax syntax="jsligo">

The value `empty` is the empty list. It is a synonym for
    `list([])`. In some contexts, it is useful to annotate it with its
    type, for example: `(empty as list<int>)`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val length : &#39;elt.&#39;elt t -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let length: &lt;elt&gt;(&#95;: t&lt;elt&gt;) =&gt; nat
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `length l` is the number of elements in the list
      `l`. Note: `List.length` is another name for `List.size`.

</Syntax>

<Syntax syntax="jsligo">

The call `length(l)` is the number of elements in the list
      `l`. Note: `List.length` is another name for `List.size`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val size : &#39;elt.&#39;elt t -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let size: &lt;elt&gt;(&#95;: t&lt;elt&gt;) =&gt; nat
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `size l` is the number of elements in the list `l`.

</Syntax>

<Syntax syntax="jsligo">

The call `size(l)` is the number of elements in the list `l`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val head : &#39;elt.&#39;elt t -&gt; &#39;elt option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let head: &lt;elt&gt;(&#95;: t&lt;elt&gt;) =&gt; option&lt;elt&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `head l`, where `l` is a list, is `None` if `l` is
    empty; otherwise, `Some hd`, where `hd` is the head of the list.

</Syntax>

<Syntax syntax="jsligo">

The call `head(l)`, where `l` is a list, is `None()` if `l` is
    empty; otherwise, `Some(hd)`, where `hd` is the head of the list.

</Syntax>


<SyntaxTitle syntax="cameligo">
val head&#95;opt : &#39;elt.&#39;elt t -&gt; &#39;elt option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let head&#95;opt: &lt;elt&gt;(&#95;: t&lt;elt&gt;) =&gt; option&lt;elt&gt;
</SyntaxTitle>
**Deprecated:** Use `List.head` instead.

<Syntax syntax="cameligo">

The call `head_opt l`, where `l` is a list, is `None` if `l` is
    empty; otherwise, `Some hd`, where `hd` is the head of the list.

</Syntax>

<Syntax syntax="jsligo">

The call `head_opt(l)`, where `l` is a list, is `None()` if `l` is
    empty; otherwise, `Some(hd)`, where `hd` is the head of the list.

</Syntax>


<SyntaxTitle syntax="cameligo">
val tail : &#39;elt.&#39;elt t -&gt; &#39;elt t option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let tail: &lt;elt&gt;(&#95;: t&lt;elt&gt;) =&gt; option&lt;t&lt;elt&gt;&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `tail l`, where `l` is a list, is `None` if `l` is
    empty; otherwise, `Some tl`, where `tl` is the tail of the list.

</Syntax>

<Syntax syntax="jsligo">

The call `tail(l)`, where `l` is a list, is `None()` if `l` is
    empty; otherwise, `Some(tl)`, where `tl` is the tail of the list.

</Syntax>


<SyntaxTitle syntax="cameligo">
val tail&#95;opt : &#39;elt.&#39;elt t -&gt; &#39;elt t option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let tail&#95;opt: &lt;elt&gt;(&#95;: t&lt;elt&gt;) =&gt; option&lt;t&lt;elt&gt;&gt;
</SyntaxTitle>
**Deprecated:** Use `List.tail` instead.

<Syntax syntax="cameligo">

The call `tail_opt l`, where `l` is a list, is `None` if `l` is
    empty; otherwise, `Some tl`, where `tl` is the tail of the list.

</Syntax>

<Syntax syntax="jsligo">

The call `tail_opt(l)`, where `l` is a list, is `None()` if `l` is
    empty; otherwise, `Some(tl)`, where `tl` is the tail of the list.

</Syntax>


<SyntaxTitle syntax="cameligo">
val map : &#39;src &#39;dst.(&#39;src -&gt; &#39;dst) -&gt; &#39;src list -&gt; &#39;dst list
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let map: &lt;src, dst&gt;(&#95;: (&#95;: src) =&gt; dst) =&gt; (&#95;: list&lt;src&gt;) =&gt; list&lt;dst&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `map f [a1; ...; an]` applies the function `f` to `a1`,
    ..., `an` (from left to right), and builds the list
    `[f a1; ...; f an]` with the results returned by `f`.

</Syntax>

<Syntax syntax="jsligo">

The call `map(f, list([a1; ...; an]))` applies the function `f` to
    `a1`, ..., `an` (from left to right), and builds the list
    `list([f(a1); ...; f(an)])` with the results returned by `f`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val iter : &#39;elt.(&#39;elt -&gt; unit) -&gt; &#39;elt t -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let iter: &lt;elt&gt;(&#95;: (&#95;: elt) =&gt; unit) =&gt; (&#95;: t&lt;elt&gt;) =&gt; unit
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `iter f [a1; ...; an]` applies the function `f` in turn
    to `[a1; ...; an]`. It is equivalent to
    `let () = f a1 in let () = f a2 in ... in f an`.

</Syntax>

<Syntax syntax="jsligo">

The call `iter(f, list([a1; ...; an]))` applies the function `f`
    in turn to `list([a1; ...; an])`. It is equivalent to `{f(a1);
    f(a2); ...; f(an)}`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val fold&#95;left : &#39;elt &#39;acc.((&#39;acc * &#39;elt) -&gt; &#39;acc) -&gt; &#39;acc -&gt; &#39;elt t -&gt; &#39;acc
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let fold&#95;left: &lt;elt, acc&gt;(&#95;: (&#95;: [acc, elt]) =&gt; acc) =&gt; (&#95;: acc) =&gt; (&#95;: t&lt;elt&gt;) =&gt; acc
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `fold_left f init [a1; ...; an]` is
    `f (... (f (f init a1) a2) ...) an`.

</Syntax>

<Syntax syntax="jsligo">

The call `fold_left(f, init, list([a1; ...; an]))` is
    `f (... (f (f(init, a1)), a2), ...), an)`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val fold&#95;right : &#39;elt &#39;acc.((&#39;elt * &#39;acc) -&gt; &#39;acc) -&gt; &#39;elt t -&gt; &#39;acc -&gt; &#39;acc
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let fold&#95;right: &lt;elt, acc&gt;(&#95;: (&#95;: [elt, acc]) =&gt; acc) =&gt; (&#95;: t&lt;elt&gt;) =&gt; (&#95;: acc) =&gt; acc
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `fold_right f [a1; ...; an] init` is
    `f a1 (f a2 (... (f an init) ...))`.

</Syntax>

<Syntax syntax="jsligo">

The call `fold_right(f, list([a1; ...; an]), init)` is
    `f (a1, f (a2, (..., f (an, init))...))`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val fold : &#39;elt &#39;acc.((&#39;acc * &#39;elt) -&gt; &#39;acc) -&gt; &#39;elt t -&gt; &#39;acc -&gt; &#39;acc
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let fold: &lt;elt, acc&gt;(&#95;: (&#95;: [acc, elt]) =&gt; acc) =&gt; (&#95;: t&lt;elt&gt;) =&gt; (&#95;: acc) =&gt; acc
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `fold f [a1; ...; an] init` is
    `f (... (f (f init a1) a2) ...) an`. Note:
    `fold_left f init list` is the same as `fold f list init`.

</Syntax>

<Syntax syntax="jsligo">

The call `fold(f, list([a1; ...; an]), init)` is
    `f (... (f (f (init, a1), a2) ...), an)`. Note:
    `fold_left(f, init, list)` is the same as `fold(f, list, init)`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val cons : &#39;elt.&#39;elt -&gt; &#39;elt t -&gt; &#39;elt t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let cons: &lt;elt&gt;(&#95;: elt) =&gt; (&#95;: t&lt;elt&gt;) =&gt; t&lt;elt&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `cons e l` is `e :: l`.

</Syntax>

<Syntax syntax="jsligo">

The call `cons(e, l)` is `list([e, ...l])`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val find&#95;opt : &#39;elt.(&#39;elt -&gt; bool) -&gt; &#39;elt t -&gt; &#39;elt option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let find&#95;opt: &lt;elt&gt;(&#95;: (&#95;: elt) =&gt; bool) =&gt; (&#95;: t&lt;elt&gt;) =&gt; option&lt;elt&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `find_opt pred list` is `None` if no element of the
    list `list` satisfies the predicate `pred`; otherwise, it is
    `Some e`, where `e` is the leftmost element in `list` that satisfies
    `pred`. The order of the calls of `pred` is not specified.

</Syntax>

<Syntax syntax="jsligo">

The call `find_opt(pred, list)` is `None()` if no element of the
    list `list` satisfies the predicate `pred`; otherwise, it is
    `Some(e)`, where `e` is the leftmost element in `list` that satisfies
    `pred`. The order of the calls of `pred` is not specified.

</Syntax>


<SyntaxTitle syntax="cameligo">
val filter&#95;map : &#39;src &#39;dst.(&#39;src -&gt; &#39;dst option) -&gt; &#39;src list -&gt; &#39;dst list
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let filter&#95;map: &lt;src, dst&gt;(&#95;: (&#95;: src) =&gt; option&lt;dst&gt;) =&gt; (&#95;: list&lt;src&gt;) =&gt; list&lt;dst&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `filter_map f l` is the maximal sub-list of `l` such
    that the call of function `f` on its elements is not `None`. Note:
    `f` is called on all elements of `l`. The order of the calls of
    `f` is not specified.

</Syntax>

<Syntax syntax="jsligo">

The call `filter_map(f, l)` is the maximal sub-list of `l` such
    that the call of function `f` on its elements is not `None()`. Note:
    `f` is called on all elements of `l`. The order of the calls of
    `f` is not specified.

</Syntax>


<SyntaxTitle syntax="cameligo">
val update : &#39;elt.(&#39;elt -&gt; &#39;elt option) -&gt; &#39;elt t -&gt; &#39;elt t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let update: &lt;elt&gt;(&#95;: (&#95;: elt) =&gt; option&lt;elt&gt;) =&gt; (&#95;: t&lt;elt&gt;) =&gt; t&lt;elt&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `update f l` is the list `l` where the elements `e`
    such that `f e` is `Some v` have been replaced by `v`.

</Syntax>

<Syntax syntax="jsligo">

The call `update(f, l)` is the list `l` where the elements `e`
    such that `f(e)` is `Some(v)` have been replaced by `v`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val update&#95;with : &#39;elt.(&#39;elt -&gt; bool) -&gt; &#39;elt -&gt; &#39;elt t -&gt; &#39;elt t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let update&#95;with: &lt;elt&gt;(&#95;: (&#95;: elt) =&gt; bool) =&gt; (&#95;: elt) =&gt; (&#95;: t&lt;elt&gt;) =&gt; t&lt;elt&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `update_with p d l` is the list `l` where the elements
    `e` such that satisfy the predicate `p` are replaced by `d`.

</Syntax>

<Syntax syntax="jsligo">

The call `update_with(p,d,l)` is the list `l` where the elements
    `e` such that satisfy the predicate `p` are replaced by `d`.

</Syntax>
