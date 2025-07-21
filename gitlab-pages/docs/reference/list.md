---
id: list-reference
title: list
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
empty: &lt;elt&gt;t&lt;elt&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The value `List.empty` is the empty list. It is a synonym for
    `[]`. In some contexts, it is useful to annotate it with its type,
    for example: `(empty : int list)`.

</Syntax>

<Syntax syntax="jsligo">

The value `List.empty` is the empty list. It is a synonym for
    `list([])`. In some contexts, it is useful to annotate it with its
    type, for example: `(empty as list<int>)`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val length : &#39;elt.&#39;elt t -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
length: &lt;elt&gt;(list: t&lt;elt&gt;) =&gt; nat
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `List.length l` is the number of elements in the list
`l`. Note: `List.length` is another name for `List.size`.

</Syntax>

<Syntax syntax="jsligo">

The call `List.length(l)` is the number of elements in the list
`l`. Note: `List.length` is another name for `List.size`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val size : &#39;elt.&#39;elt t -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
size: &lt;elt&gt;(list: t&lt;elt&gt;) =&gt; nat
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `List.size l` is the number of elements in the list `l`.

</Syntax>

<Syntax syntax="jsligo">

The call `List.size(l)` is the number of elements in the list `l`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val head : &#39;elt.&#39;elt t -&gt; &#39;elt option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
head: &lt;elt&gt;(list: t&lt;elt&gt;) =&gt; option&lt;elt&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `List.head l`, where `l` is a list, is `None` if `l` is
empty; otherwise, `Some hd`, where `hd` is the head of the list.

</Syntax>

<Syntax syntax="jsligo">

The call `List.head(l)`, where `l` is a list, is `["None" as "None"]`
if `l` is empty; otherwise, `["Some" as "Some", hd]`, where `hd` is
the head of the list.

</Syntax>


<SyntaxTitle syntax="cameligo">
val tail : &#39;elt.&#39;elt t -&gt; &#39;elt t option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
tail: &lt;elt&gt;(list: t&lt;elt&gt;) =&gt; option&lt;t&lt;elt&gt;&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `List.tail l`, where `l` is a list, is `None` if `l` is
empty; otherwise, `Some tl`, where `tl` is the tail of the list.

</Syntax>

<Syntax syntax="jsligo">

The call `List.tail(l)`, where `l` is a list, is `["None" as "None"]`
if `l` is empty; otherwise, `["Some" as "Some", tl]`, where `tl`
is the tail of the list.

</Syntax>


<SyntaxTitle syntax="cameligo">
val head_and_tail : &#39;elt.&#39;elt t -&gt; (&#39;elt * &#39;elt t) option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
head_and_tail: &lt;elt&gt;(list: t&lt;elt&gt;) =&gt; option&lt;elt,t&lt; elt&gt;&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `List.head_and_tail l`, where `l` is a list, is `None` if `l`
is empty; otherwise, `Some (hd, tl)`, where `hd` and `tl` are the head
and tail of the list, respectively.

</Syntax>

<Syntax syntax="jsligo">

The call `List.head_and_tail(l)`, where `l` is a list, is
`["None" as "None"]` if `l` is empty; otherwise,
`["Some" as "Some", [hd,tl]]`, where `hd` and `tl` are the head and
tail of the list, respectively.

</Syntax>


<SyntaxTitle syntax="cameligo">
val map : &#39;src &#39;dst.(&#39;src -&gt; &#39;dst) -&gt; &#39;src t -&gt; &#39;dst t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
map: &lt;src, dst&gt;(f: (&#95;: src) =&gt; dst, list: t&lt;src&gt;) =&gt; t&lt;dst&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `List.map f [a1; ...; an]` applies the function `f` to `a1`,
..., `an` (from left to right), and builds the list
`[f a1; ...; f an]` with the results returned by `f`.

</Syntax>

<Syntax syntax="jsligo">

The call `List.map(f, list([a1; ...; an]))` applies the function `f` to
`a1`, ..., `an` (from left to right), and builds the list
`list([f(a1); ...; f(an)])` with the results returned by `f`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val iter : &#39;elt.(&#39;elt -&gt; unit) -&gt; &#39;elt t -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
iter: &lt;elt&gt;(f: (&#95;: elt) =&gt; unit, list: t&lt;elt&gt;) =&gt; unit
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `List.iter f [a1; ...; an]` applies the function `f` in turn
to `[a1; ...; an]`. It is equivalent to
`let () = f a1 in let () = f a2 in ... in f an`.

</Syntax>

<Syntax syntax="jsligo">

The call `List.iter(f, list([a1; ...; an]))` applies the function `f`
in turn to `list([a1; ...; an])`. It is equivalent to `{f(a1);
f(a2); ...; f(an)}`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val fold_left : &#39;elt &#39;acc.((&#39;acc * &#39;elt) -&gt; &#39;acc) -&gt; &#39;acc -&gt; &#39;elt t -&gt; &#39;acc
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
fold_left: &lt;elt, acc&gt;(f: (&#95;: [acc, elt]) =&gt; acc, init:
acc, list: t&lt;elt&gt;) =&gt; acc
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `List.fold_left f init [a1; ...; an]` is
`f (... (f (f init a1) a2) ...) an`.

</Syntax>

<Syntax syntax="jsligo">

The call `List.fold_left(f, init, list([a1; ...; an]))` is
`f (... (f (f(init, a1)), a2), ...), an)`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val fold_right : &#39;elt &#39;acc.((&#39;elt * &#39;acc) -&gt; &#39;acc) -&gt; &#39;elt t -&gt; &#39;acc -&gt; &#39;acc
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
fold_right: &lt;elt, acc&gt;(f: (&#95;: [elt, acc]) =&gt; acc, list:
t&lt;elt&gt;, init: acc) =&gt; acc
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `List.fold_right f [a1; ...; an] init` is
`f a1 (f a2 (... (f an init) ...))`.

</Syntax>

<Syntax syntax="jsligo">

The call `List.fold_right(f, list([a1; ...; an]), init)` is
`f (a1, f (a2, (..., f (an, init))...))`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val fold : &#39;elt &#39;acc.((&#39;acc * &#39;elt) -&gt; &#39;acc) -&gt; &#39;elt t -&gt; &#39;acc -&gt; &#39;acc
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
fold: &lt;elt, acc&gt;(f: (&#95;: [acc, elt]) =&gt; acc, list:
t&lt;elt&gt;, init: acc) =&gt; acc
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `List.fold f [a1; ...; an] init` is
`f (... (f (f init a1) a2) ...) an`. Note:
`List.fold_left f init list` is the same as `List.fold f list init`.

</Syntax>

<Syntax syntax="jsligo">

The call `List.fold(f, list([a1; ...; an]), init)` is
`f (... (f (f (init, a1), a2) ...), an)`. Note:
`List.fold_left(f, init, list)` is the same as `List.fold(f, list, init)`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val cons : &#39;elt.&#39;elt -&gt; &#39;elt t -&gt; &#39;elt t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
cons: &lt;elt&gt;(elt: elt, list: t&lt;elt&gt;) =&gt; t&lt;elt&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `List.cons e l` is `e :: l`.

</Syntax>

<Syntax syntax="jsligo">

The call `List.cons(e, l)` is `list([e, ...l])`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val find_opt : &#39;elt.(&#39;elt -&gt; bool) -&gt; &#39;elt t -&gt; &#39;elt option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
find_opt: &lt;elt&gt;(f: (&#95;: elt) =&gt; bool, list: t&lt;elt&gt;) =&gt; option&lt;elt&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `List.find_opt pred list` is `None` if no element of the list
`list` satisfies the predicate `pred`; otherwise, it is `Some e`,
where `e` is the leftmost element in `list` that satisfies `pred`. The
order of the calls of `pred` is not specified.

</Syntax>

<Syntax syntax="jsligo">

The call `List.find_opt(pred, list)` is `["None" as "None"]` if no
element of the list `list` satisfies the predicate `pred`; otherwise,
it is `["Some" as "Some", e]`, where `e` is the leftmost element in
`list` that satisfies `pred`. The order of the calls of `pred` is not
specified.

</Syntax>


<SyntaxTitle syntax="cameligo">
val filter_map : &#39;src &#39;dst.(&#39;src -&gt; &#39;dst option) -&gt; &#39;src list -&gt; &#39;dst list
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
filter_map: &lt;src, dst&gt;(filter: (&#95;: src) =&gt;
option&lt;dst&gt;, list: list&lt;src&gt;) =&gt; list&lt;dst&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `List.filter_map f l` is the maximal sub-list of `l` such
that the call of function `f` on its elements is not `None`. Note: `f`
is called on all elements of `l`. The order of the calls of `f` is not
specified.

</Syntax>

<Syntax syntax="jsligo">

The call `List.filter_map(f, l)` is the maximal sub-list of `l` such
that the call of function `f` on its elements is not
`["None" as "None"]`. Note: `f` is called on all elements of `l`.  The
order of the calls of `f` is not specified.

</Syntax>


<SyntaxTitle syntax="cameligo">
val update : &#39;elt.(&#39;elt -&gt; &#39;elt option) -&gt; &#39;elt t -&gt; &#39;elt t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
update: &lt;elt&gt;(filter: (&#95;: elt) =&gt; option&lt;elt&gt;, list: t&lt;elt&gt;) =&gt; t&lt;elt&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `List.update f l` is the list `l` where the elements `e`
such that `f e` is `Some v` have been replaced by `v`.

</Syntax>

<Syntax syntax="jsligo">

The call `List.update(f, l)` is the list `l` where the elements `e`
such that `f(e)` is `["Some" as "Some", v]` have been replaced by `v`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val update_with : &#39;elt.(&#39;elt -&gt; bool) -&gt; &#39;elt -&gt; &#39;elt t -&gt; &#39;elt t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
update_with: &lt;elt&gt;(pred: (&#95;: elt) =&gt; bool, default: elt, list: t&lt;elt&gt;) =&gt; t&lt;elt&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `List.update_with p d l` is the list `l` where the elements
`e` such that satisfy the predicate `p` are replaced by `d`.

</Syntax>

<Syntax syntax="jsligo">

The call `List.update_with(p,d,l)` is the list `l` where the elements
`e` such that satisfy the predicate `p` are replaced by `d`.

</Syntax>
