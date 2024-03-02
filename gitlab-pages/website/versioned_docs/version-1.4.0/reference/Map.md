---
id: map-reference
title: Map
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';


Maps from keys to values, where the bindings key/value are ordered
  by increasing keys.


<SyntaxTitle syntax="cameligo">
type (&#39;key, &#39;value) t = (&#39;key, &#39;value) map
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
type t&lt;key, value&gt; = map&lt;key, value&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The type `('key,'value) t` is an alias for `('key,'value) map`.

</Syntax>

<Syntax syntax="jsligo">

The type `t<key, value>` is an alias for `map<key,value>`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val empty : &#39;key &#39;value.(&#39;key, &#39;value) t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let empty: &lt;key, value&gt;t&lt;key, value&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The value `empty` is the empty map. In some contexts, it is
    useful to annotate it with its type, for example:
    `(empty : (int, string) map)`.

</Syntax>

<Syntax syntax="jsligo">

The value `empty` is the empty map. In some contexts, it is
    useful to annotate it with its type, for example:
    `(empty as map<int, string>)`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val get&#95;and&#95;update : &#39;key &#39;value.&#39;key -&gt; &#39;value option -&gt; (&#39;key, &#39;value) t -&gt; (&#39;value option * (&#39;key, &#39;value) t)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;and&#95;update: &lt;key, value&gt;(&#95;: key) =&gt; (&#95;: option&lt;value&gt;) =&gt; (&#95;: t&lt;key, value&gt;) =&gt; [option&lt;value&gt;, t&lt;key, value&gt;]
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `get_and_update key None map` returns a copy of the map
    `map` without the entry for the key `key` in `map` (no change if
    the key is absent). The call `get_and_update key (Some value) map`
    returns a copy of the map `map` where there is an entry for the
    key `key` associated with the value `value`. In both cases, if
    there was already a value `v` bound to `key`, it is returned as
    `Some v`, otherwise `None`.

</Syntax>

<Syntax syntax="jsligo">

The call `get_and_update(key, None(), map)` returns a copy of the
    map `map` without the entry for the key `key` in `map` (no change
    if the key is absent). The call `get_and_update(key, Some(value),
    map)` returns a copy of the map `map` where there is an entry for
    the key `key` associated with the value `value`. In both cases, if
    there was already a value `v` bound to `key`, it is returned as
    `Some(v)`, otherwise `None()`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val update : &#39;key &#39;value.&#39;key -&gt; &#39;value option -&gt; (&#39;key, &#39;value) t -&gt; (&#39;key, &#39;value) t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let update: &lt;key, value&gt;(&#95;: key) =&gt; (&#95;: option&lt;value&gt;) =&gt; (&#95;: t&lt;key, value&gt;) =&gt; t&lt;key, value&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `update key None map` returns a copy of the map `map`
    without the entry for the key `key` in `map` (no change if the key
    is absent). The call `update key (Some value) map` returns the map
    `map` where there is an entry for the key `key` associated with
    the value `value`. In both cases, the value originally bound to
    `key` is lost. See `get_and_update`.

</Syntax>

<Syntax syntax="jsligo">

The call `update(key, None(), map)` returns a copy of the map `map`
    without the entry for the key `key` in `map` (no change if the key
    is absent). The call `update(key, Some(value), map)` returns the map
    `map` where there is an entry for the key `key` associated with
    the value `value`. In both cases, the value originally bound to
    `key` is lost. See `get_and_update`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val add : &#39;key &#39;value.&#39;key -&gt; &#39;value -&gt; (&#39;key, &#39;value) t -&gt; (&#39;key, &#39;value) t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let add: &lt;key, value&gt;(&#95;: key) =&gt; (&#95;: value) =&gt; (&#95;: t&lt;key, value&gt;) =&gt; t&lt;key, value&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `add key value map` returns a copy of the `map` where
    there is a binding of key `key` to value `value`. If there is a
    binding for `key` in `map`, then it is lost.

</Syntax>

<Syntax syntax="jsligo">

The call `add(key, value, map)` returns a copy of the `map` where
    there is a binding of key `key` to value `value`. If there is a
    binding for `key` in `map`, then it is lost.

</Syntax>


<SyntaxTitle syntax="cameligo">
val remove : &#39;key &#39;value.&#39;key -&gt; (&#39;key, &#39;value) t -&gt; (&#39;key, &#39;value) t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let remove: &lt;key, value&gt;(&#95;: key) =&gt; (&#95;: t&lt;key, value&gt;) =&gt; t&lt;key, value&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `remove key map` returns a copy of the map `map` where
    the binding for key `key` is absent.

</Syntax>

<Syntax syntax="jsligo">

The call `remove(key, map)` returns a copy of the map `map` where
    the binding for key `key` is absent.

</Syntax>


<SyntaxTitle syntax="cameligo">
val literal : &#39;key &#39;value.(&#39;key * &#39;value) list -&gt; (&#39;key, &#39;value) t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let literal: &lt;key, value&gt;(&#95;: list&lt;[key, value]&gt;) =&gt; t&lt;key, value&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `literal [(k1,v1); ...; (kn,vn)]` returns a map from
    the pairs of key/value in the list. Note: The list must be a
    literal, not an expression (compile-time list of values).

</Syntax>

<Syntax syntax="jsligo">

The call `literal(list[[k1,v1], ..., [kn,vn]])` returns a map from
    the pairs of key/value in the list. Note: The list must be a
    literal, not an expression (compile-time list of values).

</Syntax>


<SyntaxTitle syntax="cameligo">
val of&#95;list : &#39;key &#39;value.(&#39;key * &#39;value) list -&gt; (&#39;key, &#39;value) t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let of&#95;list: &lt;key, value&gt;(&#95;: list&lt;[key, value]&gt;) =&gt; t&lt;key, value&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `of_list bindings` returns a map from the pairs of
    key/value in the list `bindings`. Note: Use `literal` instead if
    using a literal list.

</Syntax>

<Syntax syntax="jsligo">

The call `of_list(bindings)` returns a map from the pairs of
    key/value in the list `bindings`. Note: Use `literal` instead if
    using a literal list.

</Syntax>


<SyntaxTitle syntax="cameligo">
val size : &#39;key &#39;value.(&#39;key, &#39;value) t -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let size: &lt;key, value&gt;(&#95;: t&lt;key, value&gt;) =&gt; nat
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `size map` evaluates in the number of entries in the
    map `map`.

</Syntax>

<Syntax syntax="jsligo">

The call `size(map)` evaluates in the number of entries in the
    map `map`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val mem : &#39;key &#39;value.&#39;key -&gt; (&#39;key, &#39;value) t -&gt; bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let mem: &lt;key, value&gt;(&#95;: key) =&gt; (&#95;: t&lt;key, value&gt;) =&gt; bool
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `mem key map` is `true` if, and only if, the key `key`
    is in the map `map`.

</Syntax>

<Syntax syntax="jsligo">

The call `mem(key, map)` is `true` if, and only if, the key `key`
    is in the map `map`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val find&#95;opt : &#39;key &#39;value.&#39;key -&gt; (&#39;key, &#39;value) t -&gt; &#39;value option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let find&#95;opt: &lt;key, value&gt;(&#95;: key) =&gt; (&#95;: t&lt;key, value&gt;) =&gt; option&lt;value&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `find_opt key map` returns `None` if the key `key` is
    present in the map `map`; otherwise, it is `Some v`, where `v` is
    the value associated to `key` in `map`.

</Syntax>

<Syntax syntax="jsligo">

The call `find_opt(key, map)` returns `None()` if the key `key` is
    present in the map `map`; otherwise, it is `Some(v)`, where `v` is
    the value associated to `key` in `map`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val find : &#39;key &#39;value.&#39;key -&gt; (&#39;key, &#39;value) t -&gt; &#39;value
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let find: &lt;key, value&gt;(&#95;: key) =&gt; (&#95;: t&lt;key, value&gt;) =&gt; value
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `find key map` returns the value associated to `key` in
    `map`. If the key is absent, the execution fails with the string
    `"MAP FIND"`.

</Syntax>

<Syntax syntax="jsligo">

The call `find(key, map)` returns the value associated to `key` in
    `map`. If the key is absent, the execution fails with the string
    `"MAP FIND"`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val fold : &#39;key &#39;value &#39;acc.((&#39;acc * &#39;key * &#39;value) -&gt; &#39;acc) -&gt; (&#39;key, &#39;value) t -&gt; &#39;acc -&gt; &#39;acc
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let fold: &lt;key, value, acc&gt;(&#95;: (&#95;: [acc, [key, value]]) =&gt; acc) =&gt; (&#95;: t&lt;key, value&gt;) =&gt; (&#95;: acc) =&gt; acc
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `fold f map init` is
    `f ( ... f (f (init, (k1,v1)), (k2,v2)), ..., (kn,vn))`
    where `(k1,v1)`, `(k2,v2)`, ..., `(kn,vn)` are the bindings in the
    map `map`, in increasing order of the keys `k1`, `k2`, ..., and `kn`.

</Syntax>

<Syntax syntax="jsligo">

The call `fold(f, map, init)` is
    `f (... f (f (init, [k1,v1]), [k2,v2]), ..., [kn,vn])`
    where `[k1,v1]`, `[k2,v2]`, ..., `[kn,vn]` are the bindings in the
    map `map`, in increasing order of the keys `k1`, `k2`, ..., and `kn`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val iter : &#39;key &#39;value.((&#39;key * &#39;value) -&gt; unit) -&gt; (&#39;key, &#39;value) t -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let iter: &lt;key, value&gt;(&#95;: (&#95;: [key, value]) =&gt; unit) =&gt; (&#95;: t&lt;key, value&gt;) =&gt; unit
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `iter f map` is
    `let () = f (k1,v1) in let () = f (k2,v2) in ... in f (kn,vn)`.

</Syntax>

<Syntax syntax="jsligo">

The call `iter(f, map)` is `{f (k1,v1); (k2,v2); ...; f (kn,vn);}`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val map : &#39;key &#39;value &#39;new&#95;value.((&#39;key * &#39;value) -&gt; &#39;new&#95;value) -&gt; (&#39;key, &#39;value) t -&gt; (&#39;key, &#39;new&#95;value) t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let map: &lt;key, value, new&#95;value&gt;(&#95;: (&#95;: [key, value]) =&gt; new&#95;value) =&gt; (&#95;: t&lt;key, value&gt;) =&gt; t&lt;key, new&#95;value&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `map f m`, where the map `m` contains the bindings
    `(k1,v1)`, `(k2,v2)`, ..., and `(kn,vn)` in increasing order of
    the keys, is the map containing the bindings `(k1, f (k1,v1))`,
    `(k2, f (k2,v2))`, ..., `(kn, f (kn,vn))`.

</Syntax>

<Syntax syntax="jsligo">

The call `map(f, m)`, where the map `m` contains the bindings
    `[k1,v1]`, `[k2,v2]`, ..., and `[kn,vn]` in increasing order of
    the keys, is the map containing the bindings `[k1, f (k1,v1)]`,
    `[k2, f (k2,v2)]`, ..., `[kn, f (kn,vn)]`.

</Syntax>
