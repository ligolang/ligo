---
id: big-map-reference
title: big_map
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';


Maps from keys to values, lazily accessed and where the bindings
  key/value are ordered by increasing keys.


<SyntaxTitle syntax="cameligo">
type (&#39;key, &#39;value) t = (&#39;key, &#39;value) big&#95;map
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
type t&lt;key, value&gt; = big&#95;map&lt;key, value&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The type `('key,'value) Big_map.t` is an alias for
      `('key,'value) big_map`.

</Syntax>

<Syntax syntax="jsligo">

The type `Big_map.t<key, value>` is an alias for `big_map<key,
      value>`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val empty : &#39;key &#39;value.(&#39;key, &#39;value) t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let empty: &lt;key, value&gt;t&lt;key, value&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The value `Big_map.empty` is the empty big map. In some
      contexts, it is useful to annotate it with its type, for example:
      `(Big_map.empty : (int, string) big_map)`.

</Syntax>

<Syntax syntax="jsligo">

The value `Big_map.empty` is the empty big map. In some
      contexts, it is useful to annotate it with its type, for example:
      `(Big_map.empty as big_map<int, string>`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val get&#95;and&#95;update : &#39;key &#39;value.&#39;key -&gt; &#39;value option -&gt; (&#39;key, &#39;value) t -&gt; (&#39;value option * (&#39;key, &#39;value) t)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;and&#95;update: &lt;key, value&gt;(&#95;: key) =&gt; (&#95;: option&lt;value&gt;) =&gt; (&#95;: t&lt;key, value&gt;) =&gt; [option&lt;value&gt;, t&lt;key, value&gt;]
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `Big_map.get_and_update key None map` returns a copy of
      the big map `map` without the entry for the key `key` in `map`
      (no change if the key is absent). The call
      `Big_map.get_and_update key (Some value) map` returns a copy of
      the big map `map` where there is an entry for the key `key`
      associated with the value `value`. In both cases, if there was
      already a value `v` bound to `key`, it is returned as `Some v`,
      otherwise `None`.

</Syntax>

<Syntax syntax="jsligo">

The call `Big_map.get_and_update(key, None(), map)` returns a
      copy of the big map `map` without the entry for the key `key` in
      `map` (no change if the key is absent). The call
      `Big_map.get_and_update(key, Some(value), map)` returns a copy
      of the big map `map` where there is an entry for the key `key`
      associated with the value `value`. In both cases, if there was
      already a value `v` bound to `key`, it is returned as `Some(v)`,
      otherwise `None()`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val update : &#39;key &#39;value.&#39;key -&gt; &#39;value option -&gt; (&#39;key, &#39;value) t -&gt; (&#39;key, &#39;value) t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let update: &lt;key, value&gt;(&#95;: key) =&gt; (&#95;: option&lt;value&gt;) =&gt; (&#95;: t&lt;key, value&gt;) =&gt; t&lt;key, value&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `Big_map.update key None map` returns a copy of the big
      map `map` without the entry for the key `key` in `map` (no
      change if the key is absent). The call `Big_map.update key (Some
      value) map` returns the big map `map` where there is an entry
      for the key `key` associated with the value `value`. In both
      cases, the value originally bound to `key` is lost. See
      `Big_map.get_and_update`.

</Syntax>

<Syntax syntax="jsligo">

The call `Big_map.update(key, None(), map)` returns a copy of
      the big map `map` without the entry for the key `key` in `map`
      (no change if the key is absent). The call `Big_map.update(key,
      Some(value), map)` returns the big map `map` where there is an
      entry for the key `key` associated with the value `value`. In
      both cases, the value originally bound to `key` is lost. See
      `Big_map.get_and_update`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val add : &#39;key &#39;value.&#39;key -&gt; &#39;value -&gt; (&#39;key, &#39;value) t -&gt; (&#39;key, &#39;value) t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let add: &lt;key, value&gt;(&#95;: key) =&gt; (&#95;: value) =&gt; (&#95;: t&lt;key, value&gt;) =&gt; t&lt;key, value&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `Big_map.add key value map` returns a copy of the big
      map `map` where there is a binding of key `key` to value
      `value`. If there is a binding for `key` in `map`, then it is
      lost.

</Syntax>

<Syntax syntax="jsligo">

The call `Big_map.add(key, value, map)` returns a copy of the
      big map `map` where there is a binding of key `key` to value
      `value`. If there is a binding for `key` in `map`, then it is
      lost.

</Syntax>


<SyntaxTitle syntax="cameligo">
val remove : &#39;key &#39;value.&#39;key -&gt; (&#39;key, &#39;value) t -&gt; (&#39;key, &#39;value) t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let remove: &lt;key, value&gt;(&#95;: key) =&gt; (&#95;: t&lt;key, value&gt;) =&gt; t&lt;key, value&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `Big_map.remove key map` returns a copy of the big map
      `map` where the binding for key `key` is absent.

</Syntax>

<Syntax syntax="jsligo">

The call `Big_map.remove(key, map)` returns a copy of the big
      map `map` where the binding for key `key` is absent.

</Syntax>


<SyntaxTitle syntax="cameligo">
val literal : &#39;key &#39;value.(&#39;key * &#39;value) list -&gt; (&#39;key, &#39;value) t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let literal: &lt;key, value&gt;(&#95;: list&lt;[key, value]&gt;) =&gt; t&lt;key, value&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `Big_map.literal [(k1,v1); ...; (kn,vn)]` returns a big
      map from the pairs of key/value in the list. Note: The list must
      be a literal, not an expression (compile-time list of values).

</Syntax>

<Syntax syntax="jsligo">

The call `Big_map.literal(list[[k1,v1], ..., [kn,vn]])` returns
      a big map from the pairs of key/value in the list. Note: The list
      must be a literal, not an expression (compile-time list of
      values).

</Syntax>


<SyntaxTitle syntax="cameligo">
val of&#95;list : &#39;key &#39;value.(&#39;key * &#39;value) list -&gt; (&#39;key, &#39;value) t
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let of&#95;list: &lt;key, value&gt;(&#95;: list&lt;[key, value]&gt;) =&gt; t&lt;key, value&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `Big_map.of_list bindings` returns a big map from the
      pairs of key/value in the list `bindings`. Note: Use
      `Big_map.literal` instead if using a literal list.

</Syntax>

<Syntax syntax="jsligo">

The call `Big_map.of_list(bindings)` returns a big map from the
      pairs of key/value in the list `bindings`. Note: Use
      `Big_map.literal` instead if using a literal list.

</Syntax>


<SyntaxTitle syntax="cameligo">
val mem : &#39;key &#39;value.&#39;key -&gt; (&#39;key, &#39;value) t -&gt; bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let mem: &lt;key, value&gt;(&#95;: key) =&gt; (&#95;: t&lt;key, value&gt;) =&gt; bool
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `Big_map.mem key map` is `true` if, and only if, the
      key `key` is in the big map `map`.

</Syntax>

<Syntax syntax="jsligo">

The call `Big_map.mem(key, map)` is `true` if, and only if, the
      key `key` is in the big map `map`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val find&#95;opt : &#39;key &#39;value.&#39;key -&gt; (&#39;key, &#39;value) t -&gt; &#39;value option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let find&#95;opt: &lt;key, value&gt;(&#95;: key) =&gt; (&#95;: t&lt;key, value&gt;) =&gt; option&lt;value&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `Big_map.find_opt key map` returns `None` if the key
      `key` is present in the big map `map`; otherwise, it is `Some v`,
      where `v` is the value associated to `key` in `map`.

</Syntax>

<Syntax syntax="jsligo">

The call `Big_map.find_opt(key, map)` returns `None()` if the
      key `key` is present in the big map `map`; otherwise, it is
      `Some(v)`, where `v` is the value associated to `key` in `map`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val find : &#39;key &#39;value.&#39;key -&gt; (&#39;key, &#39;value) t -&gt; &#39;value
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let find: &lt;key, value&gt;(&#95;: key) =&gt; (&#95;: t&lt;key, value&gt;) =&gt; value
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `Big_map.find key map` returns the value associated to
      `key` in the big map `map`. If the key is absent, the execution
      fails with the string `"MAP FIND"`.

</Syntax>

<Syntax syntax="jsligo">

The call `Big_map.find(key, map)` returns the value associated
      to `key` in the big map `map`. If the key is absent, the
      execution fails with the string `"MAP FIND"`.

</Syntax>
