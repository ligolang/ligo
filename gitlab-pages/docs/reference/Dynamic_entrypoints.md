---
id: dynamic-entrypoints-reference
title: Dynamic_entrypoints
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';



<SyntaxTitle syntax="cameligo">
type t = (nat, bytes) big&#95;map
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
type t = big&#95;map&lt;nat, bytes&gt;
</SyntaxTitle>
Type `t` is an alias of the predefined type
      `dynamic_entrypoints`.


<SyntaxTitle syntax="cameligo">
val set :
  &#39;param
  &#39;storage.(&#39;param, &#39;storage) dynamic&#95;entrypoint -&gt;
  (&#39;param, &#39;storage) entrypoint option -&gt; dynamic&#95;entrypoints -&gt; dynamic&#95;entrypoints
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let set:
  &lt;param, storage&gt;(&#95;: dynamic&#95;entrypoint&lt;param, storage&gt;) =&gt; (&#95;: option&lt;entrypoint&lt;param, storage&gt;&gt;) =&gt; (
    &#95;: dynamic&#95;entrypoints
  ) =&gt; dynamic&#95;entrypoints
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `set dyn None dyn_map` returns a copy of the map of
      dynamic entrypoints `dyn_map` where the dynamic entrypoint `dyn`
      is not associated to a static entrypoint. The call `set dyn
      (Some entrypoint) dyn_map` is a copy of `dyn_map` where the
      dynamic entrypoint `dyn` is associated to the static entrypoint
      `entrypoint`.

</Syntax>

<Syntax syntax="jsligo">

The call `set(dyn, None(), dyn_map)` returns a copy of the map of
      dynamic entrypoints `dyn_map` where the dynamic entrypoint `dyn`
      is not associated to a static entrypoint. The call `set(dyn,
      Some(entrypoint), dyn_map)` is a copy of `dyn_map` where the
      dynamic entrypoint `dyn` is associated to the static entrypoint
      `entrypoint`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val set&#95;bytes :
  &#39;param
  &#39;storage.(&#39;param, &#39;storage) dynamic&#95;entrypoint -&gt; bytes option -&gt; dynamic&#95;entrypoints -&gt; dynamic&#95;entrypoints
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let set&#95;bytes:
  &lt;param, storage&gt;(&#95;: dynamic&#95;entrypoint&lt;param, storage&gt;) =&gt; (&#95;: option&lt;bytes&gt;) =&gt; (&#95;: dynamic&#95;entrypoints) =&gt; dynamic&#95;entrypoints
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `set_bytes dyn None dyn_map` returns a copy of the map
      of dynamic entrypoints `dyn_map` where the dynamic entrypoint
      `dyn` is not associated to a static entrypoint. The call
      `set_bytes dyn (Some bytes) dyn_map` is a copy of `dyn_map`
      where the dynamic entrypoint `dyn` is associated to the static
      entrypoint encoded by the sequence of bytes `bytes`. If that
      sequence is invalid, any call to the dynamic entrypoint will
      fail.

</Syntax>

<Syntax syntax="jsligo">

The call `set_bytes(dyn, None(), dyn_map)` returns a copy of the
      map of dynamic entrypoints `dyn_map` where the dynamic
      entrypoint `dyn` is not associated to a static entrypoint. The
      call `set_bytes(dyn, Some(bytes), dyn_map)` is a copy of `dyn_map`
      where the dynamic entrypoint `dyn` is associated to the static
      entrypoint encoded by the sequence of bytes `bytes`. If that
      sequence is invalid, any call to the dynamic entrypoint will
      fail.

</Syntax>


<SyntaxTitle syntax="cameligo">
val get :
  &#39;param
  &#39;storage.(&#39;param, &#39;storage) dynamic&#95;entrypoint -&gt; dynamic&#95;entrypoints -&gt; (&#39;param, &#39;storage) entrypoint option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get:
  &lt;param, storage&gt;(&#95;: dynamic&#95;entrypoint&lt;param, storage&gt;) =&gt; (&#95;: dynamic&#95;entrypoints) =&gt; option&lt;
    entrypoint&lt;param, storage&gt;
  &gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `get dyn dyn_map` is `None` if the dynamic entrypoint
      `dyn` is absent from the dynamic entrypoints map
      `dyn_map`. Otherwise, it is `Some entry`, where `entry` is a
      static entrypoint that is callable (like a function). See type
      `entrypoint`.

</Syntax>

<Syntax syntax="jsligo">

The call `get(dyn, dyn_map)` is `None()` if the dynamic
      entrypoint `dyn` is absent from the dynamic entrypoints map
      `dyn_map`. Otherwise, it is `Some(entry)`, where `entry` is a
      static entrypoint that is callable (like a function). See type
      `entrypoint`.

</Syntax>
