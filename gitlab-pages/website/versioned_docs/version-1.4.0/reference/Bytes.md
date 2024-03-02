---
id: bytes-reference
title: Bytes
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';


Sequences of bytes

    Bytes are used for serializing data, in order to check signatures
    and compute hashes on them. They can also be used to read untyped
    data from outside of the contract.


<SyntaxTitle syntax="cameligo">
val length : bytes -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let length: (&#95;: bytes) =&gt; nat
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `length b` is the number of bytes in the sequence of
      bytes `b`. Note: `Bytes.length` is another name for
      `Bytes.size`.

</Syntax>

<Syntax syntax="jsligo">

The call `length(b)` is the number of bytes in the sequence of
      bytes `b`. Note: `Bytes.length` is another name for
      `Bytes.size`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val size : bytes -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let size: (&#95;: bytes) =&gt; nat
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `size b` is the number of bytes in the sequence of
    bytes `b`.

</Syntax>

<Syntax syntax="jsligo">

The call `size(b)` is the number of bytes in the sequence of
    bytes `b`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val concat : bytes -&gt; bytes -&gt; bytes
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let concat: (&#95;: bytes) =&gt; (&#95;: bytes) =&gt; bytes
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `concat left right` is the sequence of bytes obtained
    by concatenating the sequence `left` before the sequence
    `right`.

</Syntax>

<Syntax syntax="jsligo">

The call `concat(left, right)` is the sequence of bytes obtained
    by concatenating the sequence `left` before the sequence
    `right`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val concats : bytes list -&gt; bytes
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let concats: (&#95;: list&lt;bytes&gt;) =&gt; bytes
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `concats list` is the concatenation of the byte
    sequences in the list `list`, from left to right.

</Syntax>

<Syntax syntax="jsligo">

The call `concats(list)` is the concatenation of the byte
    sequences in the list `list`, from left to right.

</Syntax>


<SyntaxTitle syntax="cameligo">
val sub : nat -&gt; nat -&gt; bytes -&gt; bytes
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let sub: (&#95;: nat) =&gt; (&#95;: nat) =&gt; (&#95;: bytes) =&gt; bytes
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `sub index len bytes` is the subsequence of bytes
    `bytes` starting at index `index` (0 denoting the first byte) and
    of length `len`. If the index or length are invalid, an exception
    interrupts the execution.

</Syntax>

<Syntax syntax="jsligo">

The call `sub(index, len, bytes)` is the subsequence of bytes
    `bytes` starting at index `index` (0 denoting the first byte) and
    of length `len`. If the index or length are invalid, an exception
    interrupts the execution.

</Syntax>


<SyntaxTitle syntax="cameligo">
val slice : nat -&gt; nat -&gt; bytes -&gt; bytes
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let slice: (&#95;: nat) =&gt; (&#95;: nat) =&gt; (&#95;: bytes) =&gt; bytes
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `slice index len bytes` is the subsequence of bytes
    `bytes` starting at index `index` (0 denoting the first byte) and
    of length `len`. If the index or length are invalid, an exception
    interrupts the execution.

</Syntax>

<Syntax syntax="jsligo">

The call `slice(index, len, bytes)` is the subsequence of bytes
    `bytes` starting at index `index` (0 denoting the first byte) and
    of length `len`. If the index or length are invalid, an exception
    interrupts the execution.

</Syntax>


<SyntaxTitle syntax="cameligo">
val pack : &#39;a.&#39;a -&gt; bytes
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let pack: &lt;a&gt;(&#95;: a) =&gt; bytes
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `pack v` transforms the value `v` into a sequence of
    bytes.

</Syntax>

<Syntax syntax="jsligo">

The call `pack(v)` transforms the value `v` into a sequence of
    bytes.

</Syntax>


<SyntaxTitle syntax="cameligo">
val unpack : &#39;a.bytes -&gt; &#39;a option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let unpack: &lt;a&gt;(&#95;: bytes) =&gt; option&lt;a&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `unpack bytes` is `Some v` if the sequence of bytes
    `bytes` decodes into a valid LIGO value `v`; otherwise `None`.

</Syntax>

<Syntax syntax="jsligo">

The call `unpack(bytes)` is `Some(v)` if the sequence of bytes
    `bytes` decodes into a valid LIGO value `v`; otherwise
    `None()`.

</Syntax>
