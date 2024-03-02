---
id: bitwise-reference
title: Bitwise
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';


Bitwise operations


<SyntaxTitle syntax="cameligo">
val and : &#39;a &#39;b.&#39;a -&gt; &#39;b -&gt; (&#39;a, &#39;b) external&#95;and
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let and: &lt;a, b&gt;(&#95;: a) =&gt; (&#95;: b) =&gt; external&#95;and&lt;a, b&gt;
</SyntaxTitle>
The call `@and a b` is the conjunction defined on boolean,
    natural number and bytes operands. In the boolean case, the result
    is the logical "and" of the operands. In the natural number and
    bytes cases, the result is the bitwise "and" of the operands.

    The function `@and` is also defined when the left operand is of
    type `int`. Negative numbers are considered in two's complement
    representation, starting with a virtual infinite number of 1s.

    When `@and` is used for bytes operands, the bytes result has the
    same length as the shorter operand. The prefix of the longer
    operand is cut to match with the length of the shorter one before
    taking the bitwise "and".


<SyntaxTitle syntax="cameligo">
val or : &#39;a &#39;b.&#39;a -&gt; &#39;b -&gt; (&#39;a, &#39;b) external&#95;xor
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let or: &lt;a, b&gt;(&#95;: a) =&gt; (&#95;: b) =&gt; external&#95;xor&lt;a, b&gt;
</SyntaxTitle>
The call `@or a b` is the disjunction defined on boolean,
    natural number and bytes operands. In the boolean case, the result
    is the logical "or" of the operands. In the natural number and
    bytes cases, the result is the bitwise "or" of the operands.

    When the function `@or` is used for bytes operands, the result
    bytes has the same length as the longer operand. The shorter
    operand is zero-padded on the left to match with the length of the
    longer one before taking the bitwise "or".


<SyntaxTitle syntax="cameligo">
val xor : &#39;a &#39;b.&#39;a -&gt; &#39;b -&gt; (&#39;a, &#39;b) external&#95;or
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let xor: &lt;a, b&gt;(&#95;: a) =&gt; (&#95;: b) =&gt; external&#95;or&lt;a, b&gt;
</SyntaxTitle>
The call `xor a b` is the exclusive disjunction defined on
    boolean, natural number and bytes operands. In the boolean case,
    the result is the logical "exclusive or" of the operands. In the
    natural number and bytes cases, the result is the bitwise "xor" of
    the operands.

    When `xor` is used for bytes operands, the result bytes has the
    same length as the longer operand. The shorter operand is
    zero-padded on the left to match with the length of the longer one
    before taking the bitwise "xor".


<SyntaxTitle syntax="cameligo">
val shift&#95;left : &#39;a &#39;b.&#39;a -&gt; &#39;b -&gt; (&#39;a, &#39;b) external&#95;lsl
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let shift&#95;left: &lt;a, b&gt;(&#95;: a) =&gt; (&#95;: b) =&gt; external&#95;lsl&lt;a, b&gt;
</SyntaxTitle>
The function `shift_left` on natural numbers consumes two
    natural numbers and produces the first number logically
    left-shifted by the second number. This instruction is only
    defined if the second number is less than or equal to 256.

    For bytes, the function `shift_left` consumes one byte sequence
    and one natural number, and produces the bytes logically
    left-shifted by the natural number. The vacated bits on the right
    are filled with zeros. The shifted bits are minimally zero-padded
    on the left in order to keep all the original bits, regardless if
    they are 0 or 1: for example, `shift_left 0x1234 1` is `0x002468`,
    instead of `0x2468` (even though in this case no significant bit
    would be lost) or `0x00002468` (where padding is not minimal). The
    length of the bytes returned by `shift_left` is `l + (s + 7) / 8`
    bytes where `l` is the length of the original bytes and `s` is the
    natural number. This instruction is only defined if the second
    number is less than or equal to 64000.


<SyntaxTitle syntax="cameligo">
val shift&#95;right : &#39;a &#39;b.&#39;a -&gt; &#39;b -&gt; (&#39;a, &#39;b) external&#95;lsr
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let shift&#95;right: &lt;a, b&gt;(&#95;: a) =&gt; (&#95;: b) =&gt; external&#95;lsr&lt;a, b&gt;
</SyntaxTitle>
The function `shift_right` on natural numbers consumes two
    natural numbers and produces the first number logically
    right-shifted by second number. This function is only defined if
    the second number is less than or equal to 256.

    For bytes, the function `shift_right` consumes one chunk of bytes
    and one natural number and produces the bytes logically
    right-shifted by the natural number. The shifted bits are
    minimally zero-padded on the left. For example, `shift_right
    0x012349 9` is `0x0091`, instead of `0x91` (where the 7 left-most
    bits are lost) or `0x000091` (not minimal padding). The length of
    the returned bytes by `shift_right` is `max 0 (l - s / 8)` bytes,
    where `l` is the length of the original bytes, and `s` is the
    natural number.
