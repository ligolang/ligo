---
id: string-reference
title: String
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';


Strings of characters


<SyntaxTitle syntax="cameligo">
val length : string -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let length: (&#95;: string) =&gt; nat
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `length s` is the number of characters in the string
      `s`. Note: `String.length` is another name for `String.size`.

</Syntax>

<Syntax syntax="jsligo">

The call `length(s)` is the number of characters in the string
      `s`. Note: `String.length` is another name for `String.size`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val size : string -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let size: (&#95;: string) =&gt; nat
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `size s` is the number of characters in the string `s`.

</Syntax>

<Syntax syntax="jsligo">

The call `size(s)` is the number of characters in the string `s`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val concat : string -&gt; string -&gt; string
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let concat: (&#95;: string) =&gt; (&#95;: string) =&gt; string
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `concat left right` is the concatenation of the string
    `left` and the string `right`, in that order.

</Syntax>

<Syntax syntax="jsligo">

The call `concat(left, right)` is the concatenation of the string
    `left` and the string `right`, in that order.

</Syntax>


<SyntaxTitle syntax="cameligo">
val concats : string list -&gt; string
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let concats: (&#95;: list&lt;string&gt;) =&gt; string
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `concats list` is the concatenation of the strings in
    the list `list`, from left to right.

</Syntax>

<Syntax syntax="jsligo">

The call `concats(list)` is the concatenation of the strings in
    the list `list`, from left to right.

</Syntax>


<SyntaxTitle syntax="cameligo">
val sub : nat -&gt; nat -&gt; string -&gt; string
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let sub: (&#95;: nat) =&gt; (&#95;: nat) =&gt; (&#95;: string) =&gt; string
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `sub index len str` is the substring of string `str`
    starting at index `index` (0 denoting the first character) and of
    length `len`. If the index or length are invalid, an exception
    interrupts the execution.

</Syntax>

<Syntax syntax="jsligo">

The call `sub(index, len, str)` is the substring of string `str`
    starting at index `index` (0 denoting the first character) and of
    length `len`. If the index or length are invalid, an exception
    interrupts the execution.

</Syntax>


<SyntaxTitle syntax="cameligo">
val slice : nat -&gt; nat -&gt; string -&gt; string
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let slice: (&#95;: nat) =&gt; (&#95;: nat) =&gt; (&#95;: string) =&gt; string
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `slice index len str` is the substring of string `str`
    starting at index `index` (0 denoting the first character) and of
    length `len`. If the index or length are invalid, an exception
    interrupts the execution.

</Syntax>

<Syntax syntax="jsligo">

The call `slice(index, len, str)` is the substring of string `str`
    starting at index `index` (0 denoting the first character) and of
    length `len`. If the index or length are invalid, an exception
    interrupts the execution.

</Syntax>
