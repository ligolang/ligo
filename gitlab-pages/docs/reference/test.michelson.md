---
id: test.michelson-reference
title: michelson
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

[module contract](test.michelson.contract.md)

<SyntaxTitle syntax="cameligo">
val run : &#39;a &#39;b.(&#39;a -&gt; &#39;b) -&gt; &#39;a -&gt; michelson_program
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
run: &lt;a, b&gt;(_: (_: a) =&gt; b, _: a) =&gt; michelson_program
</SyntaxTitle>

Run a function on an input, all in Michelson. More concretely: a)
compiles the function argument to Michelson `f_mich`; b) compiles the
value argument (which was evaluated already) to Michelson `v_mich`; c)
runs the Michelson interpreter on the code `f_mich` with starting
stack `[v_mich]`.


<SyntaxTitle syntax="cameligo">
val eval : &#39;a.&#39;a -&gt; michelson_program
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
eval: &lt;a&gt;(_: a) =&gt; michelson_program
</SyntaxTitle>

Compile a LIGO value to Michelson.


<SyntaxTitle syntax="cameligo">
val decompile : &#39;a.michelson_program -&gt; &#39;a
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
decompile: &lt;a&gt;(_: michelson_program) =&gt; a
</SyntaxTitle>

Decompile a Michelson value to LIGO, following the (mandatory) type
annotation. Note: This operation can fail at run-time, in case that
the `michelson_program` given cannot be decompiled to something
compatible with the annotated type.


<SyntaxTitle syntax="cameligo">
val parse : string -&gt; michelson_program
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
parse: (code: string) =&gt; michelson_program
</SyntaxTitle>

Parses Michelson (as string) into a `michelson_program`.
