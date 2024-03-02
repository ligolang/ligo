---
id: test.next.michelson-reference
title: Michelson
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';



[module Contract](Test.Next.Michelson.Contract.md)


<SyntaxTitle syntax="cameligo">
val run : &#39;a &#39;b.(&#39;a -&gt; &#39;b) -&gt; &#39;a -&gt; michelson&#95;program
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let run: &lt;a, b&gt;(&#95;: (&#95;: a) =&gt; b) =&gt; (&#95;: a) =&gt; michelson&#95;program
</SyntaxTitle>
Run a function on an input, all in Michelson. More concretely:
        a) compiles the function argument to Michelson `f_mich`; b)
        compiles the value argument (which was evaluated already) to
        Michelson `v_mich`; c) runs the Michelson interpreter on the code
        `f_mich` with starting stack `[v_mich]`.


<SyntaxTitle syntax="cameligo">
val eval : &#39;a.&#39;a -&gt; michelson&#95;program
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let eval: &lt;a&gt;(&#95;: a) =&gt; michelson&#95;program
</SyntaxTitle>
Compile a LIGO value to Michelson. Currently it is a
        renaming of `compile_value`.


<SyntaxTitle syntax="cameligo">
val decompile : &#39;a.michelson&#95;program -&gt; &#39;a
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let decompile: &lt;a&gt;(&#95;: michelson&#95;program) =&gt; a
</SyntaxTitle>
Decompile a Michelson value to LIGO, following the
        (mandatory) type annotation. Note: This operation can fail at
        run-time, in case that the `michelson_program` given cannot be
        decompiled to something compatible with the annotated type.


<SyntaxTitle syntax="cameligo">
val parse : string -&gt; michelson&#95;program
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let parse: (&#95;: string) =&gt; michelson&#95;program
</SyntaxTitle>
Parses Michelson (as string) into a `michelson_program`.
